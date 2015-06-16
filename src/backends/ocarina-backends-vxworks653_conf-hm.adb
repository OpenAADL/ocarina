------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . V X W O R K S 6 5 3 _ C O N F . H M   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2015 ESA & ISAE.                       --
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

with Ocarina.Namet; use Ocarina.Namet;

--  with Locations;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

--  with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Vxworks653_Conf.Mapping;

package body Ocarina.Backends.Vxworks653_Conf.Hm is

--   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
--   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Vxworks653_Conf.Mapping;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   Root_Node : Node_Id := No_Node;
   HM_Node : Node_Id := No_Node;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Bus_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);

   procedure Add_System_Error (XML_Node : Node_Id;
                               Identifier : String;
                               Description : String);

   procedure Add_Error_Action (XML_Node : Node_Id;
                               Identifier : String;
                               Action     : String);

   pragma Unreferenced (Add_System_Error);

   function Generate_Partition_HM_Table
      (Virtual_Processor : Node_Id) return Node_Id;

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

   ------------------------
   --  Add_System_Error  --
   ------------------------

   procedure Add_System_Error (XML_Node : Node_Id;
                               Identifier : String;
                               Description : String) is
      Intermediate : Node_Id;
   begin
      Intermediate := Make_XML_Node ("SystemError");
      XTU.Add_Attribute ("ErrorIdentifier", Identifier, Intermediate);
      XTU.Add_Attribute ("Description", Description, Intermediate);
      Append_Node_To_List
         (Intermediate, XTN.Subitems (XML_Node));
   end Add_System_Error;

   ------------------------
   --  Add_Error_Action  --
   ------------------------

   procedure Add_Error_Action (XML_Node : Node_Id;
                               Identifier : String;
                               Action     : String) is
      Intermediate : Node_Id;
   begin
      Intermediate := Make_XML_Node ("ErrorIDAction");
      XTU.Add_Attribute ("ErrorIdentifier", Identifier, Intermediate);
      XTU.Add_Attribute ("ErrorAction", Action, Intermediate);
      Append_Node_To_List
         (Intermediate, XTN.Subitems (XML_Node));
   end Add_Error_Action;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      S                    : Node_Id;
      U                    : Node_Id;
      P                    : Node_Id;
      System_HM_Table_Node : Node_Id;
      Module_HM_Table_Node : Node_Id;
      System_State_Node    : Node_Id;
      Error_Id_Level_Node  : Node_Id;
      Settings_Node        : Node_Id;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      P := XTN.Node (Backend_Node (Identifier (E)));

      Push_Entity (P);
      Push_Entity (U);

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      --
      --  For now, just generate the default HM policy.
      --

      HM_Node := Make_XML_Node ("HealthMonitor");
      Append_Node_To_List (HM_Node, XTN.Subitems (Current_XML_Node));

      --  Building the SystemHMTable first.

      System_HM_Table_Node := Make_XML_Node ("SystemHMTable");
      XTU.Add_Attribute ("Name", "systemHm", System_HM_Table_Node);
      Append_Node_To_List (System_HM_Table_Node, XTN.Subitems (HM_Node));

      System_State_Node := Make_XML_Node ("SystemState");
      XTU.Add_Attribute ("SystemState",
                         "HM_PARTITION_MODE",
                         System_State_Node);
      Append_Node_To_List (System_State_Node,
                           XTN.Subitems (System_HM_Table_Node));

      Error_Id_Level_Node := Make_XML_Node ("ErrorIDLevel");
      XTU.Add_Attribute ("ErrorIdentifier",
                         "HME_DEFAULT",
                         Error_Id_Level_Node);
      XTU.Add_Attribute ("ErrorLevel",
                         "HM_PARTITION_LVL",
                         Error_Id_Level_Node);

      Append_Node_To_List (Error_Id_Level_Node,
                           XTN.Subitems (System_State_Node));

      System_State_Node := Make_XML_Node ("SystemState");
      XTU.Add_Attribute ("SystemState",
                         "HM_MODULE_MODE",
                         System_State_Node);
      Append_Node_To_List (System_State_Node,
                           XTN.Subitems (System_HM_Table_Node));

      Error_Id_Level_Node := Make_XML_Node ("ErrorIDLevel");
      XTU.Add_Attribute ("ErrorIdentifier",
                         "HME_DEFAULT",
                         Error_Id_Level_Node);
      XTU.Add_Attribute ("ErrorLevel",
                         "HM_MODULE_LVL",
                         Error_Id_Level_Node);
      Append_Node_To_List (Error_Id_Level_Node,
                           XTN.Subitems (System_State_Node));

      System_State_Node := Make_XML_Node ("SystemState");
      XTU.Add_Attribute ("SystemState",
                         "HM_PROCESS_MODE",
                         System_State_Node);
      Append_Node_To_List (System_State_Node,
                           XTN.Subitems (System_HM_Table_Node));

      Error_Id_Level_Node := Make_XML_Node ("ErrorIDLevel");
      XTU.Add_Attribute ("ErrorIdentifier",
                         "HME_DEFAULT", Error_Id_Level_Node);
      XTU.Add_Attribute ("ErrorLevel",
                         "HM_PROCESS_LVL", Error_Id_Level_Node);
      Append_Node_To_List (Error_Id_Level_Node,
                           XTN.Subitems (System_State_Node));

      --  Building the ModuleHMTable then.

      Module_HM_Table_Node := Make_XML_Node ("ModuleHMTable");
      XTU.Add_Attribute ("Name", "moduleHm", Module_HM_Table_Node);
      Append_Node_To_List (Module_HM_Table_Node, XTN.Subitems (HM_Node));

      System_State_Node := Make_XML_Node ("SystemState");
      Append_Node_To_List (System_State_Node,
                           XTN.Subitems (Module_HM_Table_Node));
      Add_Error_Action (System_State_Node,
                        "HME_UNKNOWN", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_NUMERIC_ERROR", "");
      Add_Error_Action (System_State_Node,
                        "HME_POWER_FAIL", "hmDH_HME_POWER_FAIL");
      Add_Error_Action (System_State_Node,
                        "HME_KERNEL", "hmDH_HME_KERNEL");
      Add_Error_Action (System_State_Node,
                        "HME_CONFIG_ERROR", "hmDH_EventLog");
      Add_Error_Action (System_State_Node,
                        "HME_INIT_ERROR", "hmDH_HME_INIT_ERROR");
      Add_Error_Action (System_State_Node,
                        "HME_PARTITION_OVERFLOW", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_PARTITION_MODE_SET",
                        "hmDH_HME_PARTITION_MODE_SET");
      Add_Error_Action (System_State_Node,
                        "HME_APEX_INTERNAL_ERROR", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_HM_INTERNAL_ERROR", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_PORT_INTERNAL_ERROR", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_LOST_TICKS", "hmDH_HME_LOST_TICKS");
      Add_Error_Action (System_State_Node,
                        "HME_HM_ERROR", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_HMQ_OVERFLOW", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_DATA_LOSS", "");
      Add_Error_Action (System_State_Node,
                        "HME_HM_DEADLINE_MISSED", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HM_MSG", "hmDH_EventLog");
      Add_Error_Action (System_State_Node,
                        "HME_DEFAULT", "hmDefaultHandler");

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            if AINU.Is_Virtual_Processor (Corresponding_Instance (S)) then
               Append_Node_To_List
                  (Generate_Partition_HM_Table (Corresponding_Instance (S)),
                  XTN.Subitems (HM_Node));
            end if;
            S := Next_Node (S);
         end loop;
      end if;

      Settings_Node := Make_XML_Node ("Settings");
      XTU.Add_Attribute ("maxQueueDepth", "34", Settings_Node);
      XTU.Add_Attribute ("queueThreshold", "32", Settings_Node);
      XTU.Add_Attribute ("stackSize", "16384", Settings_Node);
      XTU.Add_Attribute ("maxLogEntries", "100", Settings_Node);
      XTU.Add_Attribute ("logEntriesThreshold", "98", Settings_Node);
      XTU.Add_Attribute ("attributeMask", "0x00000001", Settings_Node);
      XTU.Add_Attribute ("notificationHandler", "", Settings_Node);
      XTU.Add_Attribute ("notifMaxQueueDepth", "0", Settings_Node);
      XTU.Add_Attribute ("eventFilterMask", "0x00000000", Settings_Node);
      XTU.Add_Attribute ("maxErrorHandlerQueueDepth", "0", Settings_Node);
      XTU.Add_Attribute ("errorHandlerQueueThreshold", "0", Settings_Node);
      Append_Node_To_List (Settings_Node, XTN.Subitems (Module_HM_Table_Node));

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

   ---------------------------------
   -- Generate_Partition_HM_Table --
   ---------------------------------

   function Generate_Partition_HM_Table
      (Virtual_Processor : Node_Id)
      return Node_Id is
      Partition_HM_Table_Node : Node_Id;
      System_State_Node : Node_Id;
      Settings_Node : Node_Id;
      Trusted_Partition_Node : Node_Id;
   begin
      Partition_HM_Table_Node := Make_XML_Node ("PartitionHMTable");
      XTU.Add_Attribute ("Name",
                         Get_Name_String
                           (Map_Partition_Name (Virtual_Processor))
                           & "_hmtable",
                         Partition_HM_Table_Node);

      System_State_Node := Make_XML_Node ("SystemState");
      Append_Node_To_List (System_State_Node,
                           XTN.Subitems (Partition_HM_Table_Node));
      Add_Error_Action (System_State_Node,
                        "HME_UNKNOWN", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_NUMERIC_ERROR", "");
      Add_Error_Action (System_State_Node,
                        "HME_POWER_FAIL", "hmDH_HME_POWER_FAIL");
      Add_Error_Action (System_State_Node,
                        "HME_KERNEL", "hmDH_HME_KERNEL");
      Add_Error_Action (System_State_Node,
                        "HME_CONFIG_ERROR", "hmDH_EventLog");
      Add_Error_Action (System_State_Node,
                        "HME_INIT_ERROR", "hmDH_HME_INIT_ERROR");
      Add_Error_Action (System_State_Node,
                        "HME_PARTITION_OVERFLOW", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_PARTITION_MODE_SET",
                        "hmDH_HME_PARTITION_MODE_SET");
      Add_Error_Action (System_State_Node,
                        "HME_APEX_INTERNAL_ERROR", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_HM_INTERNAL_ERROR", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_PORT_INTERNAL_ERROR", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_LOST_TICKS", "hmDH_HME_LOST_TICKS");
      Add_Error_Action (System_State_Node,
                        "HME_HM_ERROR", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_HMQ_OVERFLOW", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HME_DATA_LOSS", "");
      Add_Error_Action (System_State_Node,
                        "HME_HM_DEADLINE_MISSED", "hmDefaultHandler");
      Add_Error_Action (System_State_Node,
                        "HM_MSG", "hmDH_EventLog");
      Add_Error_Action (System_State_Node,
                        "HME_DEFAULT", "hmDefaultHandler");

      Settings_Node := Make_XML_Node ("Settings");
      XTU.Add_Attribute ("maxQueueDepth", "34", Settings_Node);
      XTU.Add_Attribute ("queueThreshold", "32", Settings_Node);
      XTU.Add_Attribute ("stackSize", "16384", Settings_Node);
      XTU.Add_Attribute ("maxLogEntries", "100", Settings_Node);
      XTU.Add_Attribute ("logEntriesThreshold", "98", Settings_Node);
      XTU.Add_Attribute ("attributeMask", "0x00000001", Settings_Node);
      XTU.Add_Attribute ("notificationHandler", "", Settings_Node);
      XTU.Add_Attribute ("notifMaxQueueDepth", "0", Settings_Node);
      XTU.Add_Attribute ("eventFilterMask", "0xFFFFFFFF", Settings_Node);
      XTU.Add_Attribute ("maxErrorHandlerQueueDepth", "128", Settings_Node);
      XTU.Add_Attribute ("errorHandlerQueueThreshold", "126", Settings_Node);
      Append_Node_To_List (Settings_Node,
                           XTN.Subitems (Partition_HM_Table_Node));

      Trusted_Partition_Node := Make_XML_Node ("TrustedPartition");
      XTU.Add_Attribute ("NameRef",
                         Get_Name_String
                           (Map_Partition_Name
                              (Virtual_Processor)),
                         Trusted_Partition_Node);
      Append_Node_To_List (Trusted_Partition_Node,
                           XTN.Subitems (Settings_Node));

      return Partition_HM_Table_Node;
   end Generate_Partition_HM_Table;

end Ocarina.Backends.Vxworks653_Conf.Hm;
