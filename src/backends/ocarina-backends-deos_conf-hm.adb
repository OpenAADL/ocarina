------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . B A C K E N D S . D E O S _ C O N F . H M         --
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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

--  with Locations;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

--  with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
--  with Ocarina.Backends.Deos_Conf.Mapping;

package body Ocarina.Backends.Deos_Conf.Hm is

--   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
--   use Ocarina.Backends.Properties;
--   use Ocarina.Backends.Deos_Conf.Mapping;

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
                               Level      : String;
                               Action     : String);
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
                               Level      : String;
                               Action     : String) is
      Intermediate : Node_Id;
   begin
      Intermediate := Make_XML_Node ("ErrorAction");
      XTU.Add_Attribute ("ErrorIdentifierRef", Identifier, Intermediate);
      XTU.Add_Attribute ("ErrorLevel", Level, Intermediate);
      XTU.Add_Attribute ("ModuleRecoveryAction", Action, Intermediate);
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
      System_Errors        : Node_Id;
      Multi_Partition_HM   : Node_Id;
      Partition_HM         : Node_Id;
      Partition_Identifier : Unsigned_Long_Long;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      P := XTN.Node (Backend_Node (Identifier (E)));

      Push_Entity (P);
      Push_Entity (U);

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Partition_Identifier := 1;

      --
      --  For now, just generate the default HM policy.
      --

      HM_Node := Make_XML_Node ("HealthMonitoring");

      Append_Node_To_List
        (HM_Node,
         XTN.Subitems (Current_XML_Node));

      System_Errors := Make_XML_Node ("SystemErrors");

      Append_Node_To_List
        (System_Errors,
         XTN.Subitems (HM_Node));
      Add_System_Error (System_Errors, "1", "processorSpecific");
      Add_System_Error (System_Errors, "2", "floatingPoint");
      Add_System_Error (System_Errors, "3", "accessViolation");
      Add_System_Error (System_Errors, "4", "powerTransient");
      Add_System_Error (System_Errors, "5", "platformSpecific");
      Add_System_Error (System_Errors, "6", "frameResync");
      Add_System_Error (System_Errors, "7", "deadlineMissed");
      Add_System_Error (System_Errors, "8", "applicationError");
      Add_System_Error (System_Errors, "9", "illegalRequest");
      Add_System_Error (System_Errors, "10", "stackOverflow");

      --
      --  The MultiPartitionHM
      --

      Multi_Partition_HM := Make_XML_Node ("MultiPartitionHM");
      XTU.Add_Attribute ("TableIdentifier", "1", Multi_Partition_HM);
      XTU.Add_Attribute ("TableName",
                         "default MultiPartitionHM",
                         Multi_Partition_HM);
      Append_Node_To_List
        (Multi_Partition_HM,
         XTN.Subitems (HM_Node));
      Add_Error_Action (Multi_Partition_HM, "1", "MODULE", "IGNORE");
      Add_Error_Action (Multi_Partition_HM, "2", "MODULE", "IGNORE");
      Add_Error_Action (Multi_Partition_HM, "3", "MODULE", "IGNORE");
      Add_Error_Action (Multi_Partition_HM, "4", "MODULE", "IGNORE");
      Add_Error_Action (Multi_Partition_HM, "5", "MODULE", "IGNORE");
      Add_Error_Action (Multi_Partition_HM, "6", "MODULE", "IGNORE");
      Add_Error_Action (Multi_Partition_HM, "7", "MODULE", "IGNORE");
      Add_Error_Action (Multi_Partition_HM, "8", "MODULE", "IGNORE");
      Add_Error_Action (Multi_Partition_HM, "9", "MODULE", "IGNORE");
      Add_Error_Action (Multi_Partition_HM, "10", "MODULE", "IGNORE");

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            if AINU.Is_Virtual_Processor (Corresponding_Instance (S)) then
               Visit (Corresponding_Instance (S));
               --
               --  The PartitionHM
               --

               Partition_HM := Make_XML_Node ("PartitionHM");
               Append_Node_To_List
                 (Partition_HM,
                  XTN.Subitems (HM_Node));

               XTU.Add_Attribute ("TableIdentifier",
                                 Trim (Unsigned_Long_Long'Image
                                    (Partition_Identifier), Left),
                                  Partition_HM);

               XTU.Add_Attribute ("TableName",
                                  "Unique name for partition " &
                                 Trim (Unsigned_Long_Long'Image
                                    (Partition_Identifier), Left),
                                  Partition_HM);
               XTU.Add_Attribute ("MultiPartitionHMTableNameRef",
                                  "default MultiPartitionHM",
                                  Partition_HM);
               Partition_Identifier := Partition_Identifier + 1;
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

end Ocarina.Backends.Deos_Conf.Hm;
