------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . B A C K E N D S . C H E D D A R . M A I N         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2010-2018 ESA & ISAE.                    --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Cheddar.Mapping;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Helper;

package body Ocarina.Backends.Cheddar.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Cheddar.Mapping;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Helper;

   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   procedure Visit_Component (E : Node_Id);
   procedure Visit_System (E : Node_Id);
   procedure Visit_Processor (E : Node_Id);
   procedure Visit_Thread (E : Node_Id);
   procedure Visit_Process (E : Node_Id);
   procedure Visit_Data (E : Node_Id);
   procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

   Root_System_Node  : Node_Id := No_Node;
   Cheddar_Node      : Node_Id := No_Node;
   Tasks_Node        : Node_Id := No_Node;
   Processors_Node   : Node_Id := No_Node;
   Address_Node      : Node_Id := No_Node;
   Buffers_Node      : Node_Id := No_Node;
   Resources_Node    : Node_Id := No_Node;
   Dependencies_Node : Node_Id := No_Node;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Root_System_Node := Root_System (E);
            Visit (Root_System_Node);

         when K_Component_Instance =>
            Visit_Component (E);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------
   -- Visit_Component --
   ---------------------

   procedure Visit_Component (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System (E);

         when CC_Processor =>
            Visit_Processor (E);

         when CC_Process =>
            Visit_Process (E);

         when CC_Thread =>
            Visit_Thread (E);

         when CC_Data =>
            Visit_Data (E);

         when others =>
            null;
      end case;
   end Visit_Component;

   ------------------
   -- Visit_Thread --
   ------------------

   procedure Visit_Thread (E : Node_Id) is
   begin
      Append_Node_To_List (Map_Thread (E), XTN.Subitems (Tasks_Node));

      for F of Features_Of (E) loop
         if Kind (F) = K_Port_Spec_Instance and then Is_Event (F) then
            if Is_In (F) then
               Append_Node_To_List
                 (Map_Buffer (E, F),
                  XTN.Subitems (Buffers_Node));
            end if;
            Append_Node_To_List
              (Map_Dependency (E, F),
               XTN.Subitems (Dependencies_Node));
         end if;
      end loop;

      Visit_Subcomponents_Of (E);
   end Visit_Thread;

   ----------------
   -- Visit_Data --
   ----------------

   procedure Visit_Data (E : Node_Id) is
   begin
      Append_Node_To_List (Map_Data (E), XTN.Subitems (Resources_Node));
      Visit_Subcomponents_Of (E);
   end Visit_Data;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process (E : Node_Id) is
   begin
      Append_Node_To_List (Map_Process (E), XTN.Subitems (Address_Node));
      Visit_Subcomponents_Of (E);
   end Visit_Process;

   ---------------------
   -- Visit_Processor --
   ---------------------

   procedure Visit_Processor (E : Node_Id) is
   begin
      Append_Node_To_List (Map_Processor (E), XTN.Subitems (Processors_Node));
      Visit_Subcomponents_Of (E);
   end Visit_Processor;

   ------------------
   -- Visit_System --
   ------------------

   procedure Visit_System (E : Node_Id) is
      P : Node_Id;
      U : Node_Id;
   begin
      if E = Root_System_Node then
         P := Map_HI_Node (E);
         Push_Entity (P);

         U := Map_HI_Unit (E);
         Push_Entity (U);

         --  A cheddar XML file is made of one cheddar node, with several
         --  children: tasks (AADL tasks), processors (AADL processors),
         --  address_spaces (AADL processes), resources (AADL data
         --  components).

         --  <!ELEMENT cheddar (processors,
         --                     (address_spaces)?,
         --                     (tasks)?,
         --                     ((event_analyzers)?
         --                      |(networks)?
         --                      |(buffers)?
         --                      |(resources)?
         --                      |(messages)?),
         --                     (dependencies)?)
         --  >

         if Cheddar_Node = No_Node then
            Cheddar_Node := Make_XML_Node ("cheddar");
            Append_Node_To_List
              (Cheddar_Node,
               XTN.Subitems (XTN.Root_Node (XTN.XML_File (U))));
         end if;

         if Processors_Node = No_Node then
            Processors_Node := Make_XML_Node ("processors");
            Append_Node_To_List (Processors_Node, XTN.Subitems (Cheddar_Node));
         end if;

         if Address_Node = No_Node then
            Address_Node := Make_XML_Node ("address_spaces");
            Append_Node_To_List (Address_Node, XTN.Subitems (Cheddar_Node));
         end if;

         if Tasks_Node = No_Node then
            Tasks_Node := Make_XML_Node ("tasks");
            Append_Node_To_List (Tasks_Node, XTN.Subitems (Cheddar_Node));
         end if;

         if Buffers_Node = No_Node then
            Buffers_Node := Make_XML_Node ("buffers");
            Append_Node_To_List (Buffers_Node, XTN.Subitems (Cheddar_Node));
         end if;

         if Resources_Node = No_Node then
            Resources_Node := Make_XML_Node ("resources");
            Append_Node_To_List (Resources_Node, XTN.Subitems (Cheddar_Node));
         end if;

         if Dependencies_Node = No_Node then
            Dependencies_Node := Make_XML_Node ("dependencies");
            Append_Node_To_List
              (Dependencies_Node,
               XTN.Subitems (Cheddar_Node));
         end if;
      end if;

      Visit_Subcomponents_Of (E);

      if E = Root_System_Node then
         Pop_Entity;
         Pop_Entity; --  A
      end if;
   end Visit_System;

end Ocarina.Backends.Cheddar.Main;
