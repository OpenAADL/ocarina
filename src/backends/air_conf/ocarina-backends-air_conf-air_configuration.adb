------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BACKENDS.AIR_CONF.AIR_CONFIGURATION                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2018-2019 ESA & ISAE.                    --
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

with Utils;         use Utils;
with Ocarina.Namet; use Ocarina.Namet;

with Ocarina.ME_AADL;

with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Backends.Properties;
with Ocarina.Backends.Properties.ARINC653;

with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.XML_Values;
with Ocarina.Backends.C_Common.Mapping;

package body Ocarina.Backends.AIR_Conf.AIR_Configuration is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Properties.ARINC653;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.C_Common.Mapping;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;
   package XV renames Ocarina.Backends.XML_Values;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

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
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   --------------------------
   -- Map_Connection_Table --
   --------------------------

   procedure Map_Connection_Table (E : Node_Id)
     with Pre => (Get_Category_Of_Component (E) = CC_System);

   procedure Map_Connection_Table (E : Node_Id) is
      Channel_Identifier : Unsigned_Long_Long := 0;
      C                     : Node_Id;
      P                     : Node_Id;
      Q                     : Node_Id;
      Source_Port_Name      : Name_Id;
      Destination_Port_Name : Name_Id;
      Destination_Partition : Node_Id;
      Source_Partition      : Node_Id;
      Connection_Table_Node : Node_Id;
      Channel_Node          : Node_Id;
      Source_Node           : Node_Id;
      Destination_Node      : Node_Id;
      Standard_Partition_Node : Node_Id;

   begin
      if not AINU.Is_Empty (Connections (E)) then

         Append_Node_To_List
           (Make_XML_Comment (Get_String_Name ("Connection Table")),
            XTN.Subitems (Current_XML_Node));

         Connection_Table_Node := Make_XML_Node ("Connection_Table");

         C := First_Node (Connections (E));
         while Present (C) loop

            if Kind (C) = K_Connection_Instance then
               Source_Port_Name :=
                 Map_C_Enumerator_Name
                 (AIN.Item
                    (AIN.Next_Node
                       (AIN.First_Node (AIN.Path (AIN.Source (C))))),
                  Fully_Qualify_Parent => True);

               Destination_Port_Name :=
                 Map_C_Enumerator_Name
                 (AIN.Item
                    (AIN.Next_Node
                       (AIN.First_Node
                          (AIN.Path (AIN.Destination (C))))),
                  Fully_Qualify_Parent => True);

               Source_Partition :=
                 AIN.Corresponding_Instance
                   (AIN.Item (AIN.First_Node (AIN.Path (AIN.Source (C)))));

               Destination_Partition :=
                 AIN.Corresponding_Instance
                   (AIN.Item
                      (AIN.First_Node (AIN.Path (AIN.Destination (C)))));

               --  Channel node

               Channel_Node := Make_XML_Node ("Channel");
               Append_Node_To_List
                 (Channel_Node,
                  XTN.Subitems (Connection_Table_Node));

               --  Channel identifier

               Set_Str_To_Name_Buffer ("ChannelIdentifier");
               P := Make_Defining_Identifier (Name_Find);
               Set_Str_To_Name_Buffer ("");
               Add_ULL_To_Name_Buffer (Channel_Identifier, 10);
               Q := Make_Defining_Identifier (Remove_Char (Name_Find, ' '));

               Append_Node_To_List (Make_Assignement (P, Q),
                                    XTN.Items (Channel_Node));
               Channel_Identifier := Channel_Identifier + 1;

               --  Channel name

               XTU.Add_Attribute ("ChannelName",
                                  Get_Name_String
                                    (To_Lower
                                       (Display_Name
                                          (Identifier (C)))), Channel_Node);

               --  Mapping of the source

               Source_Node := Make_XML_Node ("Source");
               Append_Node_To_List (Source_Node, XTN.Subitems (Channel_Node));

               Standard_Partition_Node := Make_XML_Node ("Standard_Partition");
               Append_Node_To_List (Standard_Partition_Node,
                                    XTN.Subitems (Source_Node));

               Set_Str_To_Name_Buffer ("PartitionIdentifier");
               P := Make_Defining_Identifier (Name_Find);
               Q :=
                 Make_Literal
                   (XV.New_Numeric_Value
                      (Get_Partition_Identifier
                         (Get_Bound_Processor (Source_Partition)),
                       0,
                       10));
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Standard_Partition_Node));

               Set_Str_To_Name_Buffer ("PartitionName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String
                 (To_Lower
                    (Display_Name
                       (Identifier
                          (Parent_Subcomponent (Source_Partition)))));
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Standard_Partition_Node));

               Set_Str_To_Name_Buffer ("PortName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Source_Port_Name);
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Standard_Partition_Node));

               --  Mapping of the destination

               Destination_Node := Make_XML_Node ("Destination");
               Append_Node_To_List
                 (Destination_Node,
                  XTN.Subitems (Channel_Node));

               Standard_Partition_Node := Make_XML_Node ("Standard_Partition");
               Append_Node_To_List (Standard_Partition_Node,
                                    XTN.Subitems (Destination_Node));

               Set_Str_To_Name_Buffer ("PartitionIdentifier");
               P := Make_Defining_Identifier (Name_Find);
               Q :=
                 Make_Literal
                   (XV.New_Numeric_Value
                      (Get_Partition_Identifier
                         (Get_Bound_Processor
                         (Destination_Partition)),
                       0,
                       10));
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Standard_Partition_Node));

               Set_Str_To_Name_Buffer ("PartitionName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String
                 (To_Lower
                    (Display_Name
                       (Identifier
                          (Parent_Subcomponent (Destination_Partition)))));
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Standard_Partition_Node));

               Set_Str_To_Name_Buffer ("PortName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Destination_Port_Name);
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Standard_Partition_Node));
            end if;

            C := Next_Node (C);
         end loop;

         Append_Node_To_List (Connection_Table_Node,
                              XTN.Subitems (Current_XML_Node));
      end if;
   end Map_Connection_Table;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      U : Node_Id;
      R : Node_Id;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      R := XTN.Node (Backend_Node (Identifier (E)));

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Push_Entity (U);
      Push_Entity (R);

      Map_Connection_Table (E);
      Visit_Subcomponents_Of (E);

      Pop_Entity;
      Pop_Entity;
   end Visit_System_Instance;

   procedure Map_AIR_Configuration_Node (E : Node_Id)
     with Pre => (Get_Category_Of_Component (E) = CC_Processor);

   --------------------------------
   -- Map_AIR_Configuration_Node --
   --------------------------------

   procedure Map_AIR_Configuration_Node (E : Node_Id) is
      AIR_Configuration_Node : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin

      Append_Node_To_List
        (Make_XML_Comment (Get_String_Name ("Module configuration")),
         XTN.Subitems (Current_XML_Node));

      --  Create the AIR_Configuration node

      AIR_Configuration_Node := Make_XML_Node ("AIR_Configuration");

      Set_Str_To_Name_Buffer ("TicksPerSecond");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("200"); --  XXX Hardcoded ?
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List
        (Make_Assignement (P, Q),
         XTN.Items (AIR_Configuration_Node));

      Set_Str_To_Name_Buffer ("RequiredCores");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Literal
        (XV.New_Numeric_Value (Get_Number_Of_Cores (E), 0, 10));

      Append_Node_To_List
        (Make_Assignement (P, Q),
         XTN.Items (AIR_Configuration_Node));

      Append_Node_To_List
        (AIR_Configuration_Node,
         XTN.Subitems (Current_XML_Node));

   end Map_AIR_Configuration_Node;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
   begin
      Map_Air_Configuration_Node (E);
      Visit_Subcomponents_Of (E);
   end Visit_Processor_Instance;

end Ocarina.Backends.AIR_Conf.AIR_Configuration;
