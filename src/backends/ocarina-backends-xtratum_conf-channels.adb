------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.XTRATUM_CONF.CHANNELS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2011-2015 ESA & ISAE.                    --
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

with Utils; use Utils;
with Ocarina.Namet; use Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;

with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

with Ocarina.Backends.XML_Values;

package body Ocarina.Backends.Xtratum_Conf.Channels is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Backends.Utils;

   use Ocarina.Backends.Properties;
   use Ocarina.Backends.XML_Tree.Nutils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XV renames Ocarina.Backends.XML_Values;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Memory_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
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

         when CC_Process =>
            Visit_Process_Instance (E);

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when CC_Memory =>
            Visit_Memory_Instance (E);

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
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S                     : Node_Id;
      C                     : Node_Id;
      U                     : Node_Id;
      R                     : Node_Id;
      P                     : Node_Id;
      Q                     : Node_Id;
      Queue_Size            : Unsigned_Long_Long;
      Source_Port_Name      : Name_Id;
      Destination_Port_Name : Name_Id;
      Destination_Partition : Node_Id;
      Source_Partition      : Node_Id;
      Source_Port           : Node_Id;
      Associated_Data_Size  : Unsigned_Long_Long;
      Destination_Port      : Node_Id;
      Channels_Node         : Node_Id;
      Channel_Node          : Node_Id;
      Source_Node           : Node_Id;
      Destination_Node      : Node_Id;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      R := XTN.Node (Backend_Node (Identifier (E)));

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Push_Entity (U);
      Push_Entity (R);

      if not AINU.Is_Empty (Connections (E)) then

         Channels_Node := Make_XML_Node ("Channels");

         C := First_Node (Connections (E));
         while Present (C) loop
            if Kind (C) = K_Connection_Instance then

               Source_Port :=
                 (AIN.Item
                    (AIN.Next_Node
                       (AIN.First_Node (AIN.Path (AIN.Source (C))))));

               Destination_Port :=
                 AIN.Item
                   (AIN.Next_Node
                      (AIN.First_Node (AIN.Path (AIN.Destination (C)))));

               Source_Port_Name :=
                 AIN.Name
                   (AIN.Identifier
                      (AIN.Item
                         (AIN.Next_Node
                            (AIN.First_Node (AIN.Path (AIN.Source (C)))))));

               Destination_Port_Name :=
                 AIN.Name
                   (AIN.Identifier
                      (AIN.Item
                         (AIN.Next_Node
                            (AIN.First_Node
                               (AIN.Path (AIN.Destination (C)))))));

               Source_Partition :=
                 AIN.Corresponding_Instance
                   (AIN.Item (AIN.First_Node (AIN.Path (AIN.Source (C)))));

               Destination_Partition :=
                 AIN.Corresponding_Instance
                   (AIN.Item
                      (AIN.First_Node (AIN.Path (AIN.Destination (C)))));
               if not Is_Event (Source_Port) then
                  Channel_Node := Make_XML_Node ("SamplingChannel");
               else
                  Channel_Node := Make_XML_Node ("QueuingChannel");
                  Set_Str_To_Name_Buffer ("maxNoMessages");
                  P := Make_Defining_Identifier (Name_Find);
                  Get_Name_String (Source_Port_Name);

                  if Get_Queue_Size (Destination_Port) = -1 then
                     Queue_Size := 1;
                  else
                     Queue_Size :=
                       Unsigned_Long_Long (Get_Queue_Size (Destination_Port));
                  end if;
                  Q := Make_Literal (XV.New_Numeric_Value (Queue_Size, 1, 10));
                  Append_Node_To_List
                    (Make_Assignement (P, Q),
                     XTN.Items (Channel_Node));
               end if;

               Source_Node      := Make_XML_Node ("Source");
               Destination_Node := Make_XML_Node ("Destination");

               Set_Str_To_Name_Buffer ("maxMessageLength");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Source_Port_Name);

               if Get_Data_Size (Corresponding_Instance (Source_Port)) /=
                 Null_Size
               then
                  Associated_Data_Size :=
                    To_Bytes
                      (Get_Data_Size (Corresponding_Instance (Source_Port)));
               else
                  Associated_Data_Size := 1;
               end if;

               Set_Str_To_Name_Buffer
                 (Unsigned_Long_Long'Image (Associated_Data_Size));
               Add_Str_To_Name_Buffer ("B");
               Q := Make_Defining_Identifier (Remove_Char (Name_Find, ' '));

               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Channel_Node));

               Set_Str_To_Name_Buffer ("portName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Source_Port_Name);
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Source_Node));

               Set_Str_To_Name_Buffer ("partitionId");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Source_Port_Name);
               Q :=
                 Copy_Node
                   (Backend_Node
                      (Identifier (Get_Bound_Processor (Source_Partition))));
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Source_Node));

               Set_Str_To_Name_Buffer ("partitionId");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Source_Port_Name);
               Q :=
                 Copy_Node
                   (Backend_Node
                      (Identifier
                         (Get_Bound_Processor (Destination_Partition))));
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Destination_Node));

               Set_Str_To_Name_Buffer ("portName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Destination_Port_Name);
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Destination_Node));

               Set_Str_To_Name_Buffer ("partitionName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String
                 (To_Lower
                    (Display_Name
                       (Identifier
                          (Parent_Subcomponent (Destination_Partition)))));
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Destination_Node));

               Append_Node_To_List (Source_Node, XTN.Subitems (Channel_Node));
               Append_Node_To_List
                 (Destination_Node,
                  XTN.Subitems (Channel_Node));
               Append_Node_To_List
                 (Channel_Node,
                  XTN.Subitems (Channels_Node));
            end if;

            C := Next_Node (C);
         end loop;

         Append_Node_To_List (Channels_Node, XTN.Subitems (Current_XML_Node));
      end if;

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
   end Visit_System_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
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
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Virtual_Processor_Instance;

   ---------------------------
   -- Visit_Memory_Instance --
   ---------------------------

   procedure Visit_Memory_Instance (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Memory_Instance;

end Ocarina.Backends.Xtratum_Conf.Channels;
