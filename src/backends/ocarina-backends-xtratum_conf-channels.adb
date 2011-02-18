------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.XTRATUM_CONF.CHANNELS                   --
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
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

package body Ocarina.Backends.Xtratum_Conf.Channels is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Backends.Properties;
   use Ocarina.Backends.XML_Tree.Nutils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

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
      S                    : Node_Id;
      C                    : Node_Id;
      U                    : Node_Id;
      R                    : Node_Id;
      P                    : Node_Id;
      Q                    : Node_Id;
      Source_Port_Name           : Name_Id;
      Destination_Port_Name      : Name_Id;
      Destination_Partition      : Node_Id;
      Source_Partition           : Node_Id;
      Channels_Node              : Node_Id;
      Channel_Node               : Node_Id;
      Source_Node                : Node_Id;
      Destination_Node           : Node_Id;
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

               Channel_Node      := Make_XML_Node ("SamplingChannel");
               Source_Node       := Make_XML_Node ("Source");
               Destination_Node  := Make_XML_Node ("Destination");

               Source_Port_Name :=
                  AIN.Name
                     (AIN.Identifier
                        (AIN.Item
                           (AIN.Next_Node
                              (AIN.First_Node
                                 (AIN.Path
                                    (AIN.Source (C)))))));
               Destination_Port_Name :=
                  AIN.Name
                     (AIN.Identifier
                        (AIN.Item
                           (AIN.Next_Node
                              (AIN.First_Node
                                 (AIN.Path
                                    (AIN.Destination (C)))))));
               Source_Partition :=
                     AIN.Corresponding_Instance
                        (AIN.Item
                          (AIN.First_Node
                             (AIN.Path (AIN.Source (C)))));

               Destination_Partition :=
                     AIN.Corresponding_Instance
                        (AIN.Item
                          (AIN.First_Node
                             (AIN.Path (AIN.Destination (C)))));

               Set_Str_To_Name_Buffer ("portName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Source_Port_Name);
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                  (Make_Assignement (P, Q), XTN.Items (Source_Node));

               Set_Str_To_Name_Buffer ("partitionId");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Source_Port_Name);
               Q := Copy_Node
                  (Backend_Node
                     (Identifier (Get_Bound_Processor (Source_Partition))));
               Append_Node_To_List
                  (Make_Assignement (P, Q), XTN.Items (Source_Node));

               Set_Str_To_Name_Buffer ("partitionId");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Source_Port_Name);
               Q := Copy_Node
                  (Backend_Node
                     (Identifier
                        (Get_Bound_Processor (Destination_Partition))));
               Append_Node_To_List
                  (Make_Assignement (P, Q), XTN.Items (Destination_Node));

               Set_Str_To_Name_Buffer ("portName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String (Destination_Port_Name);
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                  (Make_Assignement (P, Q), XTN.Items (Destination_Node));

               Set_Str_To_Name_Buffer ("partitionName");
               P := Make_Defining_Identifier (Name_Find);
               Get_Name_String
                  (Display_Name
                     (Identifier
                        (Parent_Subcomponent
                        (Get_Bound_Processor (Destination_Partition)))));
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                  (Make_Assignement (P, Q), XTN.Items (Destination_Node));

               Append_Node_To_List (Source_Node, XTN.Subitems (Channel_Node));
               Append_Node_To_List
                  (Destination_Node, XTN.Subitems (Channel_Node));
               Append_Node_To_List
                  (Channel_Node, XTN.Subitems (Channels_Node));
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
      S                    : Node_Id;
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
      S           : Node_Id;
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
