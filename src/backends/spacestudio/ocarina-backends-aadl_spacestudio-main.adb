------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.AADL_SPACESTUDIO.MAIN                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

pragma Warnings (Off);

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ocarina.Namet;             use Ocarina.Namet;
with Ocarina.Instances.Queries; use Ocarina.Instances.Queries;
with Ocarina.AADL_Values;
with Utils;                     use Utils;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Debug;
with Ocarina.Backends.Python.Nodes;
with Ocarina.Backends.Python.Nutils;
with Ocarina.Backends.AADL_SpaceStudio.Mapping;
with Ocarina.Backends.SpaceStudio_properties;
with Ocarina.Backends.Python.Generator;
with Ocarina.Backends.C_Common.Mapping;

package body Ocarina.Backends.AADL_SpaceStudio.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Python.Nutils;
   use Ocarina.Backends.AADL_SpaceStudio.Mapping;
   use Ocarina.Backends.SpaceStudio_properties;
   use Ocarina.ME_AADL.AADL_Instances.Debug;
   use Ocarina.Backends.Python.Generator;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package PTN renames Ocarina.Backends.Python.Nodes;
   use type ATN.Node_Kind;

   procedure Visit_Component
     (E            : Node_Id;
      Concat_Name  : Name_Id;
      Proc_Binding : Name_Id);
   procedure Visit_Subcomponents_Of
     (E            : Node_Id;
      Concat_Name  : Name_Id;
      Proc_Binding : Name_Id);
   procedure Visit_Calls_Of
     (E            : Node_Id;
      Concat_Name  : Name_Id;
      Proc_Binding : Name_Id);

   function Map_Component (E : Node_Id) return Node_Id;

   -------------------
   -- Map_Component --
   -------------------

   function Map_Component (E : Node_Id) return Node_Id is
      Category_Name_String : constant array
      (Component_Category'Range) of Name_Id :=
        (CC_Abstract          => Get_String_Name ("abstract"),
         CC_Bus               => Get_String_Name ("bus"),
         CC_Data              => Get_String_Name ("data"),
         CC_Device            => Get_String_Name ("device"),
         CC_Memory            => Get_String_Name ("memory"),
         CC_Process           => Get_String_Name ("process"),
         CC_Processor         => Get_String_Name ("processor"),
         CC_Subprogram        => Get_String_Name ("Subprogram"),
         CC_Subprogram_Group  => Get_String_Name ("subprogram group"),
         CC_System            => Get_String_Name ("system"),
         CC_Thread            => Get_String_Name ("thread"),
         CC_Thread_Group      => Get_String_Name ("thread group"),
         CC_Unknown           => No_Name,
         CC_Virtual_Bus       => Get_String_Name ("virtual bus"),
         CC_Virtual_Processor => Get_String_Name ("virtual processor"));

      N               : Node_Id;
      Classifier_Node : Node_Id;
      Component       : Component_Category := Get_Category_Of_Component (E);

   begin

      N := Make_Python_Node ("component");

      --  Category

      Append_Node_To_List
        (Make_Assignement
           (Make_Defining_Identifier (Get_String_Name ("category")),
            Make_Defining_Identifier
              (Category_Name_String (Get_Category_Of_Component (E)))),
         PTN.Items (N));

      --  Identifier

      if Present (Parent_Subcomponent (E)) then
         Append_Node_To_List
           (Make_Assignement
              (Make_Defining_Identifier (Get_String_Name ("identifier")),
               Make_Defining_Identifier
                 (Display_Name (Identifier (Parent_Subcomponent (E))))),
            PTN.Items (N));
      else
         Append_Node_To_List
           (Make_Assignement
              (Make_Defining_Identifier (Get_String_Name ("identifier")),
               Make_Defining_Identifier (Display_Name (Identifier (E)))),
            PTN.Items (N));
      end if;

      --  Classifier

      Classifier_Node := Make_Python_Node ("classifier");
      Append_Node_To_List
        (Make_Defining_Identifier (Display_Name (Identifier (E))),
         PTN.Subitems (Classifier_Node));
      Append_Node_To_List (Classifier_Node, PTN.Subitems (N));

      return N;
   end Map_Component;

   ----------------------------
   -- Visit_Subcomponents_Of --
   ----------------------------

   procedure Visit_Subcomponents_Of
     (E            : Node_Id;
      Concat_Name  : Name_Id;
      Proc_Binding : Name_Id)
   is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            Visit (Corresponding_Instance (S), Concat_Name, Proc_Binding);
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Subcomponents_Of;

   Root_System_Node    : Node_Id := No_Node;
   AADL_Python_Node    : Node_Id := No_Node;
   Current_Python_Node : Node_Id := No_Node;

   --------------------
   -- Visit_Calls_Of --
   --------------------

   procedure Visit_Calls_Of
     (E            : Node_Id;
      Concat_Name  : Name_Id;
      Proc_Binding : Name_Id)
   is
      S : Node_Id;
      T : Node_Id;
   begin
      if not AINU.Is_Empty (Calls (E)) then
         S := First_Node (Calls (E));
         while Present (S) loop
            if not AINU.Is_Empty (Subprogram_Calls (S)) then
               T := First_Node (Subprogram_Calls (S));
               while Present (T) loop
                  --  Visit the component instance corresponding to the
                  --  subcomponent S.
                  Visit
                    (Corresponding_Instance (T),
                     Concat_Name,
                     Display_Name (Identifier (S)));
                  T := Next_Node (T);
               end loop;
            end if;
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Calls_Of;

   -----------
   -- Visit --
   -----------

   procedure Visit
     (E            : Node_Id;
      Concat_Name  : Name_Id;
      Proc_Binding : Name_Id)
   is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Root_System_Node := Root_System (E);
            Visit (Root_System_Node, Concat_Name, Proc_Binding);

         when K_Component_Instance =>
            Visit_Component (E, Concat_Name, Proc_Binding);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------
   -- Visit_Component --
   ---------------------

   procedure Visit_Component
     (E            : Node_Id;
      Concat_Name  : Name_Id;
      Proc_Binding : Name_Id)
   is
      Category : constant Component_Category := Get_Category_Of_Component (E);
      N        : Node_Id;

      Old_Python_Node     : Node_Id;
      Subcomponents_Node  : Node_Id;
      Features_Node       : Node_Id;
      Properties_Node     : Node_Id;
      Feature_Node        : Node_Id;
      F                   : Node_Id;
      P                   : Node_Id;
      U                   : Node_Id;
      Property_Node       : Node_Id;
      Components_Node     : Node_Id;
      Binding_Node        : Node_Id;
      Property_Value_Node : Node_Id;
      AADL_Property_Value : Node_Id;
      chab                : Node_Id;

      Ports            : Port_List.Fifo_Type;
      New_Proc_Binding : Name_Id := Proc_Binding;
      New_concat_Name  : Name_Id;
      New_add          : Name_Id;

   begin
      if Category = CC_System then
         P := Map_HI_Node (E);
         Push_Entity (P);

         U := Map_HI_Unit (E);
         Push_Entity (U);

         chab :=
           Ocarina.Backends.C_Common.Mapping.Map_C_Data_Type_Designator (E);

         if AADL_Python_Node = No_Node then
            AADL_Python_Node := Make_Python_Node ("projet");

            Python.Generator.project.project_name :=
              Display_Name (Identifier (Root_System_Node));

            Append_Node_To_List
              (AADL_Python_Node,
               PTN.Subitems (PTN.Root_Node (PTN.Python_File (U))));

         end if;
         Current_Python_Node := Components_Node;

         New_concat_Name := Get_String_Name ("system");

      else

         if Category = CC_Subprogram then
            New_concat_Name := Concat_Name;
         else
            New_add := Display_Name (Identifier (Parent_Subcomponent (E)));
            New_concat_Name :=
              Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                ("_" & Get_Name_String (New_add),
                 Concat_Name);
         end if;

         if Category = CC_Processor then
            New_Proc_Binding :=
              Display_Name (Identifier (Parent_Subcomponent (E)));
         end if;

      end if;

      --  First bits of the component node
      --  XXX  TODO VIRER MAP_COMPONENT
      N := Map_Component (E);

      --  Features

      if Present (Features (E)) then
         F := First_Node (Features (E));
         while Present (F) loop
            Feature_Node := Make_Python_Node ("feature");

            --  Type: event/data/event data

            declare
               Type_Node    : Node_Id;
               Type_Kind    : Name_Id;
               Port_element : Port;
               Data         : Node_Id;
               Data_Type    : Node_Id;
            begin
               Type_Node := Make_Python_Node ("type");
               if (Kind (F) = K_Port_Spec_Instance) then

                  --  Direction: in/out/inout

                  if Is_In (F) and then not Is_Out (F) then
                     Port_element.direction := Get_String_Name ("in");
                  elsif (not Is_In (F)) and then Is_Out (F) then
                     Port_element.direction := Get_String_Name ("out");
                  elsif Is_In (F) and then Is_Out (F) then
                     Port_element.direction := Get_String_Name ("inout");
                  end if;

                  --  Type: event/data/event data

                  if Is_Event (F) and then not Is_Data (F) then
                     Type_Kind := Get_String_Name ("event");
                  elsif not (Is_Event (F)) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("data");
                     Data      := Corresponding_Instance (F);
                     pragma Assert
                       (Present (Properties (Data)),
                        "No C type found for " &
                        Get_Name_String (Display_Name (Identifier (Data))) &
                        " data with Data_Model::Representation property");
                     pragma Assert
                       (Get_Name_String
                          (Display_Name
                             (Identifier (First_Node (Properties (Data))))) =
                        "Data_Model::Representation",
                        "No C type found for " &
                        Get_Name_String (Display_Name (Identifier (Data))) &
                        " data with Data_Model::Representation property");
                     Data_Type :=
                       Get_Value_Of_Property_Association
                         (Data,
                          Name (Identifier (First_Node (Properties (Data)))));
                     Port_element.c_type :=
                       Get_String_Name
                         (Ocarina.AADL_Values.Image
                            (ATN.Value (Data_Type),
                             Quoted => False));
                  elsif Is_Event (F) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("event_data");
                     Data      := Corresponding_Instance (F);
                     pragma Assert
                       (Present (Properties (Data)),
                        "No C type found for " &
                        Get_Name_String (Display_Name (Identifier (Data))) &
                        " data with Data_Model::Representation property");
                     pragma Assert
                       (Get_Name_String
                          (Display_Name
                             (Identifier (First_Node (Properties (Data))))) =
                        "Data_Model::Representation",
                        "No C type found for " &
                        Get_Name_String (Display_Name (Identifier (Data))) &
                        " data with Data_Model::Representation property");
                     Data_Type :=
                       Get_Value_Of_Property_Association
                         (Data,
                          Name (Identifier (First_Node (Properties (Data)))));
                     Port_element.c_type :=
                       Get_String_Name
                         (Ocarina.AADL_Values.Image
                            (ATN.Value (Data_Type),
                             Quoted => False));
                  end if;
                  Port_element.name :=
                    Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                      ("_" & Get_Name_String (Display_Name (Identifier (F))),
                       New_concat_Name);
                  Port_element.port_type := Type_Kind;
                  Port_List.Push (Ports, Port_element);
               elsif Kind (F) = K_Parameter_Instance then

                  --  Direction: in/out/inout

                  if Is_In (F) and then not Is_Out (F) then
                     Port_element.direction := Get_String_Name ("in");
                  elsif (not Is_In (F)) and then Is_Out (F) then
                     Port_element.direction := Get_String_Name ("out");
                  elsif Is_In (F) and then Is_Out (F) then
                     Port_element.direction := Get_String_Name ("inout");
                  end if;

                  --  verification des types des ports

                  Type_Kind := Get_String_Name ("data");
                  Data      := Corresponding_Instance (F);
                  pragma Assert
                    (Present (Properties (Data)),
                     "No C type found for " &
                     Get_Name_String (Display_Name (Identifier (Data))) &
                     " data with Data_Model::Representation property");
                  pragma Assert
                    (Get_Name_String
                       (Display_Name
                          (Identifier (First_Node (Properties (Data))))) =
                     "Data_Model::Representation",
                     "No C type found for " &
                     Get_Name_String (Display_Name (Identifier (Data))) &
                     " data with Data_Model::Representation property");
                  Data_Type :=
                    Get_Value_Of_Property_Association
                      (Data,
                       Name (Identifier (First_Node (Properties (Data)))));
                  Port_element.c_type :=
                    Get_String_Name
                      (Ocarina.AADL_Values.Image
                         (ATN.Value (Data_Type),
                          Quoted => False));
                  Port_element.name      := Display_Name (Identifier (F));
                  Port_element.port_type := Type_Kind;
                  Port_List.Push (Ports, Port_element);
               elsif Kind (F) = K_Subcomponent_Access_Instance then
                  Type_Kind := Get_String_Name ("access");
               else
                  Type_Kind := Get_String_Name ("feature");
               end if;
            end;

            F := Next_Node (F);
         end loop;
      end if;

      --  Connections
      declare
         Dest                : Node_Id;
         Src                 : Node_Id;
         Port_Dest           : Node_Id;
         Port_Src            : Node_Id;
         do_connection       : Boolean;
         connection, element : td_connection;
         Source_Name         : Name_Id := Get_String_Name (" ");
         Source_Type         : Name_Id := Get_String_Name (" ");
         Destination_Name    : Name_Id := Get_String_Name (" ");
         Destination_Type    : Name_Id := Get_String_Name (" ");

      begin
         if Present (Connections (E)) then
            F := First_Node (Connections (E));
            while Present (F) loop

               if Kind (F) = K_Connection_Instance then
                  Dest      := Item (First_Node (Path (Destination (F))));
                  Src       := Item (First_Node (Path (Source (F))));
                  Port_Dest := Item (Last_Node (Path (Destination (F))));
                  Port_Src  := Item (Last_Node (Path (Source (F))));

                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Bus
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Processor
                  then
                     --  Insertion du couple Bus/Proc dans une hash map
                     --  qui sera passée en argument et traitée dans
                     --  la méthode Prop_Component

                     Hash_Name.Insert
                       (Map_BP,
                        Display_Name (Identifier (Dest)),
                        Display_Name (Identifier (Src)));
                  end if;

                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Bus
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Memory
                  then
                     Hash_Name.Insert
                       (Map_BM,
                        Display_Name (Identifier (Dest)),
                        Display_Name (Identifier (Src)));
                  end if;

                  --  Binding d'un device sur un bus
                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Bus
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Device
                  then
                     Hash_Name.Insert
                       (Map_BD,
                        Display_Name (Identifier (Dest)),
                        Display_Name (Identifier (Src)));
                  end if;

                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Subprogram
                  then
                     declare
                        ports_sub : Sub_ports;
                     begin
                        Source_Name := Display_Name (Identifier (Port_Src));
                        Destination_Name :=
                          Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                            ("_" &
                             Get_Name_String
                               (Display_Name (Identifier (Dest))),
                             New_concat_Name);
                        if Hash_Subprogram.Contains
                            (Subprogram_Ports,
                             Destination_Name)
                        then
                           ports_sub :=
                             Hash_Subprogram.Element
                               (Subprogram_Ports,
                                Destination_Name);
                           Name_List.Push (ports_sub.List, Source_Name);
                           Hash_Subprogram.Replace
                             (Subprogram_Ports,
                              Destination_Name,
                              ports_sub);
                        else
                           Name_List.Push (ports_sub.List, Source_Name);
                           Hash_Subprogram.Insert
                             (Subprogram_Ports,
                              Destination_Name,
                              ports_sub);
                        end if;
                     end;
                  end if;

                  if Get_Category_Of_Component
                      (Corresponding_Instance (Dest)) =
                    CC_Subprogram
                  then
                     declare
                        ports_sub : Sub_ports;
                     begin
                        Source_Name :=
                          Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                            ("_" &
                             Get_Name_String (Display_Name (Identifier (Src))),
                             New_concat_Name);
                        Destination_Name :=
                          Display_Name (Identifier (Port_Dest));
                        if Hash_Subprogram.Contains
                            (Subprogram_Ports,
                             Source_Name)
                        then
                           ports_sub :=
                             Hash_Subprogram.Element
                               (Subprogram_Ports,
                                Source_Name);
                           Name_List.Push (ports_sub.List, Destination_Name);
                           Hash_Subprogram.Replace
                             (Subprogram_Ports,
                              Source_Name,
                              ports_sub);
                        else
                           Name_List.Push (ports_sub.List, Destination_Name);
                           Hash_Subprogram.Insert
                             (Subprogram_Ports,
                              Source_Name,
                              ports_sub);
                        end if;
                     end;
                  end if;

                  do_connection := False;

                  --  Stockage des différentes connections

                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Device
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Device
                  then
                     do_connection := True;

                     Source_Type := Get_String_Name ("device");
                     Source_Name :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))),
                          New_concat_Name);
                     Destination_Type := Get_String_Name ("device");
                     Destination_Name :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))),
                          New_concat_Name);

                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Process
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Device
                  then
                     do_connection := True;

                     Destination_Type := Get_String_Name ("device");
                     Destination_Name :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))),
                          New_concat_Name);

                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Device
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Process
                  then
                     do_connection := True;

                     Source_Type := Get_String_Name ("device");
                     Source_Name :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))),
                          New_concat_Name);

                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Process
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Process
                  then
                     do_connection             := True;
                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Thread
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Thread
                  then
                     do_connection := True;

                     Source_Type := Get_String_Name ("thread");
                     Source_Name :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))),
                          New_concat_Name);
                     Destination_Type := Get_String_Name ("thread");
                     Destination_Name :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))),
                          New_concat_Name);

                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (Corresponding_Instance (Src)) =
                    CC_Thread_Group
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Thread_Group
                  then
                     do_connection             := True;
                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (E) = CC_Process
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Thread
                  then
                     do_connection := True;

                     Destination_Type := Get_String_Name ("thread");
                     Destination_Name :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))),
                          New_concat_Name);

                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (E) = CC_Process
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Src)) =
                      CC_Thread
                  then
                     do_connection := True;

                     Source_Type := Get_String_Name ("thread");
                     Source_Name :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))),
                          New_concat_Name);

                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (E) = CC_Process
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Thread_Group
                  then
                     do_connection             := True;
                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         (Get_Name_String (Display_Name (Identifier (Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (E) = CC_Process
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Src)) =
                      CC_Thread_Group
                  then
                     do_connection             := True;
                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (E) = CC_Thread_Group
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Dest)) =
                      CC_Thread
                  then
                     do_connection := True;

                     Destination_Type := Get_String_Name ("thread");
                     Source_Name      :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))),
                          New_concat_Name);

                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Dest))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if Get_Category_Of_Component (E) = CC_Thread_Group
                    and then
                      Get_Category_Of_Component
                        (Corresponding_Instance (Src)) =
                      CC_Thread
                  then
                     do_connection := True;

                     Source_Type := Get_String_Name ("thread");
                     Source_Name :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))),
                          New_concat_Name);

                     connection.beginning_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String (Display_Name (Identifier (Src))) &
                          "_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Src))),
                          New_concat_Name);
                     connection.ending_port :=
                       Ocarina.Backends.Python.Nutils.Add_Suffix_To_Name
                         ("_" &
                          Get_Name_String
                            (Display_Name (Identifier (Port_Dest))),
                          New_concat_Name);
                  end if;

                  if do_connection then
                     Connect_port
                       (connection,
                        Source_Name,
                        Source_Type,
                        Destination_Name,
                        Destination_Type);
                  end if;

               end if;
               F := Next_Node (F);
            end loop;
         end if;
      end;

      --  Properties

      declare
         --  Cette hash map sera utilisée dans la méthode Prop_Component
         Map_Property : Hash_Property.Map;

      begin

         if Present (Properties (E)) then
            F := First_Node (Properties (E));
            while Present (F) loop
               --  XXX Warning, if there are multiple values for a
               --  property (e.g. because of inheritance), then all
               --  values are dumped.

               AADL_Property_Value :=
                 Get_Value_Of_Property_Association (E, Name (Identifier (F)));

               if Present (AADL_Property_Value)
                 and then
                   ATN.Kind (AADL_Property_Value) =
                   ATN.K_Signed_AADLNumber
                 and then Present (ATN.Unit_Identifier (AADL_Property_Value))
               then
                  --  This property value denotes a property with a unit
                  --  identifier.

                  Hash_Property.Insert
                    (Map_Property,
                     To_Lower (Display_Name (Identifier (F))),
                     Get_String_Name
                       (Ocarina.AADL_Values.Image
                          (ATN.Value
                             (ATN.Number_Value (AADL_Property_Value)))));

               elsif Present (AADL_Property_Value)
                 and then
                   ATN.Kind (AADL_Property_Value) =
                   ATN.K_Signed_AADLNumber
                 and then
                 (not Present (ATN.Unit_Identifier (AADL_Property_Value)))
               then
                  --  This property value denotes a property without unit

                  Hash_Property.Insert
                    (Map_Property,
                     To_Lower (Display_Name (Identifier (F))),
                     Get_String_Name
                       (Ocarina.AADL_Values.Image
                          (ATN.Value
                             (ATN.Number_Value (AADL_Property_Value)))));

               elsif Present (AADL_Property_Value)
                 and then ATN.Kind (AADL_Property_Value) = ATN.K_Literal
               then
                  --  This property value denotes a literal

                  Hash_Property.Insert
                    (Map_Property,
                     To_Lower (Display_Name (Identifier (F))),
                     Get_String_Name
                       (Ocarina.AADL_Values.Image
                          (ATN.Value (AADL_Property_Value),
                           Quoted => False)));

               elsif Present (AADL_Property_Value)
                 and then ATN.Kind (AADL_Property_Value) = ATN.K_Reference_Term
               then
                  --  This property value denotes a reference term

                  Hash_Property.Insert
                    (Map_Property,
                     To_Lower (Display_Name (Identifier (F))),
                     ATN.Display_Name
                       (ATN.First_Node --  XXX must iterate
                          (ATN.List_Items
                             (ATN.Reference_Term (AADL_Property_Value)))));

                  if Get_Name_String
                      (To_Lower (Display_Name (Identifier (F)))) =
                    "actual_processor_binding" or
                    Get_Name_String
                        (To_Lower (Display_Name (Identifier (F)))) =
                      "allowed_processor_binding"
                  then
                     New_Proc_Binding :=
                       ATN.Display_Name
                         (ATN.First_Node --  XXX must iterate
                            (ATN.List_Items
                               (ATN.Reference_Term (AADL_Property_Value))));
                  end if;

               elsif Present (AADL_Property_Value)
                 and then
                   ATN.Kind (AADL_Property_Value) =
                   ATN.K_Enumeration_Term
               then
                  --  This property value denotes an enumeration term

                  Hash_Property.Insert
                    (Map_Property,
                     To_Lower (Display_Name (Identifier (F))),
                     ATN.Display_Name (ATN.Identifier (AADL_Property_Value)));

               end if;
               F := Next_Node (F);
            end loop;
         end if;

         --  Subcomponents

         Visit_Subcomponents_Of (E, New_concat_Name, New_Proc_Binding);
         Visit_Calls_Of (E, New_concat_Name, New_Proc_Binding);

         Prop_Component
           (E,
            Map_Property,
            New_concat_Name,
            New_Proc_Binding,
            Ports);
      end;

      if Category = CC_System then
         --  Binding;
         Pop_Entity;
         Pop_Entity; --  A
      end if;
   end Visit_Component;

end Ocarina.Backends.AADL_SpaceStudio.Main;
