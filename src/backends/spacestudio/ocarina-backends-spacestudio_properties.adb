------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.SPACESTUDIO_PROPERTIES                  --
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

pragma Style_Checks (Off);
pragma Warnings (Off);

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--with Strings.Maps.Constants;
--with Ada.Containers.Hashed_Maps;

with Ocarina.Namet;             use Ocarina.Namet;
with Ocarina.Instances.Queries; use Ocarina.Instances.Queries;
with Ocarina.AADL_Values;
with Utils;                     use Utils;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.python.Nodes;
with Ocarina.Backends.python.Nutils;

package body Ocarina.Backends.SpaceStudio_properties is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.python.Nutils;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.python.Nodes;
   use type ATN.Node_Kind;

   Compteur_id_Module : Integer := 0;
   Hash_Map           : Hash_Instance.Map;
   Binding_Map        : Hash_Binding.Map;

   --------------------
   -- Prop_Component --
   --------------------

   procedure Prop_Component
     (E            : Node_Id;
      Map          : Hash_Property.Map;
      Concat_Name  : Name_Id;
      Proc_Binding : Name_Id;
      Ports        : Port_List.Fifo_Type)
   is
      Component : Component_Category := Get_Category_Of_Component (E);

   begin
      case Component is
         when CC_Subprogram =>
            pragma Assert
              ((Proc_Binding = Get_String_Name ("before")) or
               (Proc_Binding = Get_String_Name ("loop")) or
               (Proc_Binding = Get_String_Name ("after")),
               Get_Name_String (Proc_Binding) &
               " is not a valid name for a list of call(before, loop, after)");
            if Hash_Property.Contains
                (Map,
                 Get_String_Name ("source_text"))
            then
               pragma Assert
                 (Hash_Property.Contains
                    (Map,
                     Get_String_Name ("source_name")),
                  Get_Name_String
                    (Display_Name (Identifier (Parent_Subcomponent (E)))) &
                  " : give this function a name with the source_name property");
               declare
                  --Subprogram_B : python.Generator.Binding;
                  call_B  : python.Generator.Call_Funct;
                  funct_B : python.Generator.Funct;
               begin
                  funct_B.source :=
                    Hash_Property.Element
                      (Map,
                       Get_String_Name ("source_text"));
                  funct_B.name :=
                    Hash_Property.Element
                      (Map,
                       Get_String_Name ("source_name"));
                  funct_B.ports := Ports;
                  if Hash_Calls.Contains (Subprog_Calls, Concat_Name) then
                     call_B := Hash_Calls.Element (Subprog_Calls, Concat_Name);
                     Hash_Calls.Delete (Subprog_Calls, Concat_Name);
                  end if;
                  if Proc_Binding = Get_String_Name ("before") then
                     call_B.Before_Function := funct_B;
                     call_B.has_before      := True;
                  elsif Proc_Binding = Get_String_Name ("loop") then
                     call_B.Loop_Function := funct_B;
                     call_B.has_loop      := True;
                  elsif Proc_Binding = Get_String_Name ("after") then
                     call_B.After_Function := funct_B;
                     call_B.has_after      := True;
                  end if;
                  Hash_Calls.Insert (Subprog_Calls, Concat_Name, call_B);
               end;
            end if;

         when CC_Device =>
            declare
               Instance_Name : Name_Id :=
                 Display_Name (Identifier (Parent_Subcomponent (E)));
               Device_B : python.Generator.Binding;
            begin
               pragma Assert
                 (Hash_Name.Contains (Map_BD, Instance_Name),
                  Get_Name_String (Instance_Name) & " is not bound to a bus");
               Device_B.first_name  := Concat_Name;
               Device_B.second_name :=
                 Hash_Name.Element (Map_BD, Instance_Name);
               Device_B.ports := Ports;
               Python.Generator.Binding_List.Push
                 (python.Generator.project.Device_List,
                  Device_B);
            end;

         when CC_Memory =>
            declare
               temp          : Name_Id;
               Mem_B         : python.Generator.Binding;
               Id : Name_Id := Display_Name (Identifier (E)); --Concat_Name;
               Instance_Name : Name_Id :=
                 Display_Name (Identifier (Parent_Subcomponent (E)));
            begin
               -- Schéma classique de récupération d'une propriété
               if Hash_Property.Contains
                   (Map,
                    Get_String_Name ("spacestudio::subtype_component"))
               then
                  temp :=
                    Hash_Property.Element
                      (Map,
                       Get_String_Name ("spacestudio::subtype_component"));

                  -- Si la propriété n'a pas été définie, on attribut
                  --  une valeur de base
               else
                  temp := Get_String_Name ("XilinxBRAM");
               end if;

               Mem_B.instance_name  := Instance_Name;
               Mem_B.first_name     := Id;
               Mem_B.component_type := temp;

               pragma Assert
                 (Hash_Name.Contains (Map_BM, Instance_Name),
                  Get_Name_String (Instance_Name) & " is not bound to a bus");
               Mem_B.second_name := Hash_Name.Element (Map_BM, Instance_Name);

               Binding_List.Push (python.Generator.project.Memory_List, Mem_B);

            end;

         when CC_Bus =>
            declare
               temp          : Name_Id;
               Bus_B         : python.Generator.Binding;
               Id : Name_Id := Display_Name (Identifier (E)); --Concat_Name;
               Instance_Name : Name_Id :=
                 Display_Name (Identifier (Parent_Subcomponent (E)));
            begin
               -- Schéma classique de récupération d'une propriété
               if Hash_Property.Contains
                   (Map,
                    Get_String_Name ("spacestudio::subtype_component"))
               then
                  temp :=
                    Hash_Property.Element
                      (Map,
                       Get_String_Name ("spacestudio::subtype_component"));

                  -- Si la propriété n'a pas été définie, on attribut
                  --  une valeur de base
               else
                  temp := Get_String_Name ("AMBA_AXIBus");
               end if;

               Bus_B.first_name     := Instance_Name;
               Bus_B.component_type := temp;
               Binding_List.Push (python.Generator.project.Bus_List, Bus_B);
            end;

         when CC_Processor =>
            declare
               temp            : Name_Id;
               Processor_B     : python.Generator.Binding;
               Binding_Element : Processor_Binding;
               Id : Name_Id := Display_Name (Identifier (E)); --Concat_Name;
               Instance_Name   : Name_Id :=
                 Display_Name (Identifier (Parent_Subcomponent (E)));
            begin
               if Hash_Property.Contains
                   (Map,
                    Get_String_Name ("spacestudio::subtype_component"))
               then
                  temp :=
                    Hash_Property.Element
                      (Map,
                       Get_String_Name ("spacestudio::subtype_component"));
               else
                  temp := Get_String_Name ("uBlaze");
               end if;

               -- On s'assure que le processeur est bien bindé sur un bus

               pragma Assert
                 (Hash_Name.Contains (Map_BP, Instance_Name),
                  Get_Name_String (Instance_Name) & " is not bound to a bus");
               Processor_B.second_name :=
                 Hash_Name.Element (Map_BP, Instance_Name);
               Processor_B.component_type := temp;
               Processor_B.first_name     := Id;
               Processor_B.instance_name  := Instance_Name;

               if Hash_Property.Contains
                 (Map,
                  Get_String_Name ("spacestudio::simulation_model"))
               then
                  Processor_B.Simulation_Model :=
                    Hash_Property.Element
                      (Map,
                       Get_String_Name ("spacestudio::simulation_model"));
               else
                  Processor_B.Simulation_Model :=
                    Get_String_Name ("None");
               end if;

               if Hash_Property.Contains
                   (Map,
                    Get_String_Name ("scheduling_policy"))
               then
                  Processor_B.scheduling_policy :=
                    Hash_Property.Element
                      (Map,
                       Get_String_Name ("scheduling_policy"));
               else
                  Processor_B.scheduling_policy :=
                    Get_String_Name ("round_robin");
               end if;

               Binding_List.Push
                 (python.Generator.project.Processor_List,
                  Processor_B);
            end;

         when CC_Thread =>
            declare
               element : Integer := 1;
               Id_name : Name_Id :=
                 Concat_Name;--Display_Name (Identifier (E));
               Id              : String := Get_Name_String (Id_name);
               Binding_Element : Processor_Binding;
               Module          : Module_Element;
               Module_B        : python.Generator.Binding;

            begin

               Module.Name := Display_Name (Identifier (E));
               if Hash_Property.Contains
                   (Map,
                    Get_String_Name ("priority"))
               then
                  Module_B.priority :=
                    Hash_Property.Element (Map, Get_String_Name ("priority"));
               else
                  Module_B.priority :=
                    Get_String_Name
                      (Trim (Integer'Image (18), Ada.Strings.Left));
               end if;

               if Hash_Property.Contains
                   (Map,
                    Get_String_Name ("dispatch_protocol"))
               then
                  Module_B.dispatch_protocol :=
                    Hash_Property.Element
                      (Map,
                       Get_String_Name ("dispatch_protocol"));
               else
                  Module_B.dispatch_protocol := Get_String_Name ("none");
               end if;

               if Hash_Property.Contains (Map, Get_String_Name ("period")) then
                  Module_B.period :=
                    Hash_Property.Element (Map, Get_String_Name ("period"));
               else
                  Module_B.period :=
                    Get_String_Name
                      (Trim (Integer'Image (0), Ada.Strings.Left));
               end if;

               if Hash_Calls.Contains (Subprog_Calls, Id_name) then
                  Module_B.function_calls :=
                    Hash_Calls.Element (Subprog_Calls, Id_name);
               end if;

               Module_B.second_name := Proc_Binding;
               Module_B.first_name  := Id_name;
               Module_B.ports       := Ports;
               Binding_List.Push
                 (python.Generator.project.Module_List,
                  Module_B);

            end;

         when others =>
            Put ("");--TODO traitement des autres composants 2nd time");

      end case;

   end Prop_Component;

   -------------
   -- Binding --
   -------------

   procedure Binding is
      Module          : Module_Element;
      Position : Hash_Binding.Cursor := Hash_Binding.First (Binding_Map);
      Binding_Element : Processor_Binding;
      Module_B        : python.Generator.Binding;
   begin

      -- On parcours la liste de processeur
      while Hash_Binding.Has_Element (Position) loop

         Binding_Element := Hash_Binding.Element (Position);

         Module_B.second_name := Binding_Element.InstanceName;

   -- Pour chaque processeur, on parcours la liste des modules mappés dessus
         pragma Assert
           (not Module_List.Is_Empty (Binding_Element.List),
            Get_Name_String (Binding_Element.InstanceName) &
            " doesn't run any thread");
         while not Module_List.Is_Empty (Binding_Element.List) loop
            Module_List.Pop (Binding_Element.List, Module);

            Module_B.first_name := Module.Name;
            Module_B.priority   := Module.Priority;

            Binding_List.Push (python.Generator.project.Module_List, Module_B);

         end loop;
         Hash_Binding.Next (Position);
      end loop;
   end Binding;

   procedure Connect_port
     (connection       : td_connection;
      Source_Name      : Name_Id;
      Source_Type      : Name_Id;
      Destination_Name : Name_Id;
      Destination_Type : Name_Id)
   is
      connection_element1, connection_element2 : Connection_Element;
      Name : Name_Id := connection.beginning_port;
      Name2 : Name_Id := connection.ending_port;
      Increment                                : Integer := 2;
   begin

      if Hash_connection.Contains (Beginning_List, connection.ending_port)
        and then Hash_Name.Contains (End_List, connection.beginning_port)
      then

         connection_element1.Parent :=
           Hash_Name.Element (End_List, connection.beginning_port);
         while Hash_connection.Contains (Beginning_List, Name) loop
            Name :=
              Ocarina.Backends.python.Nutils.Add_Suffix_To_Name
                (Integer'Image (Increment),
                 connection.beginning_port);
            Increment := Increment + 1;
         end loop;

         connection_element2 :=
           Hash_connection.Element
             (Beginning_List,
              connection_element1.Parent);
         Name_List.Push (connection_element2.Children, Name);
         Hash_connection.Replace
           (Beginning_List,
            connection_element1.Parent,
            connection_element2);

         Increment := 2;
         while Hash_connection.Contains (Beginning_List, Name2) loop
            Name_List.Push (connection_element1.Children, Name2);

            connection_element2 :=
              Hash_connection.Element (Beginning_List, Name2);
            connection_element2.Parent := Name;
            Hash_connection.Replace
              (Beginning_List,
               Name2,
               connection_element2);

            Name2 :=
              Ocarina.Backends.python.Nutils.Add_Suffix_To_Name
                (Integer'Image (Increment),
                 connection.ending_port);
            Increment := Increment + 1;
         end loop;

         connection_element1.Source_Name      := Source_Name;
         connection_element1.Source_Type      := Source_Type;
         connection_element1.Destination_Name := Destination_Name;
         connection_element1.Destination_Type := Destination_Type;
         Hash_connection.Insert (Beginning_List, Name, connection_element1);
         Hash_Name.Insert (End_List, connection.ending_port, Name);

      elsif not Hash_connection.Contains
          (Beginning_List,
           connection.ending_port)
        and then Hash_Name.Contains (End_List, connection.beginning_port)
      then

         connection_element1.Parent :=
           Hash_Name.Element (End_List, connection.beginning_port);
         while Hash_connection.Contains (Beginning_List, Name) loop
            Name :=
              Ocarina.Backends.python.Nutils.Add_Suffix_To_Name
                (Integer'Image (Increment),
                 connection.beginning_port);
            Increment := Increment + 1;
         end loop;

         connection_element2 :=
           Hash_connection.Element
             (Beginning_List,
              connection_element1.Parent);
         Name_List.Push (connection_element2.Children, Name);
         Hash_connection.Replace
           (Beginning_List,
            connection_element1.Parent,
            connection_element2);

         connection_element1.Source_Name      := Source_Name;
         connection_element1.Source_Type      := Source_Type;
         connection_element1.Destination_Name := Destination_Name;
         connection_element1.Destination_Type := Destination_Type;
         Hash_connection.Insert (Beginning_List, Name, connection_element1);
         Hash_Name.Insert (End_List, connection.ending_port, Name);

      elsif Hash_connection.Contains (Beginning_List, connection.ending_port)
        and then not Hash_Name.Contains (End_List, connection.beginning_port)
      then

         connection_element1.Parent := Get_String_Name ("none");
         while Hash_connection.Contains (Beginning_List, Name) loop
            Name :=
              Ocarina.Backends.python.Nutils.Add_Suffix_To_Name
                (Integer'Image (Increment),
                 connection.beginning_port);
            Increment := Increment + 1;
         end loop;

         Increment := 2;
         while Hash_connection.Contains (Beginning_List, Name2) loop
            Name_List.Push (connection_element1.Children, Name2);

            connection_element2 :=
              Hash_connection.Element (Beginning_List, Name2);
            connection_element2.Parent := Name;
            Hash_connection.Replace
              (Beginning_List,
               Name2,
               connection_element2);

            Name2 :=
              Ocarina.Backends.python.Nutils.Add_Suffix_To_Name
                (Integer'Image (Increment),
                 connection.ending_port);
            Increment := Increment + 1;
         end loop;

         connection_element1.Source_Name      := Source_Name;
         connection_element1.Source_Type      := Source_Type;
         connection_element1.Destination_Name := Destination_Name;
         connection_element1.Destination_Type := Destination_Type;
         Hash_connection.Insert (Beginning_List, Name, connection_element1);
         Hash_Name.Insert (End_List, connection.ending_port, Name);

      else

         while Hash_connection.Contains (Beginning_List, Name) loop
            Name :=
              Ocarina.Backends.python.Nutils.Add_Suffix_To_Name
                (Integer'Image (Increment),
                 connection.beginning_port);
            Increment := Increment + 1;
         end loop;

         connection_element1.Source_Name      := Source_Name;
         connection_element1.Source_Type      := Source_Type;
         connection_element1.Destination_Name := Destination_Name;
         connection_element1.Destination_Type := Destination_Type;
         connection_element1.Parent           := Get_String_Name ("none");

         Hash_connection.Insert (Beginning_List, Name, connection_element1);
         Hash_Name.Insert (End_List, connection.ending_port, Name);

      end if;
   end Connect_port;

   --------------------
   -- Visit_Children --
   --------------------

   function Visit_Children (Name : Name_Id) return Name_List.Fifo_Type is
      Element : Connection_Element :=
        Hash_connection.Element (Beginning_List, Name);
      List      : Name_List.Fifo_Type;
      Res, temp : Name_List.Fifo_Type;
      tempName  : Name_Id;
   begin
      if Name_List.Is_Empty (Element.Children) then
         Name_List.Push (Res, Name);
      end if;
      while not Name_List.Is_Empty (Element.Children) loop
         Name_List.Pop (Element.Children, tempName);
         Name_List.Push (List, tempName);
         temp := Visit_Children (tempName);
         while not Name_List.Is_Empty (temp) loop
            Name_List.Pop (temp, tempName);
            Name_List.Push (Res, tempName);
         end loop;
      end loop;
      Element.Children := List;
      return Res;
   end Visit_Children;

   ------------------
   -- Visit_Parent --
   ------------------

   function Visit_Parent (Name : Name_Id) return Name_Id is
      Element : Connection_Element :=
        Hash_connection.Element (Beginning_List, Name);
   begin
      if Get_Name_String (Element.Parent) = "none" then
         return Name;
      else
         return Visit_parent (Element.Parent);
      end if;
   end Visit_Parent;

end Ocarina.Backends.SpaceStudio_properties;
