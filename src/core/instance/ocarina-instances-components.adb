------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . I N S T A N C E S . C O M P O N E N T S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Locations;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Annotations;
with Ocarina.Instances.Components.Subcomponents;
with Ocarina.Instances.Components.Subprogram_Calls;
with Ocarina.Instances.Components.Features;
with Ocarina.Instances.Components.Connections;
with Ocarina.Instances.Components.Modes;
with Ocarina.Instances.Namespaces;
with Ocarina.Instances.Properties;
with Ocarina.Instances.Messages;

package body Ocarina.Instances.Components is

   use Locations;

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   use Ocarina.Annotations;
   use Ocarina.Instances.Components.Subcomponents;
   use Ocarina.Instances.Components.Subprogram_Calls;
   use Ocarina.Instances.Components.Features;
   use Ocarina.Instances.Components.Connections;
   use Ocarina.Instances.Components.Modes;
   use Ocarina.Instances.Namespaces;
   use Ocarina.Instances.Properties;
   use Ocarina.Instances.Messages;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Entities;

   procedure Add_Component_Instance (Component : Node_Id; Instance : Node_Id);
   --  Reference a component instance that has been instantiateed from
   --  Component.

   ----------------------------
   -- Add_Component_Instance --
   ----------------------------

   procedure Add_Component_Instance
     (Component : Node_Id;
      Instance  : Node_Id)
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type);
      pragma Assert (Kind (Instance) = K_Component_Instance);
   begin
      if Is_Empty (ATN.Instances (Component)) then
         Set_Instances (Component, New_List (K_List_Id, ATN.Loc (Component)));
      end if;

      Append_Node_To_List (Instance, ATN.Instances (Component));
   end Add_Component_Instance;

   ---------------------------
   -- Instantiate_Component --
   ---------------------------

   function Instantiate_Component
     (Instance_Root     : Node_Id;
      Component         : Node_Id;
      Existing_Instance : Node_Id := No_Node) return Node_Id
   is
      pragma Assert (AIN.Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert
        (ATN.Kind (Component) = K_Component_Implementation
         or else ATN.Kind (Component) = K_Component_Type);

      New_Instance       : Node_Id := Existing_Instance;
      List_Node          : Node_Id;
      Instance_Node      : Node_Id;
      Success            : Boolean := True;
      Namespace_Instance : Node_Id;

   begin
      --  Getting the component namespace

      Namespace_Instance :=
        Instantiate_Namespace (Instance_Root, ATN.Namespace (Component));

      --  Annotate the component with itself

      Annotate (Component, Component);

      if No (New_Instance) then
         New_Instance := New_Node (K_Component_Instance, ATN.Loc (Component));
         Add_Component_Instance (Component, New_Instance);
         AIN.Set_Corresponding_Declaration (New_Instance, Component);
         AIN.Set_Identifier
           (New_Instance,
            AINU.Duplicate_Identifier (ATN.Identifier (Component)));
         AIN.Set_Corresponding_Entity
           (AIN.Identifier (New_Instance),
            New_Instance);
         AIN.Set_Namespace (New_Instance, Namespace_Instance);
      end if;

      --  Mark the component as being instantiated. It is important
      --  that we mark the component as instantiated before computing
      --  its subcomponent to avoid endless recursion. We mark only if
      --  this is the top level component during instantiation
      --  (Existing_Instance is nul).

      if No (Existing_Instance) then
         Set_Instance (Component, New_Instance);
      end if;

      --  Strategy for component instatiation that is compliant with
      --  the AADL overriding rules:

      --  1 - For component types:

      --  (a) we instantiate the features of the component

      --  (b) we instantiate recursively the parents of the component

      --  (c) we apply the component properties properties

      --  We chose this order because applying properties may require
      --  visibility on features that are inherited form other
      --  parents.

      --  2 - For component implementations:

      --  (a) we instantiate the corresponding component type

      --  (b) we instantiate the subcomponents

      --  (c) we instantiate the call sequences

      --  (d) we instantiate the refined features

      --  (e) we instantiate recusively the parent implementations of
      --  the component

      --  (f) we instantiate the connections

      --  (g) we instantiate the modes and mode transitions

      --  (h) we apply the properties

      --  We chose this order because instantiating connections, modes
      --  and applying properties may require visibility on subclauses
      --  that are inherited form other parents.

      if No (Existing_Instance)
        and then Kind (Component) = K_Component_Implementation
      then
         --  If we are dealing with a component implementation, then
         --  we have to instantiate the corresponding component type.

         declare
            Component_Type : constant Node_Id :=
              ATN.Corresponding_Entity (Component_Type_Identifier (Component));

         begin
            --  Annotate the component type node with the
            --  component implementation node.

            Annotate (Component_Type, Component);

            --  Instantiate the component type

            New_Instance :=
              Instantiate_Component
                (Instance_Root,
                 Component_Type,
                 New_Instance);
         end;
      end if;

      --  Instantiate the component itself

      if Kind (Component) = K_Component_Implementation then
         --  Subcomponents

         if not ATNU.Is_Empty (ATN.Subcomponents (Component)) then
            if AIN.Subcomponents (New_Instance) = No_List then
               AIN.Set_Subcomponents
                 (New_Instance,
                  New_List (K_List_Id, No_Location));
            end if;

            List_Node := ATN.First_Node (ATN.Subcomponents (Component));

            while Present (List_Node) loop
               if No
                   (Get_First_Homonym_Instance
                      (AIN.Subcomponents (New_Instance),
                       List_Node))
               then
                  --  We do not re-instantiate subcomponent refinements

                  Instance_Node :=
                    Instantiate_Subcomponent (Instance_Root, List_Node);

                  if Present (Instance_Node) then
                     --  Annotate the corresponding component of the
                     --  subcomponent with the current AADL component
                     --  implementation.

                     if Present (Entity_Ref (List_Node)) then
                        Annotate
                          (ATE.Get_Referenced_Entity (Entity_Ref (List_Node)),
                           Component);
                     end if;

                     --  Set the component instance and the
                     --  subcomponent instance.

                     Set_Parent_Component (Instance_Node, New_Instance);

                     Append_Node_To_List
                       (Instance_Node,
                        AIN.Subcomponents (New_Instance));

                     --  We apply the properties to the component
                     --  corresponding to the subcomponent.

                     if Present (Entity_Ref (List_Node)) then
                        Success :=
                          Apply_Properties
                          (Instance_Root,
                           Corresponding_Instance (Instance_Node),
                           ATN.Properties (List_Node),
                           Override_Mode => True)
                          and then Success;
                     end if;
                  else
                     Display_Instantiation_Error (List_Node);
                     Success := False;
                  end if;
               end if;

               List_Node := ATN.Next_Node (List_Node);
            end loop;
         end if;

         --  Subprogram call sequences

         if not ATNU.Is_Empty (ATN.Calls (Component)) then
            if AIN.Calls (New_Instance) = No_List then
               AIN.Set_Calls (New_Instance, New_List (K_List_Id, No_Location));
            end if;

            List_Node := ATN.First_Node (ATN.Calls (Component));

            while Present (List_Node) loop
               if Get_First_Homonym_Instance
                   (AIN.Calls (New_Instance),
                    List_Node) =
                 No_Node
               then
                  Instance_Node :=
                    Instantiate_Call_Sequence (Instance_Root, List_Node);

                  if Present (Instance_Node) then
                     Set_Parent_Component (Instance_Node, New_Instance);
                     Append_Node_To_List
                       (Instance_Node,
                        AIN.Calls (New_Instance));
                  else
                     Display_Instantiation_Error (List_Node);
                     Success := False;
                  end if;
               end if;

               List_Node := ATN.Next_Node (List_Node);
            end loop;
         end if;

         --  Refined features

         if Refines_Type (Component) /= No_List then
            if AIN.Features (New_Instance) = No_List then
               AIN.Set_Features
                 (New_Instance,
                  New_List (K_List_Id, No_Location));
            end if;

            List_Node := ATN.First_Node (ATN.Refines_Type (Component));

            while Present (List_Node) loop
               if No
                   (Get_First_Homonym (AIN.Features (New_Instance), List_Node))
               then
                  Instance_Node :=
                    Instantiate_Feature
                      (Instance_Root,
                       List_Node,
                       New_Instance);

                  if Present (Instance_Node) then
                     Success :=
                       Apply_Properties
                         (Instance_Root,
                          Instance_Node,
                          ATN.Properties (List_Node),
                          Override_Mode => True)
                       and then Success;

                     Append_Node_To_List
                       (Instance_Node,
                        AIN.Features (New_Instance));
                  else
                     Display_Instantiation_Error (List_Node);
                     Success := False;
                  end if;
               end if;

               List_Node := ATN.Next_Node (List_Node);
            end loop;
         end if;
      else
         --  This is a component type

         if ATN.Features (Component) /= No_List then
            if AIN.Features (New_Instance) = No_List then
               AIN.Set_Features
                 (New_Instance,
                  New_List (K_List_Id, No_Location));
            end if;

            List_Node := ATN.First_Node (ATN.Features (Component));

            while Present (List_Node) loop
               if No
                   (Get_First_Homonym_Instance
                      (AIN.Features (New_Instance),
                       List_Node))
               then
                  Instance_Node :=
                    Instantiate_Feature
                      (Instance_Root,
                       List_Node,
                       New_Instance);

                  if Present (Instance_Node) then
                     Success :=
                       Apply_Properties
                         (Instance_Root,
                          Instance_Node,
                          ATN.Properties (List_Node),
                          Override_Mode => True)
                       and then Success;

                     Append_Node_To_List
                       (Instance_Node,
                        AIN.Features (New_Instance));

                     Set_Parent_Component (Instance_Node, New_Instance);
                  else
                     Display_Instantiation_Error (List_Node);
                     Success := False;
                  end if;
               else
                  --  If we are in this case, then we are a
                  --  instantiating a parent feature that has been
                  --  refined in the child component. We just apply
                  --  the properties in a NON OVERRIDING way.

                  --  We just override the properties

                  Success :=
                    Apply_Properties
                      (Instance_Root,
                       Get_First_Homonym_Instance
                         (AIN.Features (New_Instance),
                          List_Node),
                       ATN.Properties (List_Node),
                       Override_Mode => False)
                    and then Success;

               end if;

               List_Node := ATN.Next_Node (List_Node);
            end loop;
         end if;
      end if;

      --  Recursively instantiate the parent components implementations and
      --  types.

      if Present (Parent (Component)) then
         declare
            The_Parent : constant Node_Id :=
              ATE.Get_Referenced_Entity (Parent (Component));

         begin
            --  Annotate the parent component with the current
            --  component.

            Annotate (The_Parent, Component);

            --  Instantiate the parent component

            New_Instance :=
              Instantiate_Component (Instance_Root, The_Parent, New_Instance);
         end;
      end if;

      --  If there was a problem during the instantiation, we immediately
      --  exit. This case should normally not happen, though.

      if No (New_Instance) then
         Display_Instantiation_Error (Component);
         return No_Node;
      end if;

      if Kind (Component) = K_Component_Implementation then
         --  Connections

         if not ATNU.Is_Empty (ATN.Connections (Component)) then
            if AIN.Connections (New_Instance) = No_List then
               AIN.Set_Connections
                 (New_Instance,
                  New_List (K_List_Id, No_Location));
            end if;

            List_Node := ATN.First_Node (ATN.Connections (Component));

            while Present (List_Node) loop
               if No
                   (Get_First_Homonym_Instance
                      (AIN.Connections (New_Instance),
                       List_Node))
               then
                  Instance_Node :=
                    Instantiate_Connection
                      (Instance_Root,
                       New_Instance,
                       List_Node);

                  if Present (Instance_Node) then
                     Append_Node_To_List
                       (Instance_Node,
                        AIN.Connections (New_Instance));
                  else
                     Display_Instantiation_Error (List_Node);
                     Success := False;
                  end if;
               end if;

               List_Node := ATN.Next_Node (List_Node);
            end loop;
         end if;

         --  Modes

         if not ATNU.Is_Empty (ATN.Modes (Component)) then
            if AIN.Modes (New_Instance) = No_List then
               AIN.Set_Modes (New_Instance, New_List (K_List_Id, No_Location));
            end if;

            if AIN.Mode_transitions (New_Instance) = No_List then
               Set_Mode_transitions
                 (New_Instance,
                  New_List (K_List_Id, No_Location));
            end if;

            --  We must instantiate all modes before any mode
            --  transition since the mode transition instantiation
            --  uses the instantiated modes.

            List_Node := ATN.First_Node (ATN.Modes (Component));

            while Present (List_Node) loop
               if ATN.Kind (List_Node) = K_Mode then
                  Instance_Node :=
                    Instantiate_Mode (Instance_Root, New_Instance, List_Node);

                  --  Apply the properties to the instantiated mode

                  if Present (Instance_Node) then
                     Success :=
                       Apply_Properties
                         (Instance_Root,
                          Instance_Node,
                          ATN.Properties (List_Node),
                          Override_Mode => True)
                       and then Success;
                  end if;

                  if Present (Instance_Node) then
                     Append_Node_To_List
                       (Instance_Node,
                        AIN.Modes (New_Instance));
                  else
                     Display_Instantiation_Error (List_Node);
                     Success := False;
                  end if;
               end if;

               List_Node := ATN.Next_Node (List_Node);
            end loop;

            --  Mode transitions

            List_Node := ATN.First_Node (ATN.Modes (Component));

            while Present (List_Node) loop
               if Kind (List_Node) = K_Mode_Transition then
                  Instance_Node :=
                    Instantiate_Mode_Transition
                      (Instance_Root,
                       New_Instance,
                       List_Node);

                  if Present (Instance_Node) then
                     Append_Node_To_List
                       (Instance_Node,
                        AIN.Mode_transitions (New_Instance));
                  else
                     Display_Instantiation_Error (Component);
                     Success := False;
                  end if;
               end if;

               List_Node := ATN.Next_Node (List_Node);
            end loop;
         end if;

         --  At this point, we have all necessary elements to solve
         --  the "in_modes" clauses of subcomponents, call sequences,
         --  connections. "in modes" of property association is done
         --  when instantiateing the properties because it is a little
         --  bit more complicated.

         Instantiate_In_Modes (New_Instance, AIN.Subcomponents (New_Instance));
         Instantiate_In_Modes (New_Instance, AIN.Calls (New_Instance));
         Instantiate_In_Modes (New_Instance, AIN.Connections (New_Instance));
      end if;

      --  Property associations of the component instance

      Success :=
        Apply_Properties
          (Instance_Root,
           New_Instance,
           ATN.Properties (Component),
           Override_Mode => True)
        and then Success;

      if not Success then
         Display_Instantiation_Error (Component);
      elsif No (Default_Instance (Component)) then
         Set_Default_Instance (Component, New_Instance);
      end if;

      return New_Instance;
   end Instantiate_Component;

end Ocarina.Instances.Components;
