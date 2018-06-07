------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . I N S T A N C E S . P R O P E R T I E S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Instances.Components;
with Ocarina.Instances.Components.Subprogram_Calls;
with Ocarina.Instances.Components.Modes;
with Ocarina.Instances.Messages;
with Ocarina.Instances.Finder;

package body Ocarina.Instances.Properties is

   use Locations;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;

   use Ocarina.Instances.Components;
   use Ocarina.Instances.Components.Subprogram_Calls;
   use Ocarina.Instances.Components.Modes;
   use Ocarina.Instances.Messages;
   use Ocarina.Instances.Finder;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package AIE renames Ocarina.ME_AADL.AADL_Instances.Entities;

   function Duplicate_Property_Value
     (Instance_Root                  : Node_Id;
      Property_Value                 : Node_Id;
      Instance                       : Node_Id;
      Former_Property_Instance_Value : Node_Id := No_Node) return Node_Id;

   function Instantiate_Property_Value
     (Instance_Root  : Node_Id;
      Property_Value : Node_Id;
      Instance       : Node_Id) return Node_Id;

   function Instantiate_Reference_Term
     (Instance_Root      : Node_Id;
      Reference_Instance : Node_Id;
      Reference_Term     : Node_Id) return Node_Id;
   pragma Unreferenced (Instantiate_Reference_Term);
   --  Return an equivalent of Path, but containing instances instead
   --  of declarations.

   ----------------------
   -- Apply_Properties --
   ----------------------

   function Apply_Properties
     (Instance_Root : Node_Id;
      Instance      : Node_Id;
      Property_List : List_Id;
      Override_Mode : Boolean) return Boolean
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Present (Instance));

      Property_List_Node : Node_Id;
      Success            : Boolean := True;
   begin
      --  We then apply the attached properties

      if Property_List /= No_List then
         Property_List_Node := ATN.First_Node (Property_List);

         while Present (Property_List_Node) loop
            Success :=
              Add_Property_Instance
                (Instance_Root,
                 Instance,
                 Property_List_Node,
                 Override_Mode)
              and then Success;

            Property_List_Node := ATN.Next_Node (Property_List_Node);
         end loop;
      end if;

      return Success;
   end Apply_Properties;

   ---------------------------
   -- Add_Property_Instance --
   ---------------------------

   function Add_Property_Instance
     (Instance_Root        : Node_Id;
      Entity_Instance      : Node_Id;
      Property_Association : Node_Id;
      Override_Mode        : Boolean) return Boolean
   is
      pragma Assert
        (Kind (Entity_Instance) = K_Component_Instance
         or else Kind (Entity_Instance) = K_Port_Spec_Instance
         or else Kind (Entity_Instance) = K_Feature_Group_Spec_Instance
         or else Kind (Entity_Instance) = K_Parameter_Instance
         or else Kind (Entity_Instance) = K_Subcomponent_Access_Instance
         or else Kind (Entity_Instance) = K_Subcomponent_Instance
         or else Kind (Entity_Instance) = K_Subprogram_Spec_Instance
         or else Kind (Entity_Instance) = K_Connection_Instance
         or else Kind (Entity_Instance) = K_Mode_Instance);

      Property_Instance : Node_Id;
      Pointed_Instance  : Node_Id;
      Success           : Boolean := True;

   begin
      --  We first find the instance on which the property applies

      Pointed_Instance :=
        Find_Instance
          (Instance_Root,
           Entity_Instance,
           ATN.Applies_To_Prop (Property_Association));

      if Present (Pointed_Instance) then
         if Ocarina.ME_AADL.AADL_Instances.Nodes.Properties
             (Pointed_Instance) =
           No_List
         then
            AIN.Set_Properties
              (Pointed_Instance,
               AINU.New_List (K_List_Id, No_Location));
         end if;

         --  We then check if the property is already set in the pointed
         --  instance

         Property_Instance :=
           Get_First_Homonym_Instance
             (AIN.Properties (Pointed_Instance),
              Property_Association);

         if Present (Property_Instance)
           and then ATN.Is_Additive_Association (Property_Association)
         then
            --  Append the current value to the old value list

            AIN.Set_Property_Association_Value
              (Property_Instance,
               Duplicate_Property_Value
                 (Instance_Root,
                  ATN.Property_Association_Value (Property_Association),
                  Entity_Instance,
                  AIN.Property_Association_Value (Property_Instance)));

         else
            Property_Instance :=
              AINU.New_Node
                (K_Property_Association_Instance,
                 ATN.Loc (Property_Association));
            AIN.Set_Identifier
              (Property_Instance,
               AIE.Duplicate_Identifier
                 (ATN.Identifier (Property_Association)));
            AIN.Set_Corresponding_Entity
              (AIN.Identifier (Property_Instance),
               Property_Instance);
            AIN.Set_Corresponding_Declaration
              (Property_Instance,
               Property_Association);
            AIN.Set_Property_Name
              (Property_Instance,
               ATN.Property_Name (Property_Association));
            AIN.Set_Is_Additive_Association
              (Property_Instance,
               ATN.Is_Additive_Association (Property_Association));
            AIN.Set_Is_Constant
              (Property_Instance,
               ATN.Is_Constant (Property_Association));
            AIN.Set_Is_Private
              (Property_Instance,
               ATN.Is_Private (Property_Association));
            AIN.Set_Is_Access
              (Property_Instance,
               ATN.Is_Access (Property_Association));
            AIN.Set_Applies_To_Prop (Property_Instance, No_List);
            AIN.Set_In_Binding
              (Property_Instance,
               ATN.In_Binding (Property_Association));

            --  Instantiate property "in modes". The fact that a property
            --  association of a mode has an "in modes" clause, has
            --  absolutely no sense. It should have been detected as
            --  error during analysis.

            declare
               Container_Component : Node_Id := No_Node;
            begin
               --  The modes belong to the container component
               --  implementation.

               case AIN.Kind (Entity_Instance) is
                  when K_Component_Instance =>
                     Container_Component := Entity_Instance;

                  when K_Port_Spec_Instance        |
                    K_Feature_Group_Spec_Instance  |
                    K_Parameter_Instance           |
                    K_Subcomponent_Access_Instance |
                    K_Subprogram_Spec_Instance     |
                    K_Connection_Instance          =>
                     Container_Component := Parent_Component (Entity_Instance);

                  when others =>
                     null;
               end case;

               --  Workaround to fetch the in_modes of the original
               --  property association since there is no
               --  property_association_instance node kind.

               AIN.Set_In_Modes
                 (Property_Instance,
                  ATN.In_Modes (Property_Association));

               if Present (Container_Component) then
                  Instantiate_In_Modes
                    (Container_Component,
                     Property_Instance);
               end if;
            end;

            if Present
                (ATN.Property_Association_Value (Property_Association))
            then
               AIN.Set_Property_Association_Value
                 (Property_Instance,
                  Duplicate_Property_Value
                    (Instance_Root,
                     ATN.Property_Association_Value (Property_Association),
                     Entity_Instance));

               --  IMPORTANT: we use entity instance where the property
               --  has been declared, as the reference node to
               --  duplicate the property, and not pointed_instance,
               --  which is the node on which the property
               --  applies. Indeed, for properties that are
               --  references, the reference path is set from
               --  entity_instance.
            end if;

            if Override_Mode then
               --  Append the node to the BEGINNING of the property list
               --  of the instance so that properties that are declared
               --  for subcomponents override the properties that are
               --  declared for the corresponding components.

               --  XXX looks strange: why relying on order ? should
               --  _remove_ the property from the list instead

               AINU.Push_Node_To_List
                 (Property_Instance,
                  AIN.Properties (Pointed_Instance));

            else
               AINU.Append_Node_To_List
                 (Property_Instance,
                  AIN.Properties (Pointed_Instance));
            end if;
         end if;
      else
         Display_Instantiation_Error (Property_Association);
         Success := False;
      end if;

      return Success;
   end Add_Property_Instance;

   -------------------------------
   -- Replace_Property_Instance --
   -------------------------------

   function Replace_Property_Instance
     (Instance_Root        : Node_Id;
      Entity_Instance      : Node_Id;
      Property_Association : Node_Id;
      Override_Mode        : Boolean) return Boolean
   is
      pragma Assert
        (Kind (Entity_Instance) = K_Component_Instance
         or else Kind (Entity_Instance) = K_Port_Spec_Instance
         or else Kind (Entity_Instance) = K_Feature_Group_Spec_Instance
         or else Kind (Entity_Instance) = K_Parameter_Instance
         or else Kind (Entity_Instance) = K_Subcomponent_Access_Instance
         or else Kind (Entity_Instance) = K_Subprogram_Spec_Instance
         or else Kind (Entity_Instance) = K_Connection_Instance
         or else Kind (Entity_Instance) = K_Mode_Instance);

      pragma Assert
        (AIN.Kind (Property_Association) = K_Property_Association_Instance);

      Property_Instance : Node_Id;
      Pointed_Instance  : Node_Id;
      Success           : Boolean := True;
   begin
      --  We first find the instance on which the property applies

      Pointed_Instance :=
        Find_Instance_In_Instance
          (Instance_Root,
           Entity_Instance,
           AIN.Applies_To_Prop (Property_Association));

      if Present (Pointed_Instance) then
         if Ocarina.ME_AADL.AADL_Instances.Nodes.Properties
             (Pointed_Instance) =
           No_List
         then
            AIN.Set_Properties
              (Pointed_Instance,
               AINU.New_List (K_List_Id, No_Location));
         end if;

         --  We then check if the property is already set in the pointed
         --  instance

         Property_Instance :=
           Get_First_Homonym_Instance
             (AIN.Properties (Pointed_Instance),
              AIE.Get_Name_Of_Entity (Property_Association, False));

         if Present (Property_Instance)
           and then AIN.Is_Additive_Association (Property_Association)
         then
            --  Append the current value to the old value list

            AIN.Set_Property_Association_Value
              (Property_Instance,
               Duplicate_Property_Value
                 (Instance_Root,
                  AIN.Property_Association_Value (Property_Association),
                  Entity_Instance,
                  AIN.Property_Association_Value (Property_Instance)));

         else
            Property_Instance :=
              AINU.New_Node
                (K_Property_Association_Instance,
                 ATN.Loc (Property_Association));
            --  XXX TODO CRAP Must Duplicate_Identifier
            AIN.Set_Identifier
              (Property_Instance,
               AIN.Identifier (Property_Association));

            AIN.Set_Corresponding_Entity
              (AIN.Identifier (Property_Instance),
               Property_Instance);
            AIN.Set_Corresponding_Declaration
              (Property_Instance,
               Property_Association);
            AIN.Set_Property_Name
              (Property_Instance,
               AIN.Property_Name (Property_Association));
            AIN.Set_Is_Additive_Association
              (Property_Instance,
               AIN.Is_Additive_Association (Property_Association));
            AIN.Set_Is_Constant
              (Property_Instance,
               AIN.Is_Constant (Property_Association));
            AIN.Set_Is_Private
              (Property_Instance,
               AIN.Is_Private (Property_Association));
            AIN.Set_Is_Access
              (Property_Instance,
               AIN.Is_Access (Property_Association));
            AIN.Set_Applies_To_Prop (Property_Instance, No_List);
            AIN.Set_In_Binding
              (Property_Instance,
               AIN.In_Binding (Property_Association));

            --  Instantiate property "in modes". The fact that a property
            --  association of a mode has an "in modes" clause, has
            --  absolutely no sense. It should have been detected as
            --  error during analysis.

            declare
               Container_Component : Node_Id := No_Node;
            begin
               --  The modes belong to the container component
               --  implementation.

               case AIN.Kind (Entity_Instance) is
                  when K_Component_Instance =>
                     Container_Component := Entity_Instance;

                  when K_Port_Spec_Instance        |
                    K_Feature_Group_Spec_Instance  |
                    K_Parameter_Instance           |
                    K_Subcomponent_Access_Instance |
                    K_Subprogram_Spec_Instance     |
                    K_Connection_Instance          =>
                     Container_Component := Parent_Component (Entity_Instance);

                  when others =>
                     null;
               end case;

               --  Workaround to fetch the in_modes of the original
               --  property association scince there is no
               --  property_association_instance node kind.

               AIN.Set_In_Modes
                 (Property_Instance,
                  AIN.In_Modes (Property_Association));

               if Present (Container_Component) then
                  Instantiate_In_Modes
                    (Container_Component,
                     Property_Instance);
               end if;
            end;

            if Present
                (AIN.Property_Association_Value (Property_Association))
            then
               AIN.Set_Property_Association_Value
                 (Property_Instance,
                  Duplicate_Property_Value
                    (Instance_Root,
                     AIN.Property_Association_Value (Property_Association),
                     Entity_Instance));

               --  IMPORTANT: we use entity instance where the
               --  property has been declared, as the reference node
               --  to duplicate the property, and not
               --  pointed_instance, which is the node on which the
               --  property applies. Indeed, for properties that are
               --  references, the reference path is set from
               --  entity_instance.
            end if;

            if Override_Mode then
               --  Append the node to the BEGINNING of the property list
               --  of the instance so that properties that are declared
               --  for subcomponents override the properties that are
               --  declared for the corresponding components.

               AINU.Push_Node_To_List
                 (Property_Instance,
                  AIN.Properties (Pointed_Instance));
            else
               AINU.Append_Node_To_List
                 (Property_Instance,
                  AIN.Properties (Pointed_Instance));
            end if;
         end if;
      else
         Display_Instantiation_Error (Property_Association);
         Success := False;
      end if;

      return Success;
   end Replace_Property_Instance;

   ------------------------------
   -- Duplicate_Property_Value --
   ------------------------------

   function Duplicate_Property_Value
     (Instance_Root                  : Node_Id;
      Property_Value                 : Node_Id;
      Instance                       : Node_Id;
      Former_Property_Instance_Value : Node_Id := No_Node) return Node_Id
   is
      pragma Assert
        (No (Property_Value) or else Kind (Property_Value) = K_Property_Value);
      pragma Assert
        (No (Former_Property_Instance_Value)
         or else No (Single_Value (Former_Property_Instance_Value)));
      --  If we are extending a former property value, this one must
      --  be a list.

      Duplicated_Property_Value : Node_Id;
      List_Node                 : Node_Id;
   begin
      if No (Former_Property_Instance_Value) then
         if No (Property_Value) then
            Duplicated_Property_Value := No_Node;

         else
            Duplicated_Property_Value :=
              New_Node (K_Property_Value, ATN.Loc (Property_Value));

            if Present (Single_Value (Property_Value)) then
               --  We set the value calculated at analysis time

               Set_Single_Value
                 (Duplicated_Property_Value,
                  Instantiate_Property_Value
                    (Instance_Root,
                     Expanded_Single_Value (Property_Value),
                     Instance));
               Set_Expanded_Single_Value
                 (Duplicated_Property_Value,
                  Single_Value (Duplicated_Property_Value));
            end if;

            if Multi_Value (Property_Value) /= No_List then
               --  We set the value calculated at analysis time

               Set_Multi_Value
                 (Duplicated_Property_Value,
                  New_List
                    (K_List_Id,
                     ATN.Loc (Node_Id (Multi_Value (Property_Value)))));
               Set_Expanded_Multi_Value
                 (Duplicated_Property_Value,
                  Multi_Value (Duplicated_Property_Value));

               if Present (Expanded_Multi_Value (Property_Value)) then
                  --  XXX It appears that for record properties,
                  --  Expanded_Multi_Value is set to No_Node. An
                  --  inspection of the declarative tree reveals this
                  --  seems OK. To be investigated further

                  List_Node :=
                    ATN.First_Node (Expanded_Multi_Value (Property_Value));
               else
                  List_Node :=
                    ATN.First_Node (Multi_Value (Property_Value));
               end if;

               while Present (List_Node) loop
                  Append_Node_To_List
                    (Instantiate_Property_Value
                       (Instance_Root,
                        List_Node,
                        Instance),
                     Multi_Value (Duplicated_Property_Value));
                  List_Node := ATN.Next_Node (List_Node);
               end loop;

            end if;
         end if;

      else
         --  We first duplicate the values of the former property
         --  association.

         Duplicated_Property_Value :=
           New_Node (K_Property_Value, ATN.Loc (Property_Value));

         Set_Multi_Value
           (Duplicated_Property_Value,
            New_List
              (K_List_Id,
               ATN.Loc (Node_Id (Multi_Value (Property_Value)))));
         Set_Expanded_Multi_Value
           (Duplicated_Property_Value,
            Multi_Value (Duplicated_Property_Value));

         List_Node :=
           ATN.First_Node (Multi_Value (Former_Property_Instance_Value));

         while Present (List_Node) loop
            Append_Node_To_List
              (Instantiate_Property_Value (Instance_Root, List_Node, Instance),
               Multi_Value (Duplicated_Property_Value));
            List_Node := ATN.Next_Node (List_Node);
         end loop;

         --  Then, we append the new values

         if Present (Single_Value (Property_Value)) then
            Append_Node_To_List
              (Instantiate_Property_Value
                 (Instance_Root,
                  Single_Value (Property_Value),
                  Instance),
               Multi_Value (Duplicated_Property_Value));

         elsif Multi_Value (Property_Value) /= No_List then
            List_Node := ATN.First_Node (Multi_Value (Property_Value));

            while Present (List_Node) loop
               Append_Node_To_List
                 (Instantiate_Property_Value
                    (Instance_Root,
                     List_Node,
                     Instance),
                  Multi_Value (Duplicated_Property_Value));
               List_Node := ATN.Next_Node (List_Node);
            end loop;
         end if;
      end if;

      Set_Value_Container (Duplicated_Property_Value, Instance);
      --  The container of the property is the instance in which it is
      --  declared. This way, we know in what context we have to
      --  evaluate its value.

      return Duplicated_Property_Value;
   end Duplicate_Property_Value;

   --------------------------------
   -- Instantiate_Property_Value --
   --------------------------------

   function Instantiate_Property_Value
     (Instance_Root  : Node_Id;
      Property_Value : Node_Id;
      Instance       : Node_Id) return Node_Id
   is
      pragma Assert
        (No (Property_Value)
           or else Kind (Property_Value) = K_Literal
           or else Kind (Property_Value) = K_Property_Term
           or else Kind (Property_Value) = K_Enumeration_Term
           or else Kind (Property_Value) = K_Number_Range_Term
           or else Kind (Property_Value) = K_Reference_Term
           or else Kind (Property_Value) = K_Minus_Numeric_Term
           or else Kind (Property_Value) = K_Signed_AADLNumber
           or else Kind (Property_Value) = K_Not_Boolean_Term
           or else Kind (Property_Value) = K_And_Boolean_Term
           or else Kind (Property_Value) = K_Or_Boolean_Term
           or else Kind (Property_Value) = K_Parenthesis_Boolean_Term
           or else Kind (Property_Value) = K_Component_Classifier_Term
           or else Kind (Property_Value) = K_Record_Term);
      pragma Assert (Present (Instance));

      Instantiated_Value : Node_Id := No_Node;
      List_Node          : Node_Id := No_Node;
      Node               : Node_Id;

   begin
      if No (Property_Value) then
         return No_Node;
      end if;

      case ATN.Kind (Property_Value) is
         when K_Literal =>
            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            Set_Value (Instantiated_Value, Value (Property_Value));

         when K_Property_Term | K_Enumeration_Term =>
            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            ATN.Set_Identifier
              (Instantiated_Value,
               ATE.Duplicate_Identifier (ATN.Identifier (Property_Value)));
            Set_Entity (Instantiated_Value, Entity (Property_Value));
            Set_Property_Set_Identifier
              (Instantiated_Value,
               Property_Set_Identifier (Property_Value));

         when K_Number_Range_Term =>
            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            Set_Lower_Bound (Instantiated_Value, Lower_Bound (Property_Value));
            Set_Upper_Bound (Instantiated_Value, Upper_Bound (Property_Value));
            Set_Delta_Term (Instantiated_Value, Delta_Term (Property_Value));

         when K_Reference_Term =>
            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            Node :=
              New_Node
                (Kind (Reference_Term (Property_Value)),
                 ATN.Loc (Reference_Term (Property_Value)));

            case AADL_Version is
               when AADL_V1 =>
                  --  Node is an Entity_Reference

                  ATN.Set_Path
                    (Node,
                     ATN.Path (Reference_Term (Property_Value)));
                  ATN.Set_Identifier
                    (Node,
                     ATE.Duplicate_Identifier
                       (ATN.Identifier (Reference_Term (Property_Value))));

               when AADL_V2 =>
                  --  Node is a Contained_Element_Path

                  Set_List_Items
                    (Node,
                     New_List
                       (K_List_Id,
                        ATN.Loc
                          (Node_Id
                             (List_Items (Reference_Term (Property_Value))))));

                  if List_Items (Reference_Term (Property_Value)) /=
                    No_List
                  then
                     List_Node :=
                       ATN.First_Node
                         (List_Items (Reference_Term (Property_Value)));

                     while Present (List_Node) loop
                        Append_Node_To_List
                          (ATE.Duplicate_Identifier (List_Node),
                           List_Items (Node));
                        List_Node := ATN.Next_Node (List_Node);
                     end loop;
                  end if;

                  --  XXX TODO INSTANCIATE ANNEX_PATH NODE
            end case;

            Set_Reference_Term (Instantiated_Value, Node);

         when K_Minus_Numeric_Term =>
            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            Set_Numeric_Term
              (Instantiated_Value,
               Numeric_Term (Property_Value));

         when K_Signed_AADLNumber =>
            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            Set_Number_Value
              (Instantiated_Value,
               Number_Value (Property_Value));
            Set_Unit_Identifier
              (Instantiated_Value,
               Unit_Identifier (Property_Value));

         when K_And_Boolean_Term | K_Or_Boolean_Term =>
            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            Set_First_Term
              (Instantiated_Value,
               Instantiate_Property_Value
                 (Instance_Root,
                  First_Term (Property_Value),
                  Instance));
            Set_Second_Term
              (Instantiated_Value,
               Instantiate_Property_Value
                 (Instance_Root,
                  Second_Term (Property_Value),
                  Instance));

         when K_Not_Boolean_Term | K_Parenthesis_Boolean_Term =>
            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            Set_Boolean_Term
              (Instantiated_Value,
               Instantiate_Property_Value
                 (Instance_Root,
                  Boolean_Term (Property_Value),
                  Instance));

         when K_Component_Classifier_Term =>
            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            Set_Referenced_Entity
              (Instantiated_Value,
               Instantiate_Component
                 (Instance_Root,
                  ATE.Get_Referenced_Entity (Property_Value)));

            Set_Component_Cat
              (Instantiated_Value,
               Component_Cat (Property_Value));

         when K_Record_Term =>
            --  Simply propagate the list to the instance

            Instantiated_Value :=
              New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
            Set_List_Items
              (Instantiated_Value,
               ATN.List_Items (Property_Value));

         when others =>
            raise Program_Error;
      end case;

      return Instantiated_Value;
   end Instantiate_Property_Value;

   --------------------------------
   -- Instantiate_Reference_Term --
   --------------------------------

   function Instantiate_Reference_Term
     (Instance_Root      : Node_Id;
      Reference_Instance : Node_Id;
      Reference_Term     : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Present (Reference_Instance));
      pragma Assert
        (Kind (Reference_Instance) = K_Component_Instance
         or else Kind (Reference_Instance) = K_Subcomponent_Instance);

      List_Node              : Node_Id;
      Identifier_Of_Instance : Node_Id;
      Pointed_Instance       : Node_Id;
      New_Reference_Term     : constant Node_Id :=
        New_Node (K_Reference_Term, ATN.Loc (Reference_Term));
      Initial_Path : constant List_Id := ATN.Path (Reference_Term);
   begin
      --  The instantiation of a reference path returns a path that leads
      --  to the component instance, while the reference path itself
      --  was likely to point to a subcomponent or a subprogram,
      --  etc. which was associated to a component declaration.

      if Initial_Path = No_List then
         ATN.Set_Path (New_Reference_Term, No_List);
      else
         if Kind (Reference_Instance) = K_Component_Instance then
            Pointed_Instance := Reference_Instance;
         else
            Pointed_Instance := Corresponding_Instance (Reference_Instance);
         end if;

         List_Node := ATN.First_Node (Initial_Path);

         while Present (List_Node) loop
            pragma Assert (ATN.Kind (ATN.Item (List_Node)) = K_Identifier);

            Pointed_Instance :=
              Find_Local_Instance (Pointed_Instance, ATN.Item (List_Node));

            if No (Pointed_Instance) then
               ATN.Set_Path (New_Reference_Term, No_List);
               exit;

            elsif Kind (Pointed_Instance) = K_Call_Instance then
               Pointed_Instance := Corresponding_Instance (Pointed_Instance);
               Identifier_Of_Instance :=
                 AIE.Duplicate_Identifier (AIN.Identifier (Pointed_Instance));
               AIE.Add_Path_Element_To_Entity_Reference
                 (New_Reference_Term,
                  Identifier_Of_Instance);
               Pointed_Instance :=
                 Duplicate_Subprogram_Call_Instance
                   (Instance_Root,
                    Pointed_Instance);

            elsif Kind (Pointed_Instance) = K_Subcomponent_Instance then
               Pointed_Instance := Corresponding_Instance (Pointed_Instance);
               Identifier_Of_Instance :=
                 AIE.Duplicate_Identifier (AIN.Identifier (Pointed_Instance));
               AIE.Add_Path_Element_To_Entity_Reference
                 (New_Reference_Term,
                  Identifier_Of_Instance);
            end if;

            List_Node := ATN.Next_Node (List_Node);
         end loop;
      end if;

      return New_Reference_Term;
   end Instantiate_Reference_Term;

end Ocarina.Instances.Properties;
