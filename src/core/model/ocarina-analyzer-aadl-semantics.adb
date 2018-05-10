------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . A N A L Y Z E R . A A D L . S E M A N T I C S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

with Errors;
with Ocarina.Namet;

with Ocarina.AADL_Values;

with Ocarina.Analyzer.Messages;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.Analyzer.AADL.Queries;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

with Ocarina.Processor.Properties;

package body Ocarina.Analyzer.AADL.Semantics is

   use Errors;
   use Ocarina.Namet;

   use Ocarina.AADL_Values;
   use Ocarina.Analyzer.Messages;
   use Ocarina.Analyzer.AADL.Queries;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
   use Ocarina.Processor.Properties;

   function Check_Classifier_Matching_Rule
     (Source_Type      : Node_Id;
      Destination_Type : Node_Id) return Boolean;

   function Check_Cycles_In_Component_Implementation
     (Node         : Node_Id;
      Initial_Node : Node_Id := No_Node) return Boolean;

   function Check_Cycles_In_Port_Group_Or_Component_Type
     (Node         : Node_Id;
      Initial_Node : Node_Id := No_Node) return Boolean;

   function Check_Cycles_In_Inversions_Of_Port_Groups
     (Node         : Node_Id;
      Initial_Node : Node_Id := No_Node) return Boolean;

   function Check_For_A_Unique_Initial_Mode (Node : Node_Id) return Boolean;

   function Check_Cycles_In_Subcomponents
     (Node         : Node_Id;
      Initial_Node : Node_Id := No_Node) return Boolean;

   function Check_Connections (Node : Node_Id) return Boolean;

   procedure Reset_Connections (Node : Node_Id);

   function Connection_End_Is_Local (Node : Node_Id) return Boolean;
   --  Return true iff Node is local to a given component, i.e. does
   --  not belong to another entity.

   function Check_End_Types_Consistency (Node : Node_Id) return Boolean;
   --  Check that the end of the connection have compatible types

   function Check_Connection_Ends_Consistency (Node : Node_Id) return Boolean;
   --  Check if that the source is an in port, the destination is an
   --  out port, etc.

   function Check_Connection_End_Consistency
     (Connection_End      : Node_Id;
      Connection_Category : Connection_Type) return Boolean;
   --  Check if the connection end (source or destination) is of a
   --  consistent type regarding the connection type.

   function Check_End_Directions_Consistency (Node : Node_Id) return Boolean;
   --  Check that the connected entities are consistent with the
   --  direction of the connection.

   function Check_Property_Associations
     (Properties : List_Id;
      Container  : Node_Id) return Boolean;
   --  Return True if the value type of the property association
   --  Property is consistent with the one specified in the property
   --  name declaration. Container is the entity declaration in which
   --  the property association is declared

   function Check_Applies_To
     (Property  : Node_Id;
      Container : Node_Id) return Boolean;
   --  Return True if the property association can be applied to the
   --  container or to the entity designated by the 'applies to'
   --  statement, if any.

   function Check_Values_Of_Property_Association
     (Property_Association : Node_Id) return Boolean;
   --  Check wether the values of the property association are
   --  conformant with the type associated with the corresponding
   --  property name.

   function Check_Properties_Of_Component_Type
     (Component : Node_Id) return Boolean;

   function Check_Properties_Of_Component_Implementation
     (Component : Node_Id) return Boolean;

   function Check_Properties_Of_Port_Group_Type
     (Port_Group : Node_Id) return Boolean;

   function Check_Property_Type
     (Property_Type         : Node_Id;
      Display_Error_Message : Boolean := True) return Boolean;
   --  Return True if the property type is consistent, else False

   function Compare_Numbers
     (Number_1 : Node_Id;
      Number_2 : Node_Id) return Integer;
   --  Return -1 if Number_1 > Number_2, 1 if Number_2 > Number_1, or
   --  0 if Number_1 and Number_2 are equal. Return -2 if there is an
   --  error. If Number_1 and Number_2 are two unit number, the
   --  comparision is done with respect to the units (1sec is greater
   --  that 500ms).

   procedure Homogenize_Unit_Numbers
     (Number_1  :     Node_Id;
      Number_2  :     Node_Id;
      Literal_1 : out Node_Id;
      Literal_2 : out Node_Id);
   --  If Number_1 and Number_2 are to unit literals, convert them to
   --  the first common unit to be able to compare them. For example
   --  if Number_1 is 1sec and Number_2 is 500ms then Literal_1 is set
   --  to 1000 and Literal_2 is set to 500.

   function Convert_Single_Value_To_List
     (Property_Association : Node_Id) return Boolean;
   --  Edit the value of the property association in order to create a
   --  list with its single value

   function Test_Property_Type_Equivalence
     (Type_Of_Property_Name : Ocarina.ME_AADL.AADL_Tree.Entities.Properties
        .Property_Type;
      Type_Of_Property_Association : Ocarina.ME_AADL.AADL_Tree.Entities
        .Properties
        .Property_Type)
      return Boolean;

   function Test_Property_Value_Validity
     (Property_Type  : Node_Id;
      Property_Value : Node_Id) return Boolean;

   ------------------------------------
   -- Check_Classifier_Matching_Rule --
   ------------------------------------

   function Check_Classifier_Matching_Rule
     (Source_Type      : Node_Id;
      Destination_Type : Node_Id) return Boolean
   is
   begin
      --  This function implements a check for the classifier_matching
      --  rule
      --
      --  For now, we only implement the following:
      --
      --  "Classifier_Match:" The source data type and data
      --  implementation must be identical to the data type or data
      --  implementation of the destination. If the destination
      --  classifier is a component type, then any implementation of
      --  the source matches. This is the default rule.

      if Source_Type = Destination_Type then
         --  a) strict equality

         return True;

      elsif Kind (Source_Type) = K_Component_Implementation
        and then
          Corresponding_Entity (Component_Type_Identifier (Source_Type)) =
          Destination_Type
      then
         --  b) source is an implementation of the destination
         return True;
      end if;

      return False;
   end Check_Classifier_Matching_Rule;

   --------------------------------
   -- Check_Qualified_References --
   --------------------------------

   function Check_Qualified_References
     (Container           : Node_Id;
      Qualified_Reference : Node_Id) return Boolean
   is
      use Ocarina.Analyzer.AADL.Finder;

      pragma Assert
        (Kind (Container) = K_Package_Specification
         or else Kind (Container) = K_Component_Type
         or else Kind (Container) = K_Component_Implementation
         or else Kind (Container) = K_Feature_Group_Type
         or else Kind (Container) = K_Subcomponent
         or else Kind (Container) = K_Subcomponent_Access
         or else Kind (Container) = K_Port_Spec
         or else Kind (Container) = K_Parameter
         or else Kind (Container) = K_Connection);

      pragma Assert
        (Kind (Qualified_Reference) = K_Entity_Reference
         or else Kind (Qualified_Reference) = K_Identifier);

      Pack_Container : Node_Id := No_Node;
      Success        : Boolean := False;
   begin
      if Kind (Container) = K_Component_Type
        or else Kind (Container) = K_Component_Implementation
        or else Kind (Container) = K_Feature_Group_Type
      then
         Pack_Container := Namespace (Container);

      elsif Kind (Container) = K_Subcomponent
        or else Kind (Container) = K_Port_Spec
        or else Kind (Container) = K_Parameter
        or else Kind (Container) = K_Feature_Group_Spec
        or else Kind (Container) = K_Subcomponent_Access
        or else Kind (Container) = K_Connection
      then
         Pack_Container := Namespace (Container_Component (Container));

      elsif Kind (Container) = K_Package_Specification then
         Pack_Container := Container;
      end if;

      if Kind (Qualified_Reference) = K_Entity_Reference
        and then
          Name (Identifier (Pack_Container)) =
          Name (Namespace_Identifier (Qualified_Reference))
      then
         Success := True;

      elsif Kind (Qualified_Reference) = K_Identifier
        and then
          Name (Identifier (Pack_Container)) =
          Name (Qualified_Reference)
      then
         Success := True;
      else
         Success :=
           Find_In_Import_Declaration (Pack_Container, Qualified_Reference);
      end if;

      if Success = False then
         Display_Analyzer_Error
           (Qualified_Reference,
            "qualified reference name not found in 'with' statements of ",
            Pack_Container);
      end if;

      return Success;
   end Check_Qualified_References;

   ----------------------
   -- Check_Applies_To --
   ----------------------

   function Check_Applies_To
     (Property  : Node_Id;
      Container : Node_Id) return Boolean
   is
      pragma Assert (Kind (Property) = K_Property_Association);
      pragma Assert (Present (Container));

      Pointed_Node       : Node_Id;
      Entity_Of_Property : Node_Id;
      Success            : Boolean;
   begin
      if Applies_To_Prop (Property) = No_List then
         Entity_Of_Property := Container;
      else
         Pointed_Node :=
           First_Node (List_Items (First_Node (Applies_To_Prop (Property))));

         if Kind (Pointed_Node) = K_Array_Selection then
            Entity_Of_Property :=
              Corresponding_Entity (Identifier (Pointed_Node));
         else
            Entity_Of_Property := Corresponding_Entity (Pointed_Node);
            --  XXX Here we must make this verification OK for all contained
            --  element path in 'applies' to list
         end if;
      end if;

      if Kind (Entity_Of_Property) = K_Package_Specification then
         Success := True;
      else
         Success :=
           Property_Can_Apply_To_Entity (Property, Entity_Of_Property);
      end if;

      if not Success then
         Display_Property_Not_Applicable (Property, Entity_Of_Property);
      end if;
      return Success;
   end Check_Applies_To;

   ----------------------------------------------
   -- Check_Cycles_In_Component_Implementation --
   ----------------------------------------------

   function Check_Cycles_In_Component_Implementation
     (Node         : Node_Id;
      Initial_Node : Node_Id := No_Node) return Boolean
   is
      pragma Assert (Kind (Node) = K_Component_Implementation);
      pragma Assert
        (No (Initial_Node)
         or else Kind (Initial_Node) = K_Component_Implementation);

      First_Extension_Node : Node_Id;
      Success              : Boolean := True;
   begin
      --  We note the first visited node in each component we
      --  scan. Thus, if we scan a component in which we find the same
      --  node_id, it means there is a cycle.

      if No (Initial_Node) then
         First_Extension_Node := Node;
      else
         First_Extension_Node := Initial_Node;
      end if;

      if First_Visited_Node (Node) = First_Extension_Node then
         Display_Cyclic_Extension (Node);
         Set_First_Visited_Node (Node, No_Node);
         return False;
      else
         Set_First_Visited_Node (Node, First_Extension_Node);
      end if;

      if Parent (Node) /= No_Node
        and then Get_Referenced_Entity (Parent (Node)) /= No_Node
      then
         Success :=
           Check_Cycles_In_Component_Implementation
             (Get_Referenced_Entity (Parent (Node)),
              First_Extension_Node);
      else
         Success :=
           Check_Cycles_In_Port_Group_Or_Component_Type
             (Corresponding_Entity (Component_Type_Identifier (Node)));
      end if;

      Set_First_Visited_Node (Node, No_Node);
      return Success;
   end Check_Cycles_In_Component_Implementation;

   -----------------------------------------------
   -- Check_Cycles_In_Inversions_Of_Port_Groups --
   -----------------------------------------------

   function Check_Cycles_In_Inversions_Of_Port_Groups
     (Node         : Node_Id;
      Initial_Node : Node_Id := No_Node) return Boolean
   is
      pragma Assert (Kind (Node) = K_Feature_Group_Type);
      pragma Assert
        (No (Initial_Node) or else Kind (Initial_Node) = K_Feature_Group_Type);

      First_Inversion_Node : Node_Id;
      Success              : Boolean := True;
   begin
      --  We note the first visited node in each component we
      --  scan. Thus, if we scan a component in which we find the same
      --  node id, it means there is a cycle.

      if No (Initial_Node) then
         First_Inversion_Node := Node;
      else
         First_Inversion_Node := Initial_Node;
      end if;

      if First_Visited_Node (Node) = First_Inversion_Node then
         Display_Cyclic_Inversion (Node);
         Set_First_Visited_Node (Node, No_Node);
         return False;
      else
         Set_First_Visited_Node (Node, First_Inversion_Node);
      end if;

      if Inverse_Of (Node) /= No_Node
        and then Get_Referenced_Entity (Inverse_Of (Node)) /= No_Node
      then
         Success :=
           Check_Cycles_In_Inversions_Of_Port_Groups
             (Get_Referenced_Entity (Inverse_Of (Node)),
              First_Inversion_Node);
      end if;

      Set_First_Visited_Node (Node, No_Node);
      return Success;
   end Check_Cycles_In_Inversions_Of_Port_Groups;

   --------------------------------------------------
   -- Check_Cycles_In_Port_Group_Or_Component_Type --
   --------------------------------------------------

   function Check_Cycles_In_Port_Group_Or_Component_Type
     (Node         : Node_Id;
      Initial_Node : Node_Id := No_Node) return Boolean
   is
      pragma Assert
        (Kind (Node) = K_Component_Type
         or else Kind (Node) = K_Feature_Group_Type);
      pragma Assert
        (No (Initial_Node)
         or else Kind (Initial_Node) = K_Component_Type
         or else Kind (Initial_Node) = K_Feature_Group_Type);

      First_Extension_Node : Node_Id;
      Success              : Boolean := True;
   begin
      --  We note the first visited node in each component we
      --  scan. Thus, if we scan a component in which we find the same
      --  node id, it means there is a cycle.

      if No (Initial_Node) then
         First_Extension_Node := Node;
      else
         First_Extension_Node := Initial_Node;
      end if;

      if First_Visited_Node (Node) = First_Extension_Node then
         Display_Cyclic_Extension (Node);
         Set_First_Visited_Node (Node, No_Node);
         return False;
      else
         Set_First_Visited_Node (Node, First_Extension_Node);
      end if;

      if Parent (Node) /= No_Node
        and then Get_Referenced_Entity (Parent (Node)) /= No_Node
      then
         Success :=
           Check_Cycles_In_Port_Group_Or_Component_Type
             (Get_Referenced_Entity (Parent (Node)),
              First_Extension_Node);
      end if;

      Set_First_Visited_Node (Node, No_Node);
      return Success;
   end Check_Cycles_In_Port_Group_Or_Component_Type;

   -----------------------------------
   -- Check_Cycles_In_Subcomponents --
   -----------------------------------

   function Check_Cycles_In_Subcomponents
     (Node         : Node_Id;
      Initial_Node : Node_Id := No_Node) return Boolean
   is
      pragma Assert
        (Kind (Node) = K_Component_Implementation
         or else Kind (Node) = K_Component_Type);

      List_Node           : Node_Id;
      First_Instance_Node : Node_Id;
      Success             : Boolean := True;
   begin
      if Kind (Node) = K_Component_Implementation then
         if No (Initial_Node) then
            First_Instance_Node := Node;
         else
            First_Instance_Node := Initial_Node;
         end if;

         if First_Visited_Node (Node) = First_Instance_Node then
            Display_Cyclic_Subcomponents (Node);
            Set_First_Visited_Node (Node, No_Node);
            return False;
         else
            Set_First_Visited_Node (Node, First_Instance_Node);
         end if;

         if not Is_Empty (Subcomponents (Node)) then
            List_Node := First_Node (Subcomponents (Node));

            while Present (List_Node) loop
               if Entity_Ref (List_Node) /= No_Node
                 and then
                   Get_Referenced_Entity (Entity_Ref (List_Node)) /=
                   No_Node
               then
                  Success :=
                    Success
                    and then Check_Cycles_In_Subcomponents
                      (Get_Referenced_Entity (Entity_Ref (List_Node)),
                       First_Instance_Node);
               end if;

               List_Node := Next_Node (List_Node);
            end loop;
         end if;

         Set_First_Visited_Node (Node, No_Node);
      end if;

      return Success;
   end Check_Cycles_In_Subcomponents;

   ----------------------
   -- Check_Connection --
   ----------------------

   function Check_Connection (Node : Node_Id) return Boolean is
      pragma Assert (Kind (Node) = K_Connection);

      Success : Boolean := True;
   begin
      if Is_Refinement (Node) then
         return True;
      end if;

      if not Check_Connection_End_Consistency
          (Connection_End      => Get_Referenced_Entity (Destination (Node)),
           Connection_Category => Connection_Type'Val (Category (Node)))
      then
         DAE
           (Node1    => Destination (Node),
            Message1 => " points to ",
            Node2    => Get_Referenced_Entity (Destination (Node)),
            Message2 => ", which is not of a proper type");
         Success := False;
      end if;

      if not Check_Connection_End_Consistency
          (Connection_End      => Get_Referenced_Entity (Source (Node)),
           Connection_Category => Connection_Type'Val (Category (Node)))
      then
         DAE
           (Node1    => Source (Node),
            Message1 => " points to ",
            Node2    => Get_Referenced_Entity (Source (Node)),
            Message2 => ", which is not of a proper type");
         Success := False;
      end if;

      if Success then
         Success :=
           Check_End_Types_Consistency (Node)
           and then Check_Connection_Ends_Consistency (Node)
           and then Check_End_Directions_Consistency (Node);
      end if;

      return Success;
   end Check_Connection;

   ---------------------------------------
   -- Check_Connection_Ends_Consistency --
   ---------------------------------------

   function Check_Connection_Ends_Consistency
     (Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Node) = K_Connection);

      Connection_Source : constant Node_Id :=
        Get_Referenced_Entity (Source (Node));
      Connection_Destination : constant Node_Id :=
        Get_Referenced_Entity (Destination (Node));

      Same_Kind                                : Boolean := False;
      Data_Port_And_Parameter                  : Boolean := False;
      Data_And_Require_Data_Access             : Boolean := False;
      Provide_Data_Access_And_Data             : Boolean := False;
      Provide_Bus_Access_And_Bus               : Boolean := False;
      Bus_And_Require_Bus_Access               : Boolean := False;
      Provide_Subprogram_Access_And_Subprogram : Boolean := False;
      Subprogram_And_Require_Subprogram_Access : Boolean := False;
   begin
      pragma Assert
        (Kind (Connection_Source) = K_Port_Spec
         or else Kind (Connection_Source) = K_Parameter
         or else Kind (Connection_Source) = K_Feature_Group_Spec
         or else Kind (Connection_Source) = K_Subcomponent_Access
         or else Kind (Connection_Source) = K_Subcomponent
         or else Kind (Connection_Destination) = K_Subprogram_Call);

      pragma Assert
        (Kind (Connection_Destination) = K_Port_Spec
         or else Kind (Connection_Destination) = K_Parameter
         or else Kind (Connection_Destination) = K_Feature_Group_Spec
         or else Kind (Connection_Destination) = K_Subcomponent_Access
         or else Kind (Connection_Destination) = K_Subcomponent
         or else Kind (Connection_Destination) = K_Subprogram_Call);

      Same_Kind := Kind (Connection_Source) = Kind (Connection_Destination);

      Data_Port_And_Parameter :=
        (Kind (Connection_Source) = K_Port_Spec
         and then Is_Data (Connection_Source)
         and then Kind (Connection_Destination) = K_Parameter)
        or else
        (Kind (Connection_Destination) = K_Port_Spec
         and then Is_Data (Connection_Destination)
         and then Kind (Connection_Source) = K_Parameter);

      Data_And_Require_Data_Access :=
        Kind (Connection_Source) = K_Subcomponent
        and then
          Component_Category'Val (Category (Connection_Source)) =
          CC_Data
        and then Kind (Connection_Destination) = K_Subcomponent_Access
        and then
          Component_Category'Val
            (Subcomponent_Category (Connection_Destination)) =
          CC_Data;

      Provide_Data_Access_And_Data :=
        (Kind (Connection_Destination) = K_Subcomponent_Access
         and then
           Component_Category'Val
             (Subcomponent_Category (Connection_Destination)) =
           CC_Data
         and then Is_Provided (Connection_Destination)
         and then Kind (Connection_Source) = K_Subcomponent
         and then
           Component_Category'Val (Category (Connection_Source)) =
           CC_Data);

      Bus_And_Require_Bus_Access :=
        (Kind (Connection_Source) = K_Subcomponent
         and then
           Component_Category'Val (Category (Connection_Source)) =
           CC_Bus
         and then Kind (Connection_Destination) = K_Subcomponent_Access
         and then
           Component_Category'Val
             (Subcomponent_Category (Connection_Destination)) =
           CC_Bus)
        or else
        (Kind (Connection_Source) = K_Subcomponent_Access
         and then
           Component_Category'Val (Subcomponent_Category (Connection_Source)) =
           CC_Bus
         and then Kind (Connection_Destination) = K_Subcomponent
         and then
           Component_Category'Val (Category (Connection_Destination)) =
           CC_Bus);

      Provide_Bus_Access_And_Bus :=
        (Kind (Connection_Destination) = K_Subcomponent_Access
         and then
           Component_Category'Val
             (Subcomponent_Category (Connection_Destination)) =
           CC_Bus
         and then Is_Provided (Connection_Destination)
         and then Kind (Connection_Source) = K_Subcomponent
         and then
           Component_Category'Val (Category (Connection_Source)) =
           CC_Bus);

      Subprogram_And_Require_Subprogram_Access :=
        Kind (Connection_Source) = K_Subcomponent
        and then
          Component_Category'Val (Category (Connection_Source)) =
          CC_Subprogram
        and then Kind (Connection_Destination) = K_Subcomponent_Access
        and then
          Component_Category'Val
            (Subcomponent_Category (Connection_Destination)) =
          CC_Subprogram;

      Provide_Subprogram_Access_And_Subprogram :=
        (Kind (Connection_Destination) = K_Subcomponent_Access
         and then
           Component_Category'Val
             (Subcomponent_Category (Connection_Destination)) =
           CC_Subprogram
         and then Is_Provided (Connection_Destination)
         and then Kind (Connection_Source) = K_Subcomponent
         and then
           Component_Category'Val (Category (Connection_Source)) =
           CC_Subprogram);

      --  We assume that the only possibility is to connect a
      --  subcomponent into a component requires. The contrary
      --  (connecting a component requires into a subcomponent) is
      --  forbidden.

      if not
        (Bus_And_Require_Bus_Access
         or else Provide_Bus_Access_And_Bus
         or else Provide_Data_Access_And_Data
         or else Data_And_Require_Data_Access
         or else Subprogram_And_Require_Subprogram_Access
         or else Provide_Subprogram_Access_And_Subprogram
         or else Data_Port_And_Parameter
         or else Same_Kind)
      then
         DAE
           (Loc      => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Node),
            Node1    => Get_Referenced_Entity (Source (Node)),
            Message1 => " and ",
            Node2    => Get_Referenced_Entity (Destination (Node)),
            Message2 => " are not compatible");
         return False;
      else
         return True;
      end if;
   end Check_Connection_Ends_Consistency;

   --------------------------------------
   -- Check_Connection_End_Consistency --
   --------------------------------------

   function Check_Connection_End_Consistency
     (Connection_End      : Node_Id;
      Connection_Category : Connection_Type) return Boolean
   is
      pragma Assert
        (Kind (Connection_End) = K_Port_Spec
         or else Kind (Connection_End) = K_Parameter
         or else Kind (Connection_End) = K_Feature_Group_Spec
         or else Kind (Connection_End) = K_Subcomponent_Access
         or else Kind (Connection_End) = K_Subcomponent
         or else Kind (Connection_End) = K_Subprogram_Call);

      Success : Boolean := True;
   begin
      case Connection_Category is
         when CT_Error =>
            Success := False;

         when CT_Port_Connection      |
           CT_Feature                 |
           CT_Access_Subprogram_Group |
           CT_Access_Virtual_Bus      |
           CT_Access                  =>
            Success := True;
         --  XXX Incomplete TODO

         when CT_Data | CT_Data_Delayed =>
            Success :=
              Kind (Connection_End) = K_Port_Spec
              and then Is_Data (Connection_End)
              and then not Is_Event (Connection_End);

         when CT_Event =>
            Success :=
              Kind (Connection_End) = K_Port_Spec
              and then not Is_Data (Connection_End)
              and then Is_Event (Connection_End);

         when CT_Event_Data =>
            Success :=
              Kind (Connection_End) = K_Port_Spec
              and then Is_Data (Connection_End)
              and then Is_Event (Connection_End);

         when CT_Feature_Group =>
            Success := Kind (Connection_End) = K_Feature_Group_Spec;

         when CT_Parameter =>
            --  Parameter connections can connect (event) data ports
            --  to subprogram parameters, since parameter have the
            --  same semantic as (event) data ports.

            Success :=
              Kind (Connection_End) = K_Parameter
              or else
              (Kind (Connection_End) = K_Port_Spec
               and then Is_Data (Connection_End));

         when CT_Access_Bus =>
            Success :=
              (Kind (Connection_End) = K_Subcomponent_Access
               and then
                 Component_Category'Val
                   (Subcomponent_Category (Connection_End)) =
                 CC_Bus)
              or else
              (Kind (Connection_End) = K_Subcomponent
               and then
                 Component_Category'Val (Category (Connection_End)) =
                 CC_Bus);

         when CT_Access_Data =>
            Success :=
              (Kind (Connection_End) = K_Subcomponent_Access
               and then
                 Component_Category'Val
                   (Subcomponent_Category (Connection_End)) =
                 CC_Data)
              or else
              (Kind (Connection_End) = K_Subcomponent
               and then
                 Component_Category'Val (Category (Connection_End)) =
                 CC_Data);

         when CT_Access_Subprogram =>
            Success :=
              (Kind (Connection_End) = K_Subcomponent_Access
               and then
                 Component_Category'Val
                   (Subcomponent_Category (Connection_End)) =
                 CC_Subprogram)
              or else
              (Kind (Connection_End) = K_Subcomponent
               and then
                 Component_Category'Val (Category (Connection_End)) =
                 CC_Subprogram);
      end case;

      return Success;
   end Check_Connection_End_Consistency;

   -----------------------
   -- Check_Connections --
   -----------------------

   function Check_Connections (Node : Node_Id) return Boolean is

      pragma Assert (Kind (Node) = K_Component_Implementation);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node));

         while Present (List_Node) loop
            Success   := Check_Connection (List_Node) and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Check whether there are duplicate connections

      if Success
        and then not Is_Empty
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node))
      then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node));

         while Present (List_Node) loop
            declare
               Src    : constant Node_Id := Source (List_Node);
               Dst    : constant Node_Id := Destination (List_Node);
               N      : Node_Id;
               I_Name : Name_Id;
               Info   : Nat;
            begin
               --  We use name_buffer hash codes for efficiency. First
               --  we inster the following string : Node%Path
               --  (Src)%Path (Dst)%dup%cnx%check and then we compute
               --  its byte info. If we find a non-zero info, this
               --  means the connection is duplicate and the info
               --  poits to the first met connection. Finally, we set
               --  the info of the string to point to the current
               --  connection.

               Set_Nat_To_Name_Buffer (Nat (Node));
               Add_Char_To_Name_Buffer ('%');

               N := First_Node (Path (Src));
               while Present (N) loop
                  Add_Nat_To_Name_Buffer
                    (Nat (Corresponding_Entity (Item (N))));
                  Add_Char_To_Name_Buffer ('%');

                  N := Next_Node (N);
               end loop;

               N := First_Node (Path (Dst));
               while Present (N) loop
                  Add_Nat_To_Name_Buffer
                    (Nat (Corresponding_Entity (Item (N))));
                  Add_Char_To_Name_Buffer ('%');

                  N := Next_Node (N);
               end loop;

               Add_Str_To_Name_Buffer ("dup%cnx%check");
               I_Name := Name_Find;
               Info   := Get_Name_Table_Info (I_Name);

               if Info /= 0 then
                  --  Check whether the two connections have common
                  --  modes.

                  if Have_Common_Statements
                      (In_Modes (List_Node),
                       In_Modes (Node_Id (Info)))
                  then
                     Error_Loc (1) := Loc (List_Node);
                     Error_Loc (2) := Loc (Node_Id (Info));

                     DE ("This connection is a duplication of the" &
                        " connection declared!");

                     Success := False;
                  end if;
               else
                  Set_Name_Table_Info (I_Name, Nat (List_Node));
               end if;
            end;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Check_Connections;

   --------------------------------------
   -- Check_End_Directions_Consistency --
   --------------------------------------

   function Check_End_Directions_Consistency (Node : Node_Id) return Boolean is

      pragma Assert (Kind (Node) = K_Connection);

      Connection_Source : constant Node_Id :=
        Get_Referenced_Entity (Source (Node));
      Source_Is_Local : constant Boolean :=
        Connection_End_Is_Local (Source (Node));
      Connection_Destination : constant Node_Id :=
        Get_Referenced_Entity (Destination (Node));
      Destination_Is_Local : constant Boolean :=
        Connection_End_Is_Local (Destination (Node));

      Directions : Boolean := False;
   begin
      pragma Assert
        (Kind (Connection_Source) = K_Port_Spec
         or else Kind (Connection_Source) = K_Parameter
         or else Kind (Connection_Source) = K_Feature_Group_Spec
         or else Kind (Connection_Source) = K_Subcomponent_Access
         or else Kind (Connection_Source) = K_Subcomponent);

      case Kind (Connection_Destination) is
         when K_Port_Spec | K_Parameter =>
            if
              (Present (Inversed_Entity (Connection_Source))
                 and then
                 Connection_Source /=
                 Inversed_Entity (Connection_Source))
              or else
              (Present (Inversed_Entity (Connection_Destination))
                 and then
                 Connection_Destination /=
                 Inversed_Entity (Connection_Destination))
            then
               --  XXX to be refined
               Directions := True;
            else
               Directions :=
                 ((not Source_Is_Local)
                    and then (not Destination_Is_Local)
                    and then Is_Out (Connection_Source)
                    and then Is_In (Connection_Destination))
                 or else
                 (Source_Is_Local
                    and then (not Destination_Is_Local)
                    and then Is_In (Connection_Source)
                    and then Is_In (Connection_Destination))
                 or else
                 ((not Source_Is_Local)
                    and then Destination_Is_Local
                    and then Is_Out (Connection_Source)
                    and then Is_Out (Connection_Destination))
                 or else
                 (Source_Is_Local
                    and then Destination_Is_Local
                    and then Is_In (Connection_Source)
                    and then Is_Out (Connection_Destination))
                 or else
                 (Is_In (Connection_Source)
                    and then Is_Out (Connection_Source)
                    and then Is_Out (Connection_Destination)
                    and then Is_In (Connection_Destination));
               --  XXX The latest test may be redudant with the previous
               --  ones
            end if;

         when K_Feature_Group_Spec =>
            Directions := True;
            --  There is no direction for a port group

         when K_Subcomponent_Access | K_Subcomponent =>
            Directions := Is_Bidirectional (Node)

              or else
              --  AS5506/B 9.4 (L5) : if the access connection
              --  declaration represents an access connection between
              --  access features of sibling components, then the source
              --  must be a provides access, and the destination must be
              --  a requires access, or vice versa.

              (not Source_Is_Local and then
                 not Destination_Is_Local and then
                 not (Kind (Connection_Destination) = K_Subcomponent_Access
                        and then Is_Provided (Connection_Destination))
                 and then
                 Kind (Connection_Source) = K_Subcomponent_Access and then
                 Is_Provided (Connection_Source))

              or else
              --  AS5506/B 9.4 (L6): If the access connection
              --  declaration represents a feature mapping up the
              --  containment hierarchy, then one connection end must be
              --  a provides access of a subcomponent, or a data,
              --  subprogram, or bus subcomponent; and the other
              --  connection end must be a provides access feature of
              --  the enclosing component or a provides feature of a
              --  feature group of the enclosing component.

              (not Source_Is_Local
                 and then Destination_Is_Local
                 and then
                 (Kind (Connection_Destination) = K_Subcomponent_Access
                    and then Is_Provided (Connection_Destination))
                 and then Kind (Connection_Source) = K_Subcomponent_Access
                 and then Is_Provided (Connection_Source))

              or else
              (Source_Is_Local
                 and then Destination_Is_Local
                 and then
                 (Kind (Connection_Destination) = K_Subcomponent_Access
                    and then Is_Provided (Connection_Destination))
                 and then Kind (Connection_Source) = K_Subcomponent)

              or else
              --  AS5506/B 9.4 (L7): If the access connection
              --  declaration represents a feature mapping down the
              --  containment hierarchy, then one connection end must be
              --  a requires access of the enclosing component, a
              --  requires access of a feature group of the enclosing
              --  component, or a data, subprogram, or bus subcomponent;
              --  and the other connection end must be a requires access
              --  of a subcomponent.

              (((Source_Is_Local and then not Destination_Is_Local)
                  or else (not Source_Is_Local and then Destination_Is_Local))
               and then
                 ((Kind (Connection_Destination) = K_Subcomponent_Access
                     and then not Is_Provided (Connection_Destination))
                  or else Kind (Connection_Destination) = K_Subcomponent)
                 and then
                 ((Kind (Connection_Source) = K_Subcomponent_Access
                     and then not Is_Provided (Connection_Source))
                    or else Kind (Connection_Source) = K_Subcomponent));

         when others =>
            Directions := True;
      end case;

      if not Directions then
         DAE
           (Loc      => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Node),
            Node1    => Get_Referenced_Entity (Source (Node)),
            Message1 => " and ",
            Node2    => Get_Referenced_Entity (Destination (Node)),
            Message2 => " do not have compatible directions");
         Directions := False;
      end if;

      return Directions;
   end Check_End_Directions_Consistency;

   ---------------------------------
   -- Check_End_Types_Consistency --
   ---------------------------------

   function Check_End_Types_Consistency (Node : Node_Id) return Boolean is

      pragma Assert (Kind (Node) = K_Connection);

      Source_Node : constant Node_Id := Get_Referenced_Entity (Source (Node));
      Destination_Node : constant Node_Id :=
        Get_Referenced_Entity (Destination (Node));
      Source_Is_Local : constant Boolean :=
        Connection_End_Is_Local (Source (Node));
      Destination_Is_Local : constant Boolean :=
        Connection_End_Is_Local (Destination (Node));

      Source_Type      : Node_Id;
      Destination_Type : Node_Id;

      Success : Boolean := True;
      No_Type : Boolean := True;
   begin
      --  For port and parameter connections, the consistency is
      --  ensured if the associated data are of the same type, or if
      --  one is an implementation of the other one. For subcomponent
      --  accesses, the provided subcomponent must be of the same type
      --  or be an implementation of the required one. Same thing for
      --  subprogram as features. Ends of a Port group connection must
      --  be the inverse one of the other.

      if Present (Entity_Ref (Source_Node)) then
         Source_Type := Get_Referenced_Entity (Entity_Ref (Source_Node));
      else
         Source_Type := No_Node;
      end if;

      if Present (Entity_Ref (Destination_Node)) then
         Destination_Type :=
           Get_Referenced_Entity (Entity_Ref (Destination_Node));
      else
         Destination_Type := No_Node;
      end if;

      No_Type := No (Source_Type) or else No (Destination_Type);

      if No_Type then
         return True;

         --  If one of the two ends has no type, there is no use
         --  checking anything.
      end if;

      case Connection_Type'Val (Category (Node)) is
         when CT_Error =>
            Success := False;

         when CT_Feature              |
           CT_Access_Subprogram_Group |
           CT_Access_Virtual_Bus      |
           CT_Access                  =>
            Success := True;
         --  XXX incomplete TODO

         when CT_Port_Connection |
           CT_Data               |
           CT_Data_Delayed       |
           CT_Event_Data         |
           CT_Parameter          =>
            if Source_Type = Destination_Type then
               Success := True;
            else
               DAE
                 (Loc      => Loc (Node),
                  Node1    => Source (Node),
                  Message1 => " and ",
                  Node2    => Destination (Node),
                  Message2 => " do not have compatible types");
               Success := False;
            end if;

         when CT_Event =>
            Success := True;

         when CT_Feature_Group =>
            if Source_Is_Local = Destination_Is_Local then
               Success :=
                 (Present (Inverse_Of (Source_Type))
                  and then
                    Get_Referenced_Entity (Inverse_Of (Source_Type)) =
                    Destination_Type)
                 or else
                 (Present (Inverse_Of (Destination_Type))
                  and then
                    Get_Referenced_Entity (Inverse_Of (Destination_Type)) =
                    Source_Type);
            else
               Success := (Source_Type = Destination_Type);
            end if;

            if not Success then
               DAE
                 (Loc      => Loc (Node),
                  Node1    => Source (Node),
                  Message1 => " and ",
                  Node2    => Destination (Node),
                  Message2 => " do not have compatible types");
               Success := False;
            end if;

         --  XXX This comparison is too basic. We should compare
         --  the content of the port groups instead

         when CT_Access_Bus | CT_Access_Data | CT_Access_Subprogram =>

            Success :=
              Check_Classifier_Matching_Rule (Source_Type, Destination_Type);

            if not Success then
               DAE
                 (Loc      => Loc (Node),
                  Node1    => Source (Node),
                  Message1 => " and ",
                  Node2    => Destination (Node),
                  Message2 => " do not have compatible types");
            end if;
      end case;

      return Success;
   end Check_End_Types_Consistency;

   -------------------------------------
   -- Check_For_A_Unique_Initial_Mode --
   -------------------------------------

   function Check_For_A_Unique_Initial_Mode (Node : Node_Id) return Boolean is

      pragma Assert (Kind (Node) = K_Component_Implementation);

      First_Initial_Mode : Node_Id := No_Node;
      Number_Of_Modes    : Integer := 0;
      Component          : Node_Id := Node;
      Success            : Boolean := True;
      List_Node          : Node_Id;
   begin
      while Present (Component) loop
         if not Is_Empty (Modes (Component)) then
            List_Node := First_Node (Modes (Component));

            while Present (List_Node) loop
               if Kind (List_Node) = K_Mode then
                  Number_Of_Modes := Number_Of_Modes + 1;

                  if Is_Initial (List_Node) then
                     --  The initial mode may be overridden by the
                     --  component.

                     if No (First_Initial_Mode) then
                        First_Initial_Mode := List_Node;
                     else
                        if Component = Node then
                           Display_Conflicting_Initial_Modes
                             (List_Node,
                              First_Initial_Mode);
                        else
                           Display_Conflicting_Initial_Modes
                             (First_Initial_Mode,
                              List_Node);
                        end if;
                        Success := False;
                     end if;
                  end if;
               end if;

               List_Node := Next_Node (List_Node);
            end loop;
         end if;

         if Present (Parent (Component)) then
            Component := Get_Referenced_Entity (Parent (Component));
         else
            Component := No_Node;
         end if;
      end loop;

      if No (First_Initial_Mode) and then Number_Of_Modes /= 0 then
         DAE (Node1 => Node, Message1 => " has no initial mode");
         Success := False;
      end if;

      return Success;
   end Check_For_A_Unique_Initial_Mode;

   --------------------------------------------------
   -- Check_Properties_Of_Component_Implementation --
   --------------------------------------------------

   function Check_Properties_Of_Component_Implementation
     (Component : Node_Id) return Boolean
   is
      pragma Assert (Kind (Component) = K_Component_Implementation);

      Success        : Boolean := True;
      List_Node      : Node_Id;
      Call_List_Node : Node_Id;
   begin
      --  Type refinements

      if Refines_Type (Component) /= No_List then
         List_Node := First_Node (Refines_Type (Component));

         while Present (List_Node) loop
            Success :=
              Check_Property_Associations
                (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Subcomponents

      if Subcomponents (Component) /= No_List then
         List_Node := First_Node (Subcomponents (Component));

         while Present (List_Node) loop
            Success :=
              Check_Property_Associations
                (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Call sequences
      --  Some call sequences are anonymous

      if Calls (Component) /= No_List then
         List_Node := First_Node (Calls (Component));

         while Present (List_Node) loop
            if Subprogram_Calls (List_Node) /= No_List then
               Call_List_Node := First_Node (Subprogram_Calls (List_Node));

               while Present (Call_List_Node) loop
                  Success :=
                    Check_Property_Associations
                      (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
                         (Call_List_Node),
                       Call_List_Node)
                    and then Success;
                  Call_List_Node := Next_Node (Call_List_Node);
               end loop;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Connections
      --  Some connections are anonymous

      if Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Component) /=
        No_List
      then
         List_Node :=
           First_Node
             (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Component));

         while Present (List_Node) loop
            Success :=
              Check_Property_Associations
                (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 List_Node)
              and then Success;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Flows

      if Flows (Component) /= No_List then
         List_Node := First_Node (Flows (Component));

         while Present (List_Node) loop
            Success :=
              Check_Property_Associations
                (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Modes

      if Modes (Component) /= No_List then
         List_Node := First_Node (Modes (Component));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Mode then
               Success :=
                 Check_Property_Associations
                   (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                    List_Node)
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Properties

      Success :=
        Check_Property_Associations
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Component),
           Component)
        and then Success;

      return Success;
   end Check_Properties_Of_Component_Implementation;

   ----------------------------------------
   -- Check_Properties_Of_Component_Type --
   ----------------------------------------

   function Check_Properties_Of_Component_Type
     (Component : Node_Id) return Boolean
   is
      pragma Assert (Kind (Component) = K_Component_Type);

      Success   : Boolean := True;
      List_Node : Node_Id;
   begin
      --  Features

      if Features (Component) /= No_List then
         List_Node := First_Node (Features (Component));

         while Present (List_Node) loop
            Success :=
              Check_Property_Associations
                (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Flows

      if Flows (Component) /= No_List then
         List_Node := First_Node (Flows (Component));

         while Present (List_Node) loop
            Success :=
              Check_Property_Associations
                (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Properties

      Success :=
        Check_Property_Associations
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Component),
           Component)
        and then Success;

      return Success;
   end Check_Properties_Of_Component_Type;

   -----------------------------------------
   -- Check_Properties_Of_Port_Group_Type --
   -----------------------------------------

   function Check_Properties_Of_Port_Group_Type
     (Port_Group : Node_Id) return Boolean
   is
      pragma Assert (Kind (Port_Group) = K_Feature_Group_Type);

      Success   : Boolean := True;
      List_Node : Node_Id;
   begin
      --  Features

      if Features (Port_Group) /= No_List then
         List_Node := First_Node (Features (Port_Group));

         while Present (List_Node) loop
            Success :=
              Check_Property_Associations
                (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Properties

      Success :=
        Check_Property_Associations
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Port_Group),
           Port_Group)
        and then Success;

      return Success;
   end Check_Properties_Of_Port_Group_Type;

   ---------------------------------
   -- Check_Property_Associations --
   ---------------------------------

   function Check_Property_Associations
     (Properties : List_Id;
      Container  : Node_Id) return Boolean
   is
      pragma Assert (Present (Container));

      Success   : Boolean := True;
      List_Node : Node_Id;
   begin
      if Properties /= No_List then
         List_Node := First_Node (Properties);

         while Present (List_Node) loop
            pragma Assert (Kind (List_Node) = K_Property_Association);

            Success :=
              Check_Applies_To (List_Node, Container)
              and then Check_Values_Of_Property_Association (List_Node)
              and then Success;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Check_Property_Associations;

   -------------------------
   -- Check_Property_Type --
   -------------------------

   function Check_Property_Type
     (Property_Type         : Node_Id;
      Display_Error_Message : Boolean := True) return Boolean
   is
      pragma Assert
        (Kind (Property_Type) = K_Property_Type_Declaration
         or else Kind (Property_Type) = K_Property_Type
         or else Kind (Property_Type) = K_Integer_Type
         or else Kind (Property_Type) = K_Real_Type
         or else Kind (Property_Type) = K_Enumeration_Type
         or else Kind (Property_Type) = K_Boolean_Type
         or else Kind (Property_Type) = K_String_Type
         or else Kind (Property_Type) = K_Range_Type
         or else Kind (Property_Type) = K_Reference_Type
         or else Kind (Property_Type) = K_Classifier_Type
         or else Kind (Property_Type) = K_Unique_Property_Type_Identifier
         or else Kind (Property_Type) = K_Record_Type);

      Type_Designator : Node_Id;
      Success         : Boolean := True;
   begin
      case Kind (Property_Type) is
         when K_Property_Type_Declaration =>
            Type_Designator := Property_Type_Designator (Property_Type);
         when K_Property_Type =>
            Type_Designator := Expanded_Type_Designator (Property_Type);
         when others =>
            Type_Designator := Property_Type;
      end case;

      if Present (Type_Designator) then
         case Kind (Type_Designator) is
            when K_Integer_Type | K_Real_Type =>
               if Present (Type_Range (Type_Designator))
                 and then Present (Lower_Bound (Type_Range (Type_Designator)))
                 and then
                   Kind (Lower_Bound (Type_Range (Type_Designator))) =
                   K_Literal
                 and then Present (Upper_Bound (Type_Range (Type_Designator)))
                 and then
                   Kind (Upper_Bound (Type_Range (Type_Designator))) =
                   K_Literal
               then
                  --  We only check the types that are completely
                  --  defined. Typically, the types that have been
                  --  expanded.

                  Success :=
                    Compare_Numbers
                      (Lower_Bound (Type_Range (Type_Designator)),
                       Upper_Bound (Type_Range (Type_Designator))) >=
                    0;
               end if;

            when K_Range_Type =>
               Success :=
                 Check_Property_Type (Number_Type (Type_Designator), False);

            when others =>
               Success := True;
         end case;
      end if;

      if Display_Error_Message and then not Success then
         Display_Inconsistent_Property_Type (Property_Type);
      end if;

      return Success;
   end Check_Property_Type;

   -----------------------
   -- Reset_Connections --
   -----------------------

   procedure Reset_Connections (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Component_Implementation);

      List_Node : Node_Id;
   begin
      --  Reset connections info on name table

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node));

         while Present (List_Node) loop
            declare
               Src    : constant Node_Id := Source (List_Node);
               Dst    : constant Node_Id := Destination (List_Node);
               N      : Node_Id;
               I_Name : Name_Id;
            begin
               --  We use name_buffer hash codes for efficiency. First
               --  we inster the following string : Node%Path
               --  (Src)%Path (Dst)%dup%cnx%check and then we compute
               --  its byte info. If we find a non-zero info, this
               --  means the connection is duplicate and the info
               --  poits to the first met connection. Finally, we set
               --  the info of the string to point to the current
               --  connection.

               Set_Nat_To_Name_Buffer (Nat (Node));
               Add_Char_To_Name_Buffer ('%');

               N := First_Node (Path (Src));
               while Present (N) loop
                  Add_Nat_To_Name_Buffer
                    (Nat (Corresponding_Entity (Item (N))));
                  Add_Char_To_Name_Buffer ('%');

                  N := Next_Node (N);
               end loop;

               N := First_Node (Path (Dst));
               while Present (N) loop
                  Add_Nat_To_Name_Buffer
                    (Nat (Corresponding_Entity (Item (N))));
                  Add_Char_To_Name_Buffer ('%');

                  N := Next_Node (N);
               end loop;

               Add_Str_To_Name_Buffer ("dup%cnx%check");
               I_Name := Name_Find;
               Set_Name_Table_Info (I_Name, 0);
            end;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;
   end Reset_Connections;

   ----------------------
   -- Reset_Connection --
   ----------------------

   procedure Reset_All_Connections (Root : Node_Id) is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      List_Node         : Node_Id;
      Package_List_Node : Node_Id;
   begin
      if not Is_Empty (Declarations (Root)) then
         List_Node := First_Node (Declarations (Root));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Component_Implementation =>
                  Reset_Connections (List_Node);

               when K_Package_Specification =>
                  if not Is_Empty (Declarations (List_Node)) then
                     Package_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Present (Package_List_Node) loop
                        case Kind (Package_List_Node) is
                           when K_Component_Implementation =>
                              Reset_Connections (Package_List_Node);

                           when others =>
                              null;
                        end case;

                        Package_List_Node := Next_Node (Package_List_Node);
                     end loop;
                  end if;

               when others =>
                  null;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

   end Reset_All_Connections;

   -----------------------------------
   -- Check_Semantics_In_Components --
   -----------------------------------

   function Check_Semantics_In_Components (Root : Node_Id) return Boolean is

      pragma Assert (Kind (Root) = K_AADL_Specification);

      Success           : Boolean := True;
      List_Node         : Node_Id;
      Package_List_Node : Node_Id;
   begin
      if not Is_Empty (Declarations (Root)) then
         List_Node := First_Node (Declarations (Root));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Component_Implementation =>
                  Success :=
                    Check_For_A_Unique_Initial_Mode (List_Node)
                    and then Check_Cycles_In_Subcomponents (List_Node)
                    and then Check_Connections (List_Node)
                    and then Success;

               when K_Package_Specification =>
                  if not Is_Empty (Declarations (List_Node)) then
                     Package_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Present (Package_List_Node) loop
                        case Kind (Package_List_Node) is
                           when K_Component_Implementation =>
                              Success :=
                                Check_For_A_Unique_Initial_Mode
                                  (Package_List_Node)
                                and then Check_Cycles_In_Subcomponents
                                  (Package_List_Node)
                                and then Check_Connections (Package_List_Node)
                                and then Success;

                           when others =>
                              null;
                        end case;

                        Package_List_Node := Next_Node (Package_List_Node);
                     end loop;
                  end if;

               when others =>
                  null;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Check_Semantics_In_Components;

   -----------------------------------
   -- Check_Semantics_In_Namespaces --
   -----------------------------------

   function Check_Semantics_In_Namespaces (Root : Node_Id) return Boolean is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      Success           : Boolean := True;
      List_Node         : Node_Id;
      Package_List_Node : Node_Id;
   begin
      if not Is_Empty (Declarations (Root)) then
         List_Node := First_Node (Declarations (Root));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Component_Type =>
                  Success :=
                    Check_Cycles_In_Port_Group_Or_Component_Type (List_Node)
                    and then Success;

               when K_Component_Implementation =>
                  Success :=
                    Check_Cycles_In_Component_Implementation (List_Node)
                    and then Success;

               when K_Feature_Group_Type =>
                  Success :=
                    (Check_Cycles_In_Port_Group_Or_Component_Type (List_Node)
                     and then Check_Cycles_In_Inversions_Of_Port_Groups
                       (List_Node))
                    and then Success;

               when K_Package_Specification =>
                  if not Is_Empty (Declarations (List_Node)) then
                     Package_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Present (Package_List_Node) loop
                        case Kind (Package_List_Node) is
                           when K_Component_Type =>
                              Success :=
                                Check_Cycles_In_Port_Group_Or_Component_Type
                                  (Package_List_Node)
                                and then Success;

                           when K_Component_Implementation =>
                              Success :=
                                Check_Cycles_In_Component_Implementation
                                  (Package_List_Node)
                                and then Success;

                           when K_Feature_Group_Type =>
                              Success :=
                                (Check_Cycles_In_Port_Group_Or_Component_Type
                                   (Package_List_Node)
                                   and then
                                   Check_Cycles_In_Inversions_Of_Port_Groups
                                   (Package_List_Node))
                                and then Success;

                           when others =>
                              null;
                        end case;

                        Package_List_Node := Next_Node (Package_List_Node);
                     end loop;
                  end if;

               when others =>
                  null;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Check_Semantics_In_Namespaces;

   -----------------------------------
   -- Check_Semantics_Of_Properties --
   -----------------------------------

   function Check_Semantics_Of_Properties (Root : Node_Id) return Boolean is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      Success           : Boolean;
      List_Node         : Node_Id;
      Package_List_Node : Node_Id;
   begin
      Success := Compute_Property_Values (Root);

      if Success and then Declarations (Root) /= No_List then
         List_Node := First_Node (Declarations (Root));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Component_Implementation =>
                  Success :=
                    Check_Properties_Of_Component_Implementation (List_Node)
                    and then Success;

               when K_Component_Type =>
                  Success :=
                    Check_Properties_Of_Component_Type (List_Node)
                    and then Success;

               when K_Feature_Group_Type =>
                  Success :=
                    Check_Properties_Of_Port_Group_Type (List_Node)
                    and then Success;

               when K_Package_Specification =>
                  Success :=
                    Check_Property_Associations
                      (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                       List_Node)
                    and then Success;

                  if Declarations (List_Node) /= No_List then
                     Package_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Present (Package_List_Node) loop
                        case Kind (Package_List_Node) is
                           when K_Component_Implementation =>
                              Success :=
                                Check_Properties_Of_Component_Implementation
                                  (Package_List_Node)
                                and then Success;

                           when K_Component_Type =>
                              Success :=
                                Check_Properties_Of_Component_Type
                                  (Package_List_Node)
                                and then Success;

                           when K_Feature_Group_Type =>
                              Success :=
                                Check_Properties_Of_Port_Group_Type
                                  (Package_List_Node)
                                and then Success;

                           when others =>
                              null;
                        end case;

                        Package_List_Node := Next_Node (Package_List_Node);
                     end loop;
                  end if;

               when K_Property_Set =>
                  if Declarations (List_Node) /= No_List then
                     Package_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Present (Package_List_Node) loop
                        case Kind (Package_List_Node) is
                           when K_Property_Type_Declaration =>
                              Success :=
                                Check_Property_Type (Package_List_Node)
                                and then Success;
                           when others =>
                              null;
                        end case;

                        Package_List_Node := Next_Node (Package_List_Node);
                     end loop;
                  end if;

               when others =>
                  null;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Check_Semantics_Of_Properties;

   ------------------------------------------
   -- Check_Values_Of_Property_Association --
   ------------------------------------------

   function Check_Values_Of_Property_Association
     (Property_Association : Node_Id) return Boolean
   is
      pragma Assert (Kind (Property_Association) = K_Property_Association);

      Property_Name : constant Node_Id :=
        Get_Referenced_Entity
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Property_Name
             (Property_Association));
      Type_Of_Property_Name : constant Property_Type :=
        Get_Type_Of_Property (Property_Name);

      List_Node            : Node_Id;
      Success              : Boolean := True;
      Types_Are_Compatible : Boolean := True;
   begin
      if Value_Of_Property_Association_Is_Undefined (Property_Association) then
         Success := True;
      else
         --  Extract from the AADL 1.0 standard (paragraph 10.3 page 158):

         --  "If the property declaration for the associated property
         --  name *does not* contain the reserved words *list of*, the
         --  property value must be a single property value. If the
         --  property declaration for the associated property name
         --  contains the reserved words *list of*, the property value
         --  *can be a single property value*, which is interpreted to
         --  be a list of one value."

         --  So list properties can take single values after '=>' and
         --  '+=>'

         --  However we convert these property to their right form
         --  (and keep the old form) to make easier further tree
         --  manipulations.

         if Is_Additive_Association (Property_Association)
           and then Present
             (Expanded_Single_Value
                (Property_Association_Value (Property_Association)))
         then
            --  Additive association allowed only for list properties

            if not Type_Of_Property_Is_A_List (Property_Name) then
               Success := Convert_Single_Value_To_List (Property_Association);

               --  Even if the conversion succeded, this is an
               --  error. This is a workaround to get the proper error
               --  message.

               Display_Property_List_Discrepancy
                 (Property_Association => Property_Association,
                  Property_Name        => Property_Name);
               return False;
            else
               Success := Convert_Single_Value_To_List (Property_Association);

               --  We do not return since there are more tests to
               --  perform.
            end if;
         end if;

         --  To avoid endless recursion, we begin by testing list
         --  properties.

         if Expanded_Multi_Value
             (Property_Association_Value (Property_Association)) /=
           No_List
         then
            if Type_Of_Property_Is_A_List (Property_Name) then
               List_Node :=
                 First_Node
                   (Expanded_Multi_Value
                      (Property_Association_Value (Property_Association)));

               while Present (List_Node) loop
                  Types_Are_Compatible :=
                    Test_Property_Type_Equivalence
                      (Type_Of_Property_Name,
                       Get_Type_Of_Property_Value (List_Node))
                    and then Test_Property_Value_Validity
                      (Property_Name_Type (Property_Name),
                       List_Node);

                  if not Types_Are_Compatible then
                     Display_Incompatible_Property_Types
                       (Property_Association => Property_Association,
                        Property_Value       => List_Node,
                        Property_Name        => Property_Name);
                     Success := False;
                  end if;

                  List_Node := Next_Node (List_Node);
               end loop;

            else
               --  Single value properties cannot have a list as
               --  value.

               Success := False;
               Display_Property_List_Discrepancy
                 (Property_Association => Property_Association,
                  Property_Name        => Property_Name);
            end if;

         elsif Expanded_Single_Value
             (Property_Association_Value (Property_Association)) /=
           No_Node
         then
            if Type_Of_Property_Is_A_List (Property_Name) then
               --  If the value is a single element while we are
               --  expecting a list, we build a list from the single
               --  element, and display a warning. and keep the single
               --  element untouched.

               Display_Conversion_To_Property_List
                 (Property_Association => Property_Association,
                  Property_Name        => Property_Name);
               Success :=
                 Convert_Single_Value_To_List (Property_Association)
                 and then Check_Values_Of_Property_Association
                   (Property_Association);
            else
               Success :=
                 Test_Property_Type_Equivalence
                   (Type_Of_Property_Name,
                    Get_Type_Of_Property_Value
                      (Expanded_Single_Value
                         (Property_Association_Value (Property_Association))))
                 and then Test_Property_Value_Validity
                   (Property_Name_Type (Property_Name),
                    Expanded_Single_Value
                      (Property_Association_Value (Property_Association)));

               if not Success then
                  Display_Incompatible_Property_Types
                    (Property_Association => Property_Association,
                     Property_Value       =>
                       Expanded_Single_Value
                         (Property_Association_Value (Property_Association)),
                     Property_Name => Property_Name);
               end if;
            end if;

         else
            --  If the property association has no actual value

            Success := True;
         end if;
      end if;

      return Success;
   end Check_Values_Of_Property_Association;

   ---------------------
   -- Compare_Numbers --
   ---------------------

   function Compare_Numbers
     (Number_1 : Node_Id;
      Number_2 : Node_Id) return Integer
   is
      pragma Assert
        (Kind (Number_1) = K_Literal
         or else Kind (Number_1) = K_Signed_AADLNumber);
      pragma Assert
        (Kind (Number_2) = K_Literal
         or else Kind (Number_2) = K_Signed_AADLNumber);

      Literal_1 : Node_Id;
      Literal_2 : Node_Id;
      Result    : Integer;
   begin
      if Kind (Number_1) = K_Literal and then Kind (Number_2) = K_Literal then
         Literal_1 := Number_1;
         Literal_2 := Number_2;
      else
         Homogenize_Unit_Numbers (Number_1, Number_2, Literal_1, Literal_2);
      end if;

      --  Use the routines of the AADL_Values package to compare the
      --  values and wrap them to intercept any comparison error.

      begin
         if Value (Value (Literal_1)) < Value (Value (Literal_2)) then
            Result := 1;
         elsif Value (Value (Literal_2)) < Value (Value (Literal_1)) then
            Result := -1;
         else
            Result := 0;
         end if;
      exception
         when Constraint_Error =>
            Result := -2;
            raise;
      end;

      return Result;
   end Compare_Numbers;

   -----------------------------
   -- Connection_End_Is_Local --
   -----------------------------

   function Connection_End_Is_Local (Node : Node_Id) return Boolean is
      pragma Assert (Kind (Node) = K_Entity_Reference);

   begin
      return Next_Node (First_Node (Path (Node))) = No_Node
        or else
          Kind (Corresponding_Entity (Item (First_Node (Path (Node))))) =
          K_Feature_Group_Spec;
   end Connection_End_Is_Local;

   ----------------------------------
   -- Convert_Single_Value_To_List --
   ----------------------------------

   function Convert_Single_Value_To_List
     (Property_Association : Node_Id) return Boolean
   is
      pragma Assert (Kind (Property_Association) = K_Property_Association);
   begin
      Set_Expanded_Multi_Value
        (Property_Association_Value (Property_Association),
         New_List
           (K_List_Id,
            Loc
              (Expanded_Single_Value
                 (Property_Association_Value (Property_Association)))));
      Append_Node_To_List
        (Expanded_Single_Value
           (Property_Association_Value (Property_Association)),
         Expanded_Multi_Value
           (Property_Association_Value (Property_Association)));

      Set_Multi_Value
        (Property_Association_Value (Property_Association),
         New_List
           (K_List_Id,
            Loc
              (Single_Value
                 (Property_Association_Value (Property_Association)))));
      Append_Node_To_List
        (Single_Value (Property_Association_Value (Property_Association)),
         Multi_Value (Property_Association_Value (Property_Association)));

      return True;
   end Convert_Single_Value_To_List;

   -----------------------------
   -- Homogenize_Unit_Numbers --
   -----------------------------

   procedure Homogenize_Unit_Numbers
     (Number_1  :     Node_Id;
      Number_2  :     Node_Id;
      Literal_1 : out Node_Id;
      Literal_2 : out Node_Id)
   is

      Unit_1 : Node_Id;
      Unit_2 : Node_Id;
   begin
      --  If one of the numbers is a literal (without a unit) this
      --  means that the corresponding unit type contains only one
      --  unit identifier and that the node linker did not detect an
      --  error. So we return the literal without modifying them.

      if Kind (Number_1) = K_Literal then
         Literal_1 := Number_1;

         --  Number_2 is necessarily a K_Signed_AADLNumber

         Literal_2 := Number_Value (Number_2);

         --  Nothing else to do

         return;
      end if;

      if Kind (Number_2) = K_Literal then
         Literal_2 := Number_2;

         --  Number_1 is necessarily a K_Signed_AADLNumber

         Literal_1 := Number_Value (Number_1);

         --  Nothing more to do

         return;
      end if;

      --  At this stage, both numbers are K_Signed_AADLNumber's. But
      --  they may have null Unit identifiers.

      if No (Unit_Identifier (Number_1))
        or else No (Unit_Identifier (Number_2))
      then
         Literal_1 := Number_Value (Number_1);
         Literal_2 := Number_Value (Number_2);

         --  Nothing more to do

         return;
      end if;

      --  At this stage, we have two K_Signed_AADLNumber's with non
      --  null unit identifiers.

      --  Get the corresponding unit identifiers. If the name linker
      --  failed to find the corresponding unit identifier, do not
      --  cause error cascade.

      Unit_1 := Corresponding_Entity (Unit_Identifier (Number_1));
      Unit_2 := Corresponding_Entity (Unit_Identifier (Number_2));

      --  Convert the literals

      if Present (Unit_1) then
         Literal_1 := Convert_To_Base (Number_Value (Number_1), Unit_1);
      else
         Literal_1 := Number_Value (Number_1);
      end if;

      if Present (Unit_2) then
         Literal_2 := Convert_To_Base (Number_Value (Number_2), Unit_2);
      else
         Literal_2 := Number_Value (Number_2);
      end if;
   end Homogenize_Unit_Numbers;

   ------------------------------------
   -- Test_Property_Type_Equivalence --
   ------------------------------------

   function Test_Property_Type_Equivalence
     (Type_Of_Property_Name : Ocarina.ME_AADL.AADL_Tree.Entities.Properties
        .Property_Type;
      Type_Of_Property_Association : Ocarina.ME_AADL.AADL_Tree.Entities
        .Properties
        .Property_Type)
      return Boolean
   is
      Success : Boolean;
   begin
      case Type_Of_Property_Name is
         when PT_Boolean =>
            Success :=
              Type_Of_Property_Association = PT_Boolean_Expression
              or else Type_Of_Property_Association = PT_Boolean;

         when PT_Integer =>
            Success :=
              Type_Of_Property_Association = PT_Integer
              or else Type_Of_Property_Association = PT_Unsigned_Integer;

         when PT_Float =>
            Success :=
              Type_Of_Property_Association = PT_Float
              or else Type_Of_Property_Association = PT_Unsigned_Float
              or else Type_Of_Property_Association = PT_Integer
              or else Type_Of_Property_Association = PT_Unsigned_Integer;

         when PT_List =>
            Success := False;

         when PT_Reference =>
            Success := Type_Of_Property_Association = PT_Reference;

         when others =>
            Success := Type_Of_Property_Association = Type_Of_Property_Name;
      end case;

      return Success;
   end Test_Property_Type_Equivalence;

   ----------------------------------
   -- Test_Property_Value_Validity --
   ----------------------------------

   function Check_Property_Value
     (Type_Designator : Node_Id;
      Property_Value : Node_Id)
     return Boolean
   is
      List_Node       : Node_Id;
      Temp_Node       : Node_Id;

      Is_Integer      : Boolean;
      Actual_Literal  : Node_Id;
      Success : Boolean := True;

   begin
      case Kind (Type_Designator) is
         when K_Classifier_Type =>
            List_Node := First_Node (List_Items (Type_Designator));
            Success   := False;

            if Kind (Property_Value) = K_Component_Classifier_Term then
               Temp_Node := Get_Referenced_Entity (Property_Value);

               if Present (Temp_Node) then
                  case AADL_Version is
                     when AADL_V1 =>
                        while Present (List_Node) loop
                           if Get_Category_Of_Component (Temp_Node) =
                             Component_Category'Val (Category (List_Node))
                           then
                              Success := True;
                           end if;

                           List_Node := Next_Node (List_Node);
                        end loop;

                     when AADL_V2 =>
                        Success := True;
                  end case;
               end if;
            end if;

         when K_Record_Term => --  XXX incomplete implementation
            Success := True;
            null;

         when K_Record_Type => --  XXX incomplete implementation
            Success := True;
--              declare
--                 J : Node_Id := First_Node (List_Items (Property_Value));
--              begin
--                 while Present (J) loop
--                    Put_Line ("plop"
--                                & Kind (J)'Img);
--                    wni (J);
--                    J := Next_Node (J);
--                 end loop;
--              end;

         when K_Reference_Type =>
            if List_Items (Type_Designator) = No_List then
               List_Node := No_Node;
            else
               List_Node := First_Node (List_Items (Type_Designator));
            end if;

            if Present (List_Node) then
                  Success := False;
            else
               Success := True;

               --  If no type is indicated, then any reference is
               --  correct.
            end if;

            if Kind (Property_Value) = K_Reference_Term then
               Temp_Node :=
                    Get_Referenced_Entity (Reference_Term (Property_Value));

               if Present (Temp_Node) then
                  while Present (List_Node) loop
                     case AADL_Version is
                        when AADL_V1 =>
                           case
                             (Referable_Element_Category'Val
                                (Category (List_Node)))
                              is
                              when REC_Component_Category =>
                                 if Get_Entity_Category (Temp_Node) =
                                   EC_Subcomponent
                                 then
                                    --  If the subcomponent
                                    --  specification is incomplete
                                    --  (see AADL 1.0 standard
                                    --  paragraph 4.5 section
                                    --  `semantics'), then there is
                                    --  nothing else to analyze.

                                    if No (Entity_Ref (Temp_Node)) then
                                       Success := True;
                                    elsif Get_Category_Of_Component
                                      (Get_Referenced_Entity
                                         (Entity_Ref (Temp_Node))) =
                                      Component_Category'Val
                                      (Component_Cat (List_Node))
                                    then
                                       Success := True;
                                    end if;
                                 end if;
                              when REC_Connections =>
                                 if Get_Entity_Category (Temp_Node) =
                                   EC_Connection
                                 then
                                    Success := True;
                                 end if;
                              when REC_Server_Subprogram =>
                                 if Get_Entity_Category (Temp_Node) =
                                   EC_Feature
                                 then
                                    --  XXX This is incomplete

                                    Success := True;
                                 end if;
                              when REC_Identifier =>
                                 --  XXX This is incomplete

                                 Success := True;
                              when REC_Subprogram_Call_Sequence =>
                                 --  XXX This is incomplete

                                 Success := True;
                           end case;
                        when AADL_V2 =>
                           Success := True;
                           --  XXX This incomplete

                     end case;

                     List_Node := Next_Node (List_Node);
                  end loop;
               end if;
            else
               Success := False;
            end if;

         when K_Real_Type =>
            if Kind (Property_Value) = K_Literal
              or else Kind (Property_Value) = K_Signed_AADLNumber
            then
               Actual_Literal := Property_Value;
            else
               Actual_Literal := No_Node;
            end if;

            if Present (Actual_Literal) then
               if No (Type_Range (Type_Designator)) then
                  Success := True;
               else
                  Success :=
                    (Compare_Numbers
                       (Lower_Bound (Type_Range (Type_Designator)),
                        Actual_Literal) >=
                       0)
                    and then
                    (Compare_Numbers
                       (Actual_Literal,
                        Upper_Bound (Type_Range (Type_Designator))) >=
                       0);
                  if not Success then
                     Error_Loc (1) := Loc (Property_Value);
                     DE ("Property value is not in the range" &
                           " defined for this property type");
                  end if;
               end if;
            else
               Success := False;
            end if;

         when K_Integer_Type =>
            if Kind (Property_Value) = K_Literal then
               Actual_Literal := Property_Value;
               Is_Integer := Value (Value (Actual_Literal)).T = LT_Integer;
            elsif Kind (Property_Value) = K_Signed_AADLNumber then
               Actual_Literal := Property_Value;
               Is_Integer     :=
                 Value (Value (Number_Value (Actual_Literal))).T =
                 LT_Integer;
            else
               Actual_Literal := No_Node;
               Is_Integer     := False;
            end if;

            if Is_Integer then
               if Type_Range (Type_Designator) = No_Node then
                  Success := True;
               else
                  Success :=
                    Present (Lower_Bound (Type_Range (Type_Designator)))
                    and then
                    (Compare_Numbers
                       (Lower_Bound (Type_Range (Type_Designator)),
                        Actual_Literal) >=
                       0)
                    and then Present
                    (Upper_Bound (Type_Range (Type_Designator)))
                    and then
                    (Compare_Numbers
                       (Actual_Literal,
                        Upper_Bound (Type_Range (Type_Designator))) >=
                       0);
                  if not Success then
                     Error_Loc (1) := Loc (Property_Value);
                     DE ("Property value is not in the range" &
                           " defined for this property type");
                  end if;
               end if;
            else
               Success := False;
            end if;

         when K_Range_Type =>
            Success := True;

         when K_Boolean_Type =>
            Success :=
              Kind (Property_Value) = K_Literal
              and then Value (Value (Property_Value)).T = LT_Boolean;

         when K_String_Type =>
            Success :=
              Kind (Property_Value) = K_Literal
              and then Value (Value (Property_Value)).T = LT_String;

         when K_Enumeration_Type =>
            case AADL_Version is
               when AADL_V1 =>
                  Success :=
                    Kind (Property_Value) = K_Literal
                    and then
                    Value (Value (Property_Value)).T =
                    LT_Enumeration;

                  if Success then
                     Success   := False;
                     List_Node :=
                       First_Node (Identifiers (Type_Designator));

                     while Present (List_Node) loop
                        Success :=
                          Success
                          or else
                               Name (List_Node) =
                          Value (Value (Property_Value)).EVal;

                        List_Node := Next_Node (List_Node);
                     end loop;
                  end if;

               when AADL_V2 =>
                  Success := Kind (Property_Value) = K_Enumeration_Term;

                  if Success then
                     Success   := False;
                     List_Node :=
                       First_Node (Identifiers (Type_Designator));

                     while Present (List_Node) loop
                        Success :=
                          Success
                          or else
                          Name (List_Node) =
                          Name (Identifier (Property_Value));

                        List_Node := Next_Node (List_Node);
                     end loop;
                  end if;
            end case;

         when others =>
            Success := False;
      end case;

      return Success;
   end Check_Property_Value;

   function Test_Property_Value_Validity
     (Property_Type  : Node_Id;
      Property_Value : Node_Id) return Boolean
   is
      pragma Assert (Kind (Property_Type) = K_Property_Type);
      pragma Assert
        (Kind (Property_Value) = K_Component_Classifier_Term
         or else Kind (Property_Value) = K_Reference_Term
         or else Kind (Property_Value) = K_Record_Term
         or else Kind (Property_Value) = K_Enumeration_Term
         or else Kind (Property_Value) = K_Number_Range_Term
         or else Kind (Property_Value) = K_Literal
         or else Kind (Property_Value) = K_Signed_AADLNumber);

      Type_Designator : Node_Id;

   begin
      Type_Designator := Expanded_Type_Designator (Property_Type);

      return Check_Property_Type (Type_Designator)
        and then Check_Property_Value (Type_Designator, Property_Value);
   end Test_Property_Value_Validity;

end Ocarina.Analyzer.AADL.Semantics;
