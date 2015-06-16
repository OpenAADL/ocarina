------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B U I L D E R . A A D L . P R O P E R T I E S       --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
with Ocarina.Builder.AADL.Namespaces;
with Ocarina.Builder.AADL.Components;
with Ocarina.Builder.AADL.Components.Subcomponents;
with Ocarina.Builder.AADL.Components.Features;
with Ocarina.Builder.AADL.Components.Connections;
with Ocarina.Builder.AADL.Components.Flows;
with Ocarina.Builder.AADL.Components.Modes;
with Ocarina.Builder.AADL.Components.Subprogram_Calls;

package body Ocarina.Builder.AADL.Properties is

   function Add_Property_Type_Declaration
     (Property_Set              : Node_Id;
      Property_Type_Declaration : Node_Id) return Boolean;

   function Add_Property_Definition_Declaration
     (Property_Set                    : Node_Id;
      Property_Definition_Declaration : Node_Id) return Boolean;

   function Add_Property_Constant_Declaration
     (Property_Set                  : Node_Id;
      Property_Constant_Declaration : Node_Id) return Boolean;

   -----------------------------------------
   -- Add_Property_Definition_Declaration --
   -----------------------------------------

   function Add_Property_Definition_Declaration
     (Property_Set                    : Node_Id;
      Property_Definition_Declaration : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Property_Set) = K_Property_Set);
      pragma Assert
        (Kind (Property_Definition_Declaration) =
         K_Property_Definition_Declaration);
   begin
      if Is_Empty (Declarations (Property_Set)) then
         Set_Declarations
           (Property_Set,
            New_List (K_List_Id, Loc (Property_Definition_Declaration)));
      end if;

      Append_Node_To_List
        (Property_Definition_Declaration,
         Declarations (Property_Set));
      return True;
   end Add_Property_Definition_Declaration;

   -----------------------------------
   -- Add_Property_Type_Declaration --
   -----------------------------------

   function Add_Property_Type_Declaration
     (Property_Set              : Node_Id;
      Property_Type_Declaration : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Property_Set) = K_Property_Set);
      pragma Assert
        (Kind (Property_Type_Declaration) = K_Property_Type_Declaration);
   begin
      if Is_Empty (Declarations (Property_Set)) then
         Set_Declarations
           (Property_Set,
            New_List (K_List_Id, Loc (Property_Type_Declaration)));
      end if;

      Append_Node_To_List
        (Property_Type_Declaration,
         Declarations (Property_Set));
      return True;
   end Add_Property_Type_Declaration;

   ---------------------------------------
   -- Add_Property_Constant_Declaration --
   ---------------------------------------

   function Add_Property_Constant_Declaration
     (Property_Set                  : Node_Id;
      Property_Constant_Declaration : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Property_Set) = K_Property_Set);
      pragma Assert
        (Kind (Property_Constant_Declaration) =
         K_Constant_Property_Declaration);
   begin
      if Is_Empty (Declarations (Property_Set)) then
         Set_Declarations
           (Property_Set,
            New_List (K_List_Id, Loc (Property_Constant_Declaration)));
      end if;

      Append_Node_To_List
        (Property_Constant_Declaration,
         Declarations (Property_Set));
      return True;
   end Add_Property_Constant_Declaration;

   --------------------------
   -- Add_New_Property_Set --
   --------------------------

   function Add_New_Property_Set
     (Loc       : Location;
      Name      : Node_Id;
      Namespace : Node_Id) return Node_Id
   is
      use Ocarina.Builder.AADL.Namespaces;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Name) = K_Identifier);
      pragma Assert (Kind (Namespace) = K_AADL_Specification);

      Node    : Node_Id;
      Success : Boolean := True;
   begin
      Node := New_Node (K_Property_Set, Loc);
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Entity_Scope (Node, New_Node (K_Scope_Definition, Loc));
      Set_Corresponding_Entity (Entity_Scope (Node), Node);
      Set_Declarations (Node, No_List);

      Success := Add_Declaration (Namespace, Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Property_Set;

   -------------------------------------------
   -- Add_New_Property_Constant_Declaration --
   -------------------------------------------

   function Add_New_Property_Constant_Declaration
     (Loc             : Location;
      Name            : Node_Id;
      Property_Set    : Node_Id;
      Constant_Type   : Node_Id;
      Unit_Identifier : Node_Id;
      Single_Value    : Node_Id;
      Multiple_Values : List_Id;
      Multiplicity    : Int) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Name) = K_Identifier);
      pragma Assert (Kind (Property_Set) = K_Property_Set);

      pragma Assert
        (No (Unit_Identifier)
         or else Kind (Unit_Identifier) = K_Unique_Property_Type_Identifier);

      pragma Assert (No (Single_Value) or else Is_Empty (Multiple_Values));

      pragma Assert
        (Kind (Constant_Type) = K_Integer_Type
         or else Kind (Constant_Type) = K_Real_Type
         or else Kind (Constant_Type) = K_String_Type
         or else Kind (Constant_Type) = K_Boolean_Type
         or else Kind (Constant_Type) = K_Unique_Property_Type_Identifier
         or else Kind (Constant_Type) = K_Classifier_Type);

      Node : constant Node_Id :=
        New_Node (K_Constant_Property_Declaration, Loc);
      Value_Node : constant Node_Id := New_Node (K_Property_Value, Loc);
      Success    : Boolean          := True;
   begin
      Set_Constant_Value (Node, Value_Node);
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Constant_Type (Node, Constant_Type);
      Set_Unique_Unit_Identifier (Node, Unit_Identifier);
      Set_Multiplicity (Node, Multiplicity);

      Set_Value_Container (Value_Node, Property_Set);
      Set_Single_Value (Value_Node, Single_Value);
      Set_Multi_Value (Value_Node, Multiple_Values);

      Success := Add_Property_Constant_Declaration (Property_Set, Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Property_Constant_Declaration;

   ---------------------------------------
   -- Add_New_Property_Type_Declaration --
   ---------------------------------------

   function Add_New_Property_Type_Declaration
     (Loc             : Location;
      Name            : Node_Id;
      Property_Set    : Node_Id;
      Type_Designator : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Name) = K_Identifier);
      pragma Assert (Kind (Property_Set) = K_Property_Set);

      Node    : Node_Id;
      Success : Boolean := True;
   begin
      Node := New_Node (K_Property_Type_Declaration, Loc);
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Property_Type_Designator (Node, Type_Designator);

      Success := Add_Property_Type_Declaration (Property_Set, Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Property_Type_Declaration;

   ---------------------------------------------
   -- Add_New_Property_Definition_Declaration --
   ---------------------------------------------

   function Add_New_Property_Definition_Declaration
     (Loc                     : Location;
      Name                    : Node_Id;
      Property_Set            : Node_Id;
      Is_Inherit              : Boolean;
      Is_Access               : Boolean;
      Single_Default_Value    : Node_Id;
      Multiple_Default_Value  : List_Id;
      Property_Name_Type      : Node_Id;
      Property_Type_Is_A_List : Boolean;
      Applies_To_All          : Boolean;
      Applies_To              : List_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Name) = K_Identifier);
      pragma Assert (Kind (Property_Set) = K_Property_Set);
      pragma Assert (Safe_XOR (Applies_To_All, (not Is_Empty (Applies_To))));

      Node    : Node_Id;
      Success : Boolean := True;
   begin
      Node := New_Node (K_Property_Definition_Declaration, Loc);
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Is_Inherit (Node, Is_Inherit);
      Set_Is_Access (Node, Is_Access);
      Set_Applies_To (Node, New_Node (K_Applies_To, Loc));
      Set_Is_All
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Applies_To (Node),
         Applies_To_All);
      Set_Owner_Categories
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Applies_To (Node),
         Applies_To);
      Set_Property_Name_Type (Node, New_Node (K_Property_Type, Loc));
      Set_Property_Type_Designator
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Property_Name_Type (Node),
         Property_Name_Type);
      Set_Is_List
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Property_Name_Type (Node),
         Property_Type_Is_A_List);

      if Multiple_Default_Value = No_List
        and then Single_Default_Value = No_Node
      then
         Set_Default_Value (Node, No_Node);
      else
         Set_Default_Value (Node, New_Node (K_Property_Value, Loc));
         Set_Single_Value (Default_Value (Node), Single_Default_Value);
         Set_Multi_Value (Default_Value (Node), Multiple_Default_Value);
         Set_Value_Container (Default_Value (Node), Property_Set);
      end if;

      Success := Add_Property_Definition_Declaration (Property_Set, Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Property_Definition_Declaration;

   ----------------------------------
   -- Add_New_Property_Association --
   ----------------------------------

   function Add_New_Property_Association
     (Loc                 : Location;
      Name                : Node_Id;
      Property_Name       : Node_Id;
      Container           : Node_Id;
      In_Binding          : Node_Id;
      In_Modes            : Node_Id;
      Property_Value      : Node_Id;
      Is_Constant         : Boolean;
      Is_Access           : Boolean;
      Is_Additive         : Boolean;
      Applies_To          : List_Id;
      Check_For_Conflicts : Boolean := False;
      Override            : Boolean := False) return Node_Id
   is
      use Ocarina.Builder.AADL.Components;
      use Ocarina.Builder.AADL.Components.Features;
      use Ocarina.Builder.AADL.Components.Connections;
      use Ocarina.Builder.AADL.Components.Flows;
      use Ocarina.Builder.AADL.Components.Modes;
      use Ocarina.Builder.AADL.Components.Subprogram_Calls;
      use Ocarina.Builder.AADL.Components.Subcomponents;
      use Ocarina.Builder.AADL.Namespaces;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

      pragma Assert (Kind (Name) = K_Identifier);

      pragma Assert
        (Kind (Property_Name) = K_Entity_Reference
         and then Identifier (Property_Name) /= No_Node);

      pragma Assert
        (Kind (Container) = K_Component_Type
         or else Kind (Container) = K_Component_Implementation
         or else Kind (Container) = K_Subcomponent
         or else Kind (Container) = K_Mode
         or else Kind (Container) = K_Flow_Spec
         or else Kind (Container) = K_Flow_Implementation
         or else Kind (Container) = K_Flow_Implementation_Refinement
         or else Kind (Container) = K_End_To_End_Flow_Spec
         or else Kind (Container) = K_End_To_End_Flow_Refinement
         or else Kind (Container) = K_Feature_Group_Type
         or else Kind (Container) = K_Connection
         or else Kind (Container) = K_Subprogram_Call
         or else Kind (Container) = K_Subprogram_Spec
         or else Kind (Container) = K_Port_Spec
         or else Kind (Container) = K_Parameter
         or else Kind (Container) = K_Feature_Group_Spec
         or else Kind (Container) = K_Subcomponent_Access
         or else Kind (Container) = K_Package_Specification);

      Node, Existing_Node  : Node_Id          := No_Node;
      Value_Of_Association : constant Node_Id :=
        New_Node (K_Property_Value, Loc);
      Success       : Boolean := True;
      Property_List : List_Id;
   begin
      if Check_For_Conflicts then
         Property_List :=
           Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Container);
         Existing_Node :=
           Find_Property_Association_From_Name
             (Property_List,
              Ocarina.ME_AADL.AADL_Tree.Nodes.Name (Name));
      end if;

      if Existing_Node = No_Node or else Override then
         Node := New_Node (K_Property_Association, Loc);
         Set_Identifier (Node, Name);
         Set_Corresponding_Entity (Identifier (Node), Node);
         Set_Property_Name (Node, Property_Name);
         Set_Is_Constant (Node, Is_Constant);
         Set_Is_Additive_Association (Node, Is_Additive);
         Set_Is_Access (Node, Is_Access);
         Set_Applies_To_Prop (Node, Applies_To);
         Set_In_Binding (Node, In_Binding);
         Set_In_Modes (Node, In_Modes);
         Set_Is_Private (Node, False);
         Set_Property_Association_Value (Node, Value_Of_Association);
         Set_Value_Container (Value_Of_Association, Container);

         if Property_Value /= No_Node then
            if Kind (Property_Value) = K_Property_List_Value then
               Set_Single_Value (Value_Of_Association, No_Node);
               Set_Multi_Value
                 (Value_Of_Association,
                  List_Id (Property_Value));
            else
               Set_Single_Value (Value_Of_Association, Property_Value);
               Set_Multi_Value (Value_Of_Association, No_List);
            end if;
         else
            Set_Single_Value (Value_Of_Association, No_Node);
            Set_Multi_Value (Value_Of_Association, No_List);
         end if;

         case Kind (Container) is
            when K_Component_Type        |
              K_Component_Implementation |
              K_Feature_Group_Type       =>
               Success :=
                 Ocarina.Builder.AADL.Components.Add_Property_Association
                   (Container,
                    Node);
            when K_Subcomponent =>
               Success :=
                 Ocarina.Builder.AADL.Components.Subcomponents
                   .Add_Property_Association
                   (Container,
                    Node);
            when K_Mode =>
               Success :=
                 Ocarina.Builder.AADL.Components.Modes.Add_Property_Association
                   (Container,
                    Node);
            when K_Flow_Spec                   |
              K_Flow_Implementation            |
              K_Flow_Implementation_Refinement |
              K_End_To_End_Flow_Spec           |
              K_End_To_End_Flow_Refinement     =>
               Success :=
                 Ocarina.Builder.AADL.Components.Flows.Add_Property_Association
                   (Container,
                    Node);
            when K_Connection =>
               Success :=
                 Ocarina.Builder.AADL.Components.Connections
                   .Add_Property_Association
                   (Container,
                    Node);
            when K_Subprogram_Call =>
               Success :=
                 Ocarina.Builder.AADL.Components.Subprogram_Calls
                   .Add_Property_Association
                   (Container,
                    Node);
            when K_Package_Specification =>
               Success :=
                 Ocarina.Builder.AADL.Namespaces.Add_Property_Association
                   (Container,
                    Node);
            when K_Subprogram_Spec  |
              K_Port_Spec           |
              K_Feature_Group_Spec  |
              K_Parameter           |
              K_Subcomponent_Access =>
               Success :=
                 Ocarina.Builder.AADL.Components.Features
                   .Add_Property_Association
                   (Container,
                    Node);
            when others =>
               raise Program_Error;
         end case;
      else
         Success := False;
      end if;

      if Existing_Node /= No_Node and then Override then
         Remove_Node_From_List (Existing_Node, Property_List);
      end if;

      if Success then
         return Node;
      else
         return Existing_Node;
      end if;
   end Add_New_Property_Association;

   ------------------------------------
   -- Add_New_Contained_Element_Path --
   ------------------------------------

   function Add_New_Contained_Element_Path
     (Loc             : Location;
      Container       : Node_Id;
      Applies_To_Elts : List_Id;
      Annex_Path      : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      Node : constant Node_Id := New_Node (K_Contained_Element_Path, Loc);

   begin

      Set_Annex_Path (Node, Annex_Path);
      Set_Container_Component (Node, Container);
      if not Is_Empty (Applies_To_Elts) then
         Set_List_Items (Node, Applies_To_Elts);
      end if;

      return Node;

   end Add_New_Contained_Element_Path;

end Ocarina.Builder.AADL.Properties;
