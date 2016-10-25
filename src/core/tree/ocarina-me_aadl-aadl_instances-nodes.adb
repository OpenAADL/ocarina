pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.ME_AADL.AADL_Instances.Debug; use Ocarina.ME_AADL.AADL_Instances.Debug;

package body Ocarina.ME_AADL.AADL_Instances.Nodes is

   pragma Warnings (Off);
   use Entries;

   function Kind (N : Node_Id) return Node_Kind is
   begin
      return Table (Types.Node_Id (N)).Kind;
   end Kind;

   procedure Set_Kind (N : Node_Id; V : Node_Kind) is
   begin
      Table (Types.Node_Id (N)).Kind := V;
   end Set_Kind;

   function Loc (N : Node_Id) return Location is
   begin
      return Table (Types.Node_Id (N)).Loc;
   end Loc;

   procedure Set_Loc (N : Node_Id; V : Location) is
   begin
      Table (Types.Node_Id (N)).Loc := V;
   end Set_Loc;

   function Next_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container
        or else Table (Types.Node_Id (N)).Kind = K_Architecture_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container
        or else Table (Types.Node_Id (N)).Kind = K_Architecture_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Next_Node;

   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;

   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;

   function Next_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Next_Entity;

   procedure Set_Next_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Next_Entity;

   function Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Identifier;

   procedure Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Identifier;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (2));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Name;

   function Display_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (3));
   end Display_Name;

   procedure Set_Display_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Display_Name;

   function Corresponding_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Corresponding_Entity;

   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Corresponding_Entity;

   function Scope_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Scope_Entity;

   procedure Set_Scope_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Scope_Entity;

   function Homonym (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Homonym;

   procedure Set_Homonym (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Homonym;

   function Visible (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Visible;

   procedure Set_Visible (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Visible;

   function Backend_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Backend_Node;

   procedure Set_Backend_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Backend_Node;

   function Item (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Item;

   procedure Set_Item (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Item;

   function Extra_Item (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Extra_Item;

   procedure Set_Extra_Item (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Extra_Item;

   function Root_System (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Architecture_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Root_System;

   procedure Set_Root_System (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Architecture_Instance);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Root_System;

   function Namespaces (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Architecture_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Namespaces;

   procedure Set_Namespaces (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Architecture_Instance);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Namespaces;

   function Unnamed_Namespace (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Architecture_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Unnamed_Namespace;

   procedure Set_Unnamed_Namespace (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Architecture_Instance);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Unnamed_Namespace;

   function Corresponding_Declaration (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Corresponding_Declaration;

   procedure Set_Corresponding_Declaration (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Corresponding_Declaration;

   function Path (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Path;

   procedure Set_Path (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Path;

   function Properties (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (9));
   end Properties;

   procedure Set_Properties (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Properties;

   function Is_Private (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Private;

   procedure Set_Is_Private (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Private;

   function Features (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Features;

   procedure Set_Features (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Features;

   function Subcomponents (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Subcomponents;

   procedure Set_Subcomponents (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Subcomponents;

   function Modes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Modes;

   procedure Set_Modes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Modes;

   function Mode_transitions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (10));
   end Mode_transitions;

   procedure Set_Mode_transitions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Mode_transitions;

   function Connections (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (11));
   end Connections;

   procedure Set_Connections (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Connections;

   function Calls (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (12));
   end Calls;

   procedure Set_Calls (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Calls;

   function Flows (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (13));
   end Flows;

   procedure Set_Flows (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_Flows;

   function Annexes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (14));
   end Annexes;

   procedure Set_Annexes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_Annexes;

   function Parent_Subcomponent (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (15));
   end Parent_Subcomponent;

   procedure Set_Parent_Subcomponent (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_Parent_Subcomponent;

   function Namespace (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (16));
   end Namespace;

   procedure Set_Namespace (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Instance);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Namespace;

   function Parent_Component (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Parent_Component;

   procedure Set_Parent_Component (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Parent_Component;

   function Corresponding_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Corresponding_Instance;

   procedure Set_Corresponding_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Corresponding_Instance;

   function Destinations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (12));
   end Destinations;

   procedure Set_Destinations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Destinations;

   function In_Modes (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (13));
   end In_Modes;

   procedure Set_In_Modes (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_In_Modes;

   function Declarations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Declarations;

   procedure Set_Declarations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Namespace_Instance);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Declarations;

   function Sources (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (13));
   end Sources;

   procedure Set_Sources (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_Sources;

   function Is_In (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_In;

   procedure Set_Is_In (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_In;

   function Is_Out (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Out;

   procedure Set_Is_Out (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Instance);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Out;

   function Is_Event (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Event;

   procedure Set_Is_Event (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Event;

   function Is_Data (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end Is_Data;

   procedure Set_Is_Data (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_Is_Data;

   function Is_Server (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Server;

   procedure Set_Is_Server (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec_Instance);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Server;

   function Is_Provided (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Provided;

   procedure Set_Is_Provided (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access_Instance);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Provided;

   function Subprogram_Calls (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Subprogram_Calls;

   procedure Set_Subprogram_Calls (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Instance);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Subprogram_Calls;

   function Parent_Sequence (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Parent_Sequence;

   procedure Set_Parent_Sequence (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Call_Instance);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Parent_Sequence;

   function Is_Initial (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Initial;

   procedure Set_Is_Initial (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Instance);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Initial;

   function Source_Modes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Source_Modes;

   procedure Set_Source_Modes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Source_Modes;

   function Triggers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Triggers;

   procedure Set_Triggers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Triggers;

   function Destination_Mode (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Destination_Mode;

   procedure Set_Destination_Mode (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Instance);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Destination_Mode;

   function Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Source;

   procedure Set_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Source;

   function Destination (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Destination;

   procedure Set_Destination (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Destination;

   function Associated_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Associated_Type;

   procedure Set_Associated_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Instance);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Associated_Type;

   function Property_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Property_Name;

   procedure Set_Property_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Property_Name;

   function Is_Additive_Association (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Additive_Association;

   procedure Set_Is_Additive_Association (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Additive_Association;

   function Is_Constant (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Constant;

   procedure Set_Is_Constant (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Constant;

   function Is_Access (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end Is_Access;

   procedure Set_Is_Access (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_Is_Access;

   function Property_Association_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Property_Association_Type;

   procedure Set_Property_Association_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Property_Association_Type;

   function Property_Association_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Property_Association_Value;

   procedure Set_Property_Association_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Property_Association_Value;

   function Applies_To_Prop (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return List_Id (Table (Types.Node_Id (N)).L (12));
   end Applies_To_Prop;

   procedure Set_Applies_To_Prop (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Applies_To_Prop;

   function In_Binding (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (14));
   end In_Binding;

   procedure Set_In_Binding (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association_Instance);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_In_Binding;

   function Corresponding_Annex (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Corresponding_Annex;

   procedure Set_Corresponding_Annex (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Instance);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Corresponding_Annex;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_AADL_Entity =>
            W_AADL_Entity
              (Node_Id (N));
         when K_Named_AADL_Entity =>
            W_Named_AADL_Entity
              (Node_Id (N));
         when K_Identifier =>
            W_Identifier
              (Node_Id (N));
         when K_Node_Container =>
            W_Node_Container
              (Node_Id (N));
         when K_Architecture_Instance =>
            W_Architecture_Instance
              (Node_Id (N));
         when K_Entity_Instance =>
            W_Entity_Instance
              (Node_Id (N));
         when K_Entity_Reference_Instance =>
            W_Entity_Reference_Instance
              (Node_Id (N));
         when K_Declaration_Instance =>
            W_Declaration_Instance
              (Node_Id (N));
         when K_Component_Instance =>
            W_Component_Instance
              (Node_Id (N));
         when K_Subcomponent_Instance =>
            W_Subcomponent_Instance
              (Node_Id (N));
         when K_Namespace_Instance =>
            W_Namespace_Instance
              (Node_Id (N));
         when K_Feature_Instance =>
            W_Feature_Instance
              (Node_Id (N));
         when K_Port_Spec_Instance =>
            W_Port_Spec_Instance
              (Node_Id (N));
         when K_Feature_Group_Spec_Instance =>
            W_Feature_Group_Spec_Instance
              (Node_Id (N));
         when K_Subprogram_Spec_Instance =>
            W_Subprogram_Spec_Instance
              (Node_Id (N));
         when K_Parameter_Instance =>
            W_Parameter_Instance
              (Node_Id (N));
         when K_Subcomponent_Access_Instance =>
            W_Subcomponent_Access_Instance
              (Node_Id (N));
         when K_Call_Sequence_Instance =>
            W_Call_Sequence_Instance
              (Node_Id (N));
         when K_Call_Instance =>
            W_Call_Instance
              (Node_Id (N));
         when K_Mode_Instance =>
            W_Mode_Instance
              (Node_Id (N));
         when K_Mode_Transition_Instance =>
            W_Mode_Transition_Instance
              (Node_Id (N));
         when K_Connection_Instance =>
            W_Connection_Instance
              (Node_Id (N));
         when K_Property_Association_Instance =>
            W_Property_Association_Instance
              (Node_Id (N));
         when K_Annex_Instance =>
            W_Annex_Instance
              (Node_Id (N));
         when others =>
            null;
      end case;
   end W_Node;

   procedure W_AADL_Entity (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
   end W_AADL_Entity;

   procedure W_Named_AADL_Entity (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
   end W_Named_AADL_Entity;

   procedure W_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Display_Name",
         "Name_Id",
         Image (Display_Name (N)));
      W_Node_Attribute
        ("Corresponding_Entity",
         "Node_Id",
         Image (Corresponding_Entity (N)),
         Int (Corresponding_Entity (N)));
      W_Node_Attribute
        ("Scope_Entity",
         "Node_Id",
         Image (Scope_Entity (N)),
         Int (Scope_Entity (N)));
      W_Node_Attribute
        ("Homonym",
         "Node_Id",
         Image (Homonym (N)),
         Int (Homonym (N)));
      W_Node_Attribute
        ("Visible",
         "Boolean",
         Image (Visible (N)));
      W_Node_Attribute
        ("Backend_Node",
         "Node_Id",
         Image (Backend_Node (N)),
         Int (Backend_Node (N)));
   end W_Identifier;

   procedure W_Node_Container (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Item",
         "Node_Id",
         Image (Item (N)),
         Int (Item (N)));
      W_Node_Attribute
        ("Extra_Item",
         "Node_Id",
         Image (Extra_Item (N)),
         Int (Extra_Item (N)));
   end W_Node_Container;

   procedure W_Architecture_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Root_System",
         "Node_Id",
         Image (Root_System (N)),
         Int (Root_System (N)));
      W_Node_Attribute
        ("Namespaces",
         "List_Id",
         Image (Namespaces (N)),
         Int (Namespaces (N)));
      W_Node_Attribute
        ("Unnamed_Namespace",
         "Node_Id",
         Image (Unnamed_Namespace (N)),
         Int (Unnamed_Namespace (N)));
   end W_Architecture_Instance;

   procedure W_Entity_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
   end W_Entity_Instance;

   procedure W_Entity_Reference_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Path",
         "List_Id",
         Image (Path (N)),
         Int (Path (N)));
   end W_Entity_Reference_Instance;

   procedure W_Declaration_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
   end W_Declaration_Instance;

   procedure W_Component_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("Features",
         "List_Id",
         Image (Features (N)),
         Int (Features (N)));
      W_Node_Attribute
        ("Subcomponents",
         "List_Id",
         Image (Subcomponents (N)),
         Int (Subcomponents (N)));
      W_Node_Attribute
        ("Modes",
         "List_Id",
         Image (Modes (N)),
         Int (Modes (N)));
      W_Node_Attribute
        ("Mode_transitions",
         "List_Id",
         Image (Mode_transitions (N)),
         Int (Mode_transitions (N)));
      W_Node_Attribute
        ("Connections",
         "List_Id",
         Image (Connections (N)),
         Int (Connections (N)));
      W_Node_Attribute
        ("Calls",
         "List_Id",
         Image (Calls (N)),
         Int (Calls (N)));
      W_Node_Attribute
        ("Flows",
         "List_Id",
         Image (Flows (N)),
         Int (Flows (N)));
      W_Node_Attribute
        ("Annexes",
         "List_Id",
         Image (Annexes (N)),
         Int (Annexes (N)));
      W_Node_Attribute
        ("Parent_Subcomponent",
         "Node_Id",
         Image (Parent_Subcomponent (N)),
         Int (Parent_Subcomponent (N)));
      W_Node_Attribute
        ("Namespace",
         "Node_Id",
         Image (Namespace (N)),
         Int (Namespace (N)));
   end W_Component_Instance;

   procedure W_Subcomponent_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("Corresponding_Instance",
         "Node_Id",
         Image (Corresponding_Instance (N)),
         Int (Corresponding_Instance (N)));
      W_Node_Attribute
        ("Destinations",
         "List_Id",
         Image (Destinations (N)),
         Int (Destinations (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
   end W_Subcomponent_Instance;

   procedure W_Namespace_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
   end W_Namespace_Instance;

   procedure W_Feature_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("Destinations",
         "List_Id",
         Image (Destinations (N)),
         Int (Destinations (N)));
      W_Node_Attribute
        ("Sources",
         "List_Id",
         Image (Sources (N)),
         Int (Sources (N)));
   end W_Feature_Instance;

   procedure W_Port_Spec_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("Corresponding_Instance",
         "Node_Id",
         Image (Corresponding_Instance (N)),
         Int (Corresponding_Instance (N)));
      W_Node_Attribute
        ("Destinations",
         "List_Id",
         Image (Destinations (N)),
         Int (Destinations (N)));
      W_Node_Attribute
        ("Sources",
         "List_Id",
         Image (Sources (N)),
         Int (Sources (N)));
      W_Node_Attribute
        ("Is_In",
         "Boolean",
         Image (Is_In (N)));
      W_Node_Attribute
        ("Is_Out",
         "Boolean",
         Image (Is_Out (N)));
      W_Node_Attribute
        ("Is_Event",
         "Boolean",
         Image (Is_Event (N)));
      W_Node_Attribute
        ("Is_Data",
         "Boolean",
         Image (Is_Data (N)));
   end W_Port_Spec_Instance;

   procedure W_Feature_Group_Spec_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Features",
         "List_Id",
         Image (Features (N)),
         Int (Features (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("Destinations",
         "List_Id",
         Image (Destinations (N)),
         Int (Destinations (N)));
      W_Node_Attribute
        ("Sources",
         "List_Id",
         Image (Sources (N)),
         Int (Sources (N)));
   end W_Feature_Group_Spec_Instance;

   procedure W_Subprogram_Spec_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("Corresponding_Instance",
         "Node_Id",
         Image (Corresponding_Instance (N)),
         Int (Corresponding_Instance (N)));
      W_Node_Attribute
        ("Destinations",
         "List_Id",
         Image (Destinations (N)),
         Int (Destinations (N)));
      W_Node_Attribute
        ("Sources",
         "List_Id",
         Image (Sources (N)),
         Int (Sources (N)));
      W_Node_Attribute
        ("Is_Server",
         "Boolean",
         Image (Is_Server (N)));
   end W_Subprogram_Spec_Instance;

   procedure W_Parameter_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("Corresponding_Instance",
         "Node_Id",
         Image (Corresponding_Instance (N)),
         Int (Corresponding_Instance (N)));
      W_Node_Attribute
        ("Destinations",
         "List_Id",
         Image (Destinations (N)),
         Int (Destinations (N)));
      W_Node_Attribute
        ("Sources",
         "List_Id",
         Image (Sources (N)),
         Int (Sources (N)));
      W_Node_Attribute
        ("Is_In",
         "Boolean",
         Image (Is_In (N)));
      W_Node_Attribute
        ("Is_Out",
         "Boolean",
         Image (Is_Out (N)));
   end W_Parameter_Instance;

   procedure W_Subcomponent_Access_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("Corresponding_Instance",
         "Node_Id",
         Image (Corresponding_Instance (N)),
         Int (Corresponding_Instance (N)));
      W_Node_Attribute
        ("Destinations",
         "List_Id",
         Image (Destinations (N)),
         Int (Destinations (N)));
      W_Node_Attribute
        ("Sources",
         "List_Id",
         Image (Sources (N)),
         Int (Sources (N)));
      W_Node_Attribute
        ("Is_Data",
         "Boolean",
         Image (Is_Data (N)));
      W_Node_Attribute
        ("Is_Provided",
         "Boolean",
         Image (Is_Provided (N)));
   end W_Subcomponent_Access_Instance;

   procedure W_Call_Sequence_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Subprogram_Calls",
         "List_Id",
         Image (Subprogram_Calls (N)),
         Int (Subprogram_Calls (N)));
   end W_Call_Sequence_Instance;

   procedure W_Call_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Path",
         "List_Id",
         Image (Path (N)),
         Int (Path (N)));
      W_Node_Attribute
        ("Corresponding_Instance",
         "Node_Id",
         Image (Corresponding_Instance (N)),
         Int (Corresponding_Instance (N)));
      W_Node_Attribute
        ("Parent_Sequence",
         "Node_Id",
         Image (Parent_Sequence (N)),
         Int (Parent_Sequence (N)));
   end W_Call_Instance;

   procedure W_Mode_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("Is_Initial",
         "Boolean",
         Image (Is_Initial (N)));
   end W_Mode_Instance;

   procedure W_Mode_Transition_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("Source_Modes",
         "List_Id",
         Image (Source_Modes (N)),
         Int (Source_Modes (N)));
      W_Node_Attribute
        ("Triggers",
         "List_Id",
         Image (Triggers (N)),
         Int (Triggers (N)));
      W_Node_Attribute
        ("Destination_Mode",
         "Node_Id",
         Image (Destination_Mode (N)),
         Int (Destination_Mode (N)));
   end W_Mode_Transition_Instance;

   procedure W_Connection_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent_Component",
         "Node_Id",
         Image (Parent_Component (N)),
         Int (Parent_Component (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Source",
         "Node_Id",
         Image (Source (N)),
         Int (Source (N)));
      W_Node_Attribute
        ("Destination",
         "Node_Id",
         Image (Destination (N)),
         Int (Destination (N)));
      W_Node_Attribute
        ("Associated_Type",
         "Node_Id",
         Image (Associated_Type (N)),
         Int (Associated_Type (N)));
   end W_Connection_Instance;

   procedure W_Property_Association_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Corresponding_Declaration",
         "Node_Id",
         Image (Corresponding_Declaration (N)),
         Int (Corresponding_Declaration (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Property_Name",
         "Node_Id",
         Image (Property_Name (N)),
         Int (Property_Name (N)));
      W_Node_Attribute
        ("Is_Additive_Association",
         "Boolean",
         Image (Is_Additive_Association (N)));
      W_Node_Attribute
        ("Is_Constant",
         "Boolean",
         Image (Is_Constant (N)));
      W_Node_Attribute
        ("Is_Access",
         "Boolean",
         Image (Is_Access (N)));
      W_Node_Attribute
        ("Property_Association_Type",
         "Node_Id",
         Image (Property_Association_Type (N)),
         Int (Property_Association_Type (N)));
      W_Node_Attribute
        ("Property_Association_Value",
         "Node_Id",
         Image (Property_Association_Value (N)),
         Int (Property_Association_Value (N)));
      W_Node_Attribute
        ("Applies_To_Prop",
         "List_Id",
         Image (Applies_To_Prop (N)),
         Int (Applies_To_Prop (N)));
      W_Node_Attribute
        ("In_Binding",
         "Node_Id",
         Image (In_Binding (N)),
         Int (In_Binding (N)));
   end W_Property_Association_Instance;

   procedure W_Annex_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Corresponding_Annex",
         "Node_Id",
         Image (Corresponding_Annex (N)),
         Int (Corresponding_Annex (N)));
   end W_Annex_Instance;

end Ocarina.ME_AADL.AADL_Instances.Nodes;
