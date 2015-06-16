------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B U I L D E R . A A D L . C O M P O N E N T S       --
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

with Ocarina.Builder.AADL.Namespaces;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

package body Ocarina.Builder.AADL.Components is

   ---------------
   -- Add_Annex --
   ---------------

   function Add_Annex (Component : Node_Id; Annex : Node_Id) return Boolean is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Package_Specification
         or else Kind (Component) = K_Component_Type
         or else Kind (Component) = K_Feature_Group_Type);
      pragma Assert (Present (Annex));
   begin
      if Is_Empty (Annexes (Component)) then
         Set_Annexes (Component, New_List (K_List_Id, Loc (Annex)));
      end if;

      Append_Node_To_List (Annex, Annexes (Component));
      return True;
   end Add_Annex;

   --------------------
   -- Add_Connection --
   --------------------

   function Add_Connection
     (Component  : Node_Id;
      Connection : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Present (Component)
         and then Kind (Component) = K_Component_Implementation);
      pragma Assert (Present (Connection));
   begin
      if Connections (Component) = No_List then
         Set_Connections (Component, New_List (K_List_Id, Loc (Connection)));
      end if;

      Append_Node_To_List (Connection, Connections (Component));
      Set_Container_Component (Connection, Component);
      return True;
   end Add_Connection;

   -----------------
   -- Add_Feature --
   -----------------

   function Add_Feature
     (Component : Node_Id;
      Feature   : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Kind (Component) = K_Component_Type
         or else Kind (Component) = K_Feature_Group_Type);

      pragma Assert (Present (Feature));
   begin
      if Features (Component) = No_List then
         Set_Features (Component, New_List (K_List_Id, Loc (Feature)));
      end if;

      Append_Node_To_List (Feature, Features (Component));
      Set_Container_Component (Feature, Component);
      return True;
   end Add_Feature;

   ----------------------
   -- Add_Refined_Type --
   ----------------------

   function Add_Refined_Type
     (Component    : Node_Id;
      Refined_Type : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Present (Refined_Type));
   begin
      if Refines_Type (Component) = No_List then
         Set_Refines_Type
           (Component,
            New_List (K_List_Id, Loc (Refined_Type)));
      end if;

      Append_Node_To_List (Refined_Type, Refines_Type (Component));
      return True;
   end Add_Refined_Type;

   ----------------------
   -- Add_Subcomponent --
   ----------------------

   function Add_Subcomponent
     (Component    : Node_Id;
      Subcomponent : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Present (Subcomponent));
   begin
      if Subcomponents (Component) = No_List then
         Set_Subcomponents
           (Component,
            New_List (K_List_Id, Loc (Subcomponent)));
      end if;

      Append_Node_To_List (Subcomponent, Subcomponents (Component));
      Set_Container_Component (Subcomponent, Component);
      return True;
   end Add_Subcomponent;

   -------------------
   -- Add_Prototype --
   -------------------

   function Add_Prototype
     (Component : Node_Id;
      Prototype : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type
         or else Kind (Component) = K_Feature_Group_Type);
      pragma Assert (Present (Prototype));
   begin
      if Prototypes (Component) = No_List then
         Set_Prototypes (Component, New_List (K_List_Id, Loc (Prototype)));
      end if;

      Append_Node_To_List (Prototype, Prototypes (Component));
      Set_Container_Component (Prototype, Component);
      return True;
   end Add_Prototype;

   ----------------------------------
   -- Add_Subprogram_Call_Sequence --
   ----------------------------------

   function Add_Subprogram_Call_Sequence
     (Component     : Node_Id;
      Call_Sequence : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Present (Call_Sequence));
   begin
      if Calls (Component) = No_List then
         Set_Calls (Component, New_List (K_List_Id, Loc (Call_Sequence)));
      end if;

      Append_Node_To_List (Call_Sequence, Calls (Component));
      Set_Container_Component (Call_Sequence, Component);
      return True;
   end Add_Subprogram_Call_Sequence;

   -------------------
   -- Add_Flow_Spec --
   -------------------

   function Add_Flow_Spec
     (Component : Node_Id;
      Flow_Spec : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Component) = K_Component_Type);
      pragma Assert (Present (Flow_Spec));
   begin
      if Flows (Component) = No_List then
         Set_Flows (Component, New_List (K_List_Id, Loc (Flow_Spec)));
      end if;

      Append_Node_To_List (Flow_Spec, Flows (Component));
      Set_Container_Component (Flow_Spec, Component);
      return True;
   end Add_Flow_Spec;

   -----------------------------
   -- Add_Flow_Implementation --
   -----------------------------

   function Add_Flow_Implementation
     (Component : Node_Id;
      Flow_Impl : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Present (Flow_Impl));
   begin
      if Flows (Component) = No_List then
         Set_Flows (Component, New_List (K_List_Id, Loc (Flow_Impl)));
      end if;

      Append_Node_To_List (Flow_Impl, Flows (Component));
      Set_Container_Component (Flow_Impl, Component);
      return True;
   end Add_Flow_Implementation;

   ------------------------------
   -- Add_End_To_End_Flow_Spec --
   ------------------------------

   function Add_End_To_End_Flow_Spec
     (Component       : Node_Id;
      End_To_End_Flow : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Present (End_To_End_Flow));
   begin
      if Flows (Component) = No_List then
         Set_Flows (Component, New_List (K_List_Id, Loc (End_To_End_Flow)));
      end if;

      Append_Node_To_List (End_To_End_Flow, Flows (Component));
      Set_Container_Component (End_To_End_Flow, Component);
      return True;
   end Add_End_To_End_Flow_Spec;

   --------------
   -- Add_Mode --
   --------------

   function Add_Mode (Component : Node_Id; Mode : Node_Id) return Boolean is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type);
      pragma Assert (Present (Mode));
   begin
      if Ocarina.ME_AADL.AADL_Tree.Nodes.Modes (Component) = No_List then
         Set_Modes (Component, New_List (K_List_Id, Loc (Mode)));
      end if;

      Append_Node_To_List
        (Mode,
         Ocarina.ME_AADL.AADL_Tree.Nodes.Modes (Component));
      Set_Container_Component (Mode, Component);
      return True;
   end Add_Mode;

   ------------------------------
   -- Add_Property_Association --
   ------------------------------

   function Add_Property_Association
     (Component            : Node_Id;
      Property_Association : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type
         or else Kind (Component) = K_Feature_Group_Type);
      pragma Assert (Present (Property_Association));
   begin
      if Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Component)) then
         Set_Properties
           (Component,
            New_List (K_List_Id, Loc (Property_Association)));
      end if;

      Append_Node_To_List
        (Property_Association,
         Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Component));
      return True;
   end Add_Property_Association;

   ----------------------------
   -- Add_New_Component_Type --
   ----------------------------

   function Add_New_Component_Type
     (Loc            : Location;
      Identifier     : Node_Id;
      Namespace      : Node_Id;
      Component_Type : Ocarina.ME_AADL.Component_Category;
      Is_Private     : Boolean := False) return Node_Id
   is
      use Ocarina.Builder.AADL.Namespaces;
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Present (Identifier));
      pragma Assert
        (Kind (Namespace) = K_Package_Specification
         or else Kind (Namespace) = K_AADL_Specification);

      Node          : constant Node_Id := New_Node (K_Component_Type, Loc);
      Entity_Scop   : constant Node_Id := New_Node (K_Scope_Definition, Loc);
      Property_Scop : constant Node_Id := New_Node (K_Scope_Definition, Loc);
      Success       : Boolean          := True;
   begin
      Set_Category (Node, Component_Category'Pos (Component_Type));
      Set_Identifier (Node, Identifier);
      Set_Corresponding_Entity (Identifier, Node);
      Set_Instances (Node, No_List);
      Set_Namespace (Node, Namespace);

      Set_Features (Node, No_List);
      Set_Flows (Node, No_List);
      Set_Properties (Node, No_List);
      Set_Annexes (Node, No_List);
      Set_Modes (Node, No_List);

      Set_Entity_Scope (Node, Entity_Scop);
      Set_Property_Scope (Node, Property_Scop);
      Set_Corresponding_Entity (Entity_Scop, Node);
      Set_Corresponding_Entity (Property_Scop, Node);

      Set_Is_Private (Node, Is_Private);

      Success := Add_Declaration (Namespace => Namespace, Element => Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Component_Type;

   --------------------------------------
   -- Add_New_Component_Implementation --
   --------------------------------------

   function Add_New_Component_Implementation
     (Loc            : Location;
      Identifier     : Node_Id;
      Namespace      : Node_Id;
      Component_Type : Ocarina.ME_AADL.Component_Category;
      Is_Private     : Boolean := False) return Node_Id
   is
      use Ocarina.Builder.AADL.Namespaces;
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Present (Identifier));
      pragma Assert
        (Kind (Namespace) = K_Package_Specification
         or else Kind (Namespace) = K_AADL_Specification);

      Entity_Scop   : constant Node_Id := New_Node (K_Scope_Definition, Loc);
      Property_Scop : constant Node_Id := New_Node (K_Scope_Definition, Loc);
      Node : constant Node_Id := New_Node (K_Component_Implementation, Loc);
      Success       : Boolean          := True;
   begin
      Set_Category (Node, Component_Category'Pos (Component_Type));
      Set_Parent (Node, No_Node);
      Set_Identifier (Node, Identifier);
      Set_Corresponding_Entity (Identifier, Node);
      Set_Instances (Node, No_List);
      Set_Namespace (Node, Namespace);

      Set_Refines_Type (Node, No_List);
      Set_Subcomponents (Node, No_List);
      Set_Calls (Node, No_List);
      Set_Connections (Node, No_List);
      Set_Flows (Node, No_List);
      Set_Modes (Node, No_List);
      Set_Properties (Node, No_List);
      Set_Annexes (Node, No_List);

      Set_Entity_Scope (Node, Entity_Scop);
      Set_Property_Scope (Node, Property_Scop);
      Set_Corresponding_Entity (Entity_Scop, Node);
      Set_Corresponding_Entity (Property_Scop, Node);

      Set_Is_Private (Node, Is_Private);

      Success := Add_Declaration (Namespace => Namespace, Element => Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Component_Implementation;

   ---------------------------
   -- Add_New_Feature_Group --
   ---------------------------

   function Add_New_Feature_Group
     (Loc        : Location;
      Name       : Node_Id;
      Namespace  : Node_Id;
      Is_Private : Boolean := False) return Node_Id
   is
      use Ocarina.Builder.AADL.Namespaces;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Name) = K_Identifier);
      pragma Assert
        (Kind (Namespace) = K_Package_Specification
         or else Kind (Namespace) = K_AADL_Specification);

      Node          : constant Node_Id := New_Node (K_Feature_Group_Type, Loc);
      Property_Scop : constant Node_Id := New_Node (K_Scope_Definition, Loc);
      Entity_Scop   : constant Node_Id := New_Node (K_Scope_Definition, Loc);
      Success       : Boolean          := True;
   begin
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Features (Node, No_List);
      Set_Inverse_Of (Node, No_Node);
      Set_Properties (Node, No_List);
      Set_Annexes (Node, No_List);
      Set_Namespace (Node, Namespace);

      Set_Entity_Scope (Node, Entity_Scop);
      Set_Property_Scope (Node, Property_Scop);

      Set_Corresponding_Entity (Entity_Scop, Node);
      Set_Corresponding_Entity (Property_Scop, Node);

      Set_Parent (Node, No_Node);
      Set_Is_Private (Node, Is_Private);

      Success := Add_Declaration (Namespace, Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Feature_Group;

end Ocarina.Builder.AADL.Components;
