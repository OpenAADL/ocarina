------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BUILDER.AADL.COMPONENTS.FEATURES                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2018 ESA & ISAE.        --
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

with Ocarina.Builder.AADL.Components;

with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

package body Ocarina.Builder.AADL.Components.Features is

   function Add_New_Feature
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Feature_Kind  : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Is_Refinement : Boolean := False) return Node_Id;
   --  The generic function to create a new feature. This is meant to
   --  be called by Add_New_* functions

   ------------------------------
   -- Add_Property_Association --
   ------------------------------

   function Add_Property_Association
     (Feature              : Node_Id;
      Property_Association : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Kind (Feature) = K_Feature
         or else Kind (Feature) = K_Port_Spec
         or else Kind (Feature) = K_Feature_Group_Spec
         or else Kind (Feature) = K_Subprogram_Spec
         or else Kind (Feature) = K_Parameter
         or else Kind (Feature) = K_Subcomponent_Access);

      pragma Assert (Present (Property_Association));
   begin
      if Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Feature)) then
         Set_Properties
           (Feature,
            New_List (K_List_Id, Loc (Property_Association)));
      end if;

      Append_Node_To_List
        (Property_Association,
         Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Feature));
      return True;
   end Add_Property_Association;

   ---------------------
   -- Add_New_Feature --
   ---------------------

   function Add_New_Feature
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Feature_Kind  : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Is_Refinement : Boolean := False) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Name /= No_Node and then Kind (Name) = K_Identifier);
      pragma Assert
        (Container /= No_Node
         and then
         (Kind (Container) = K_Feature_Group_Type
          or else Kind (Container) = K_Component_Implementation
          or else Kind (Container) = K_Component_Type));
      pragma Assert
        (Feature_Kind = K_Port_Spec
         or else Feature_Kind = K_Feature_Group_Spec
         or else Feature_Kind = K_Subprogram_Spec
         or else Feature_Kind = K_Parameter
         or else Feature_Kind = K_Subcomponent_Access);

      Node    : constant Node_Id := New_Node (Feature_Kind, Loc);
      Success : Boolean;
   begin
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Properties (Node, No_List);
      Set_Property_Scope (Node, New_Node (K_Scope_Definition, Loc));
      Set_Corresponding_Entity (Property_Scope (Node), Node);
      Set_Is_Refinement (Node, Is_Refinement);
      Set_Entity_Ref (Node, No_Node);
      Set_Is_Implicit_Inverse (Node, False);
      Set_Inversed_Entity (Node, Node);
      --  By default, a feature is its own inverse

      if Kind (Container) = K_Component_Type
        or else Kind (Container) = K_Feature_Group_Type
      then
         Success := Add_Feature (Container, Node);
      elsif Kind (Container) = K_Component_Implementation then
         Success := Add_Refined_Type (Container, Node);
      end if;

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Feature;

   -----------------------
   -- Add_New_Port_Spec --
   -----------------------

   function Add_New_Port_Spec
     (Loc               : Location;
      Name              : Node_Id;
      Container         : Node_Id;
      Is_In             : Boolean;
      Is_Out            : Boolean;
      Is_Data           : Boolean;
      Is_Event          : Boolean;
      Is_Feature        : Boolean;
      Is_Refinement     : Boolean := False;
      Associated_Entity : Node_Id := No_Node) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      pragma Assert (Name /= No_Node and then Kind (Name) = K_Identifier);
      pragma Assert (Container /= No_Node);

      Node, Inversed_Node : Node_Id;
   begin
      Node :=
        Add_New_Feature
          (Loc           => Loc,
           Name          => Name,
           Container     => Container,
           Feature_Kind  => K_Port_Spec,
           Is_Refinement => Is_Refinement);

      Set_Is_In (Node, Is_In);
      Set_Is_Out (Node, Is_Out);
      Set_Is_Data (Node, Is_Data);
      Set_Is_Event (Node, Is_Event);
      Set_Is_Feature (Node, Is_Feature);

      Set_Entity_Ref (Node, Associated_Entity);

      --  We only create an inversed feature for in or out features
      --  (not in out)

      if Kind (Container) = K_Feature_Group_Type and then Is_In /= Is_Out then
         --  Port group types can be inversed; hence we add an
         --  implicit inversed port

         Inversed_Node :=
           Add_New_Feature
             (Loc           => Loc,
              Name          => Duplicate_Identifier (Name),
              Container     => Container,
              Feature_Kind  => K_Port_Spec,
              Is_Refinement => Is_Refinement);

         Set_Is_Implicit_Inverse (Inversed_Node, True);
         Set_Inversed_Entity (Node, Inversed_Node);
         Set_Inversed_Entity (Inversed_Node, Node);
         Set_Is_In (Inversed_Node, not Is_In or else Is_Out);
         Set_Is_Out (Inversed_Node, not Is_Out or else Is_In);
         Set_Is_Data (Inversed_Node, Is_Data);
         Set_Is_Event (Inversed_Node, Is_Event);
         Set_Entity_Ref (Inversed_Node, Associated_Entity);
      end if;

      return Node;
   end Add_New_Port_Spec;

   -----------------------------
   -- Add_New_Port_Group_Spec --
   -----------------------------

   function Add_New_Port_Group_Spec
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Name /= No_Node and then Kind (Name) = K_Identifier);
      pragma Assert (Container /= No_Node);
   begin
      return Add_New_Feature
          (Loc,
           Name,
           Container,
           K_Feature_Group_Spec,
           Is_Refinement);

      --  Port group spec are not inversed, since the corresponding
      --  port group type will contain the implicit inversed features.
   end Add_New_Port_Group_Spec;

   --------------------------------
   -- Add_New_Feature_Group_Spec --
   --------------------------------

   function Add_New_Feature_Group_Spec
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Name /= No_Node and then Kind (Name) = K_Identifier);
      pragma Assert (Container /= No_Node);
   begin
      return Add_New_Feature
          (Loc,
           Name,
           Container,
           K_Feature_Group_Spec,
           Is_Refinement);

   end Add_New_Feature_Group_Spec;

   -------------------------------
   -- Add_New_Server_Subprogram --
   -------------------------------

   function Add_New_Server_Subprogram
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Name /= No_Node and then Kind (Name) = K_Identifier);
      pragma Assert (Container /= No_Node);

      Node : Node_Id;
   begin
      Node :=
        Add_New_Feature
          (Loc,
           Name,
           Container,
           K_Subprogram_Spec,
           Is_Refinement);

      Set_Is_Server (Node, True);
      return Node;
   end Add_New_Server_Subprogram;

   ----------------------------------
   -- Add_New_Data_Subprogram_Spec --
   ----------------------------------

   function Add_New_Data_Subprogram_Spec
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Name /= No_Node and then Kind (Name) = K_Identifier);
      pragma Assert (Container /= No_Node);

      Node : Node_Id;
   begin
      Node :=
        Add_New_Feature
          (Loc,
           Name,
           Container,
           K_Subprogram_Spec,
           Is_Refinement);

      Set_Is_Server (Node, False);
      return Node;
   end Add_New_Data_Subprogram_Spec;

   ---------------------------------
   -- Add_New_Subcomponent_Access --
   ---------------------------------

   function Add_New_Subcomponent_Access
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False;
      Category      : Ocarina.ME_AADL.Component_Category;
      Is_Provided   : Boolean) return Node_Id
   is
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Name /= No_Node and then Kind (Name) = K_Identifier);
      pragma Assert (Container /= No_Node);

      Node : Node_Id;
   begin
      Node :=
        Add_New_Feature
          (Loc,
           Name,
           Container,
           K_Subcomponent_Access,
           Is_Refinement);

      if Node /= No_Node then
         Set_Subcomponent_Category (Node, Component_Category'Pos (Category));
         Set_Is_Provided (Node, Is_Provided);
      end if;

      return Node;
   end Add_New_Subcomponent_Access;

   -----------------------
   -- Add_New_Parameter --
   -----------------------

   function Add_New_Parameter
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_In         : Boolean := True;
      Is_Out        : Boolean := True;
      Is_Refinement : Boolean := False) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      pragma Assert (Name /= No_Node and then Kind (Name) = K_Identifier);
      pragma Assert (Container /= No_Node);

      Node, Inversed_Node : Node_Id;
   begin
      Node :=
        Add_New_Feature (Loc, Name, Container, K_Parameter, Is_Refinement);

      Set_Is_In (Node, Is_In);
      Set_Is_Out (Node, Is_Out);

      --  We only create an inversed feature for in or out features
      --  (not in out)

      if Kind (Container) = K_Feature_Group_Type and then Is_In /= Is_Out then
         --  Port group types can be inversed; hence we add an
         --  implicit inversed parameter

         Inversed_Node :=
           Add_New_Feature
             (Loc           => Loc,
              Name          => Duplicate_Identifier (Name),
              Container     => Container,
              Feature_Kind  => K_Parameter,
              Is_Refinement => Is_Refinement);

         Set_Is_Implicit_Inverse (Inversed_Node, True);
         Set_Inversed_Entity (Inversed_Node, Node);
         Set_Inversed_Entity (Node, Inversed_Node);
         Set_Is_In (Inversed_Node, not Is_In or else Is_Out);
         Set_Is_Out (Inversed_Node, not Is_Out or else Is_In);
      end if;

      return Node;
   end Add_New_Parameter;

end Ocarina.Builder.AADL.Components.Features;
