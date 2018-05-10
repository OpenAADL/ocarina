------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.BUILDER.AADL.COMPONENTS.SUBCOMPONENTS               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

package body Ocarina.Builder.AADL.Components.Subcomponents is

   ------------------------------
   -- Add_Property_Association --
   ------------------------------

   function Add_Property_Association
     (Subcomponent         : Node_Id;
      Property_Association : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Subcomponent) = K_Subcomponent);
      pragma Assert (Present (Property_Association));
   begin
      if Is_Empty
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Subcomponent))
      then
         Set_Properties
           (Subcomponent,
            New_List (K_List_Id, Loc (Property_Association)));
      end if;

      Append_Node_To_List
        (Property_Association,
         Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Subcomponent));
      return True;
   end Add_Property_Association;

   --------------------------
   -- Add_New_Subcomponent --
   --------------------------

   function Add_New_Subcomponent
     (Loc                 : Location;
      Name                : Node_Id;
      Comp_Impl           : Node_Id;
      Category            : Ocarina.ME_AADL.Component_Category;
      Is_Refinement       : Boolean := False;
      In_Modes            : Node_Id := No_Node;
      Prototypes_Bindings : List_Id := No_List;
      Entity_Ref          : Node_Id := No_Node) return Node_Id
   is
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Comp_Impl /= No_Node
         and then Kind (Comp_Impl) = K_Component_Implementation);

      Node      : Node_Id;
      List_Node : Node_Id;
      Success   : Boolean := True;

   begin
      Node := New_Node (K_Subcomponent, Loc);
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Is_Refinement (Node, Is_Refinement);
      Set_Category (Node, Component_Category'Pos (Category));
      Set_In_Modes (Node, In_Modes);
      Set_Property_Scope (Node, New_Node (K_Scope_Definition, Loc));
      Set_Corresponding_Entity (Property_Scope (Node), Node);
      Set_Entity_Ref (Node, Entity_Ref);

      Set_Prototype_Bindings (Node, Prototypes_Bindings);
      if Prototypes_Bindings /= No_List then
         List_Node := First_Node (Prototypes_Bindings);
         while Present (List_Node) loop
            Set_Container_Component (List_Node, Node);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Success :=
        Add_Subcomponent (Component => Comp_Impl, Subcomponent => Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;

   end Add_New_Subcomponent;

end Ocarina.Builder.AADL.Components.Subcomponents;
