------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BUILDER.AADL.COMPONENTS.PROTOTYPES                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2012 ESA & ISAE.        --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

package body Ocarina.Builder.AADL.Components.Prototypes is

   use Ocarina.ME_AADL;

   -----------------------
   -- Add_New_Prototype --
   -----------------------

   function Add_New_Prototype
     (Loc            : Location;
      Name           : Node_Id;
      Container      : Node_Id;
      Classifier_Ref : Node_Id;
      Category       : Ocarina.ME_AADL.Component_Category;
      Is_Refinement  : Boolean := False)
     return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Container /= No_Node
               and then (Kind (Container) = K_Component_Implementation
                           or else Kind (Container) = K_Component_Type
                           or else Kind (Container) = K_Feature_Group_Type));

      Node : constant Node_Id := New_Node (K_Prototype, Loc);
      Success : Boolean := True;
   begin
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Is_Refinement (Node, Is_Refinement);

      Set_Entity_Ref (Node, Classifier_Ref);
      Set_Category (Node, Component_Category'Pos (Category));

      Set_Property_Scope (Node, New_Node (K_Scope_Definition, Loc));
      Set_Corresponding_Entity (Property_Scope (Node), Node);

      Success := Add_Prototype (Component => Container,
                                Prototype => Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;

   end Add_New_Prototype;

   -------------------------------
   -- Add_New_Prototype_Binding --
   -------------------------------

   function Add_New_Prototype_Binding
     (Loc            : Location;
      Name           : Node_Id;
      Container      : Node_Id;
      Classifier_Ref : Node_Id;
      Category       : Ocarina.ME_AADL.Component_Category)
     return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      Node : constant Node_Id := New_Node (K_Binding_Prototype, Loc);
   begin
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Container_Component (Node, Container);
      Set_Entity_Ref (Node, Classifier_Ref);
      Set_Category (Node, Component_Category'Pos (Category));

      return Node;

   end Add_New_Prototype_Binding;

end Ocarina.Builder.AADL.Components.Prototypes;
