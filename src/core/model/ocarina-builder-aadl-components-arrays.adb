------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL.COMPONENTS.ARRAYS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

package body Ocarina.Builder.AADL.Components.Arrays is

   ----------------------------------
   -- Add_New_Array_Dimension_Size --
   ----------------------------------

   function Add_New_Array_Dimension_Size
     (Loc            : Location;
      Container      : Node_Id;
      Dimension_Size : Node_Id)
     return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Container /= No_Node
                     and then Kind (Container) = K_Array_Dimensions);

      Node : constant Node_Id := New_Node (K_Array_Dimension_Size, Loc);

   begin
      Set_Parent (Node, Container);
      Set_Size (Node, Dimension_Size);

      return Node;

   end Add_New_Array_Dimension_Size;

   -----------------------------
   -- Add_New_Array_Selection --
   -----------------------------

   function Add_New_Array_Selection
     (Loc             : Location;
      Container       : Node_Id;
      Identifier      : Node_Id;
      Range_List      : List_Id)
     return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Unreferenced (Container);
      pragma Assert (Kind (Identifier) = K_Identifier);

      Node : constant Node_Id := New_Node (K_Array_Selection, Loc);
   begin
      Set_Identifier (Node, Identifier);
      Set_Range_Selections (Node, Range_List);

      return Node;
   end Add_New_Array_Selection;

   -----------------------------
   -- Add_New_Range_Selection --
   -----------------------------

   function Add_New_Range_Selection
     (Container   : Node_Id;
      Lower_Bound : Node_Id;
      Upper_Bound : Node_Id)
     return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Unreferenced (Container);
      pragma Assert (Present (Lower_Bound));

      Node : constant Node_Id := New_Node (K_Range_Selection,
                            Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Lower_Bound));
   begin

      Set_Lower_Bound (Node, Lower_Bound);
      Set_Upper_Bound (Node, Upper_Bound);

      return Node;

   end Add_New_Range_Selection;

end Ocarina.Builder.AADL.Components.Arrays;
