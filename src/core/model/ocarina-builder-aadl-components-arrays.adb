------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL.COMPONENTS.ARRAYS                   --
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

package body Ocarina.Builder.AADL.Components.Arrays is

   ----------------------------------
   -- Add_New_Array_Dimension_Size --
   ----------------------------------

   function Add_New_Array_Dimension_Size
     (Loc            : Location;
      Container      : Node_Id;
      Dimension_Size : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Container /= No_Node and then Kind (Container) = K_Array_Dimensions);

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
     (Loc        : Location;
      Container  : Node_Id;
      Identifier : Node_Id;
      Range_List : List_Id) return Node_Id
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
      Upper_Bound : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Unreferenced (Container);
      pragma Assert (Present (Lower_Bound));

      Node : constant Node_Id :=
        New_Node
          (K_Range_Selection,
           Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Lower_Bound));
   begin

      Set_Lower_Bound (Node, Lower_Bound);
      Set_Upper_Bound (Node, Upper_Bound);

      return Node;

   end Add_New_Range_Selection;

end Ocarina.Builder.AADL.Components.Arrays;
