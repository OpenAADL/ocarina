------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . M E _ A A D L _ B A . B A _ T R E E . N U T I L S     --
--                                                                          --
--                                 S p e c                                  --
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

with Locations;                        use Locations;
with Ocarina.Types;                    use Ocarina.Types;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes; use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

package Ocarina.ME_AADL_BA.BA_Tree.Nutils is

   function First_Homonym (N : Node_Id) return Node_Id;
   procedure Set_First_Homonym (N : Node_Id; V : Node_Id);

   procedure Append_List_To_List (S : List_Id; D : in out List_Id);
   pragma Inline (Append_List_To_List);
   --  Append list S to list D, if D does not exist, then D := S

   procedure Push_Node_To_List (E : Node_Id; L : List_Id);
   --  Add node N to the front of list L

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   --  Append node N to list L (to the end of list L)

   procedure Append_Node_To_Node_List
     (Node :        Node_Id;
      List : in out Node_List;
      Once :        Boolean := True);
   --  Append Node to List. If Once true, do not append when Node
   --  already there.

   procedure Replace_Node_To_List
     (List     : List_Id;
      Old_Node : Node_Id;
      New_Node : Node_Id);
   --  Replace Old_Node by New_Node in List.

   procedure Remove_Nodes_From_List (List : in out Node_List);
   --  Clear all links between the nodes of List.

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Remove_Last_Node_From_List (L : List_Id) return Node_Id;
   --  Remove the last node from list L, return the removed node
   --  Return No_Node if L does not exists or is empty

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   function Length (L : List_Id) return Natural;

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   --  Create a new node

   function New_List (Kind : Node_Kind; Loc : Location) return List_Id;
   --  Create a new list

   function Make_Identifier
     (Loc          : Location;
      Name         : Name_Id;
      Display_Name : Name_Id;
      Entity       : Node_Id)
     return Node_Id;
   --  Make an identifier

   function Make_Node_Container
     (Item       : Node_Id;
      Extra_Item : Node_Id := No_Node)
     return Node_Id;
   --  Creates a container for the nodes Item and Extra_Item to be able
   --  to put one node in several lists

   procedure Reset_Nodes;
   --  Reset the table that contains the nodes. Beware that everything
   --  will be lost.

end Ocarina.ME_AADL_BA.BA_Tree.Nutils;
