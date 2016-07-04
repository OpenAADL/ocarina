------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.ME_AO4AADL.AO4AADL_TREE.NUTILS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

--  Offers list manipulation functions

with Ocarina.Types;
with Locations;
with Ocarina.ME_AO4AADL.AO4AADL_Tree.Nodes;

package Ocarina.ME_AO4AADL.AO4AADL_Tree.Nutils is
   use Ocarina.Types;
   use Locations;
   use Ocarina.ME_AO4AADL.AO4AADL_Tree.Nodes;

   AO4AADL_Root   : Node_Id;
   --  Current AO4AADL annex root node

   AADL_Model_Root : Node_Id;
   --  Current AADL model root node

   Owner_Node  : Node_Id;
   --  Node that is owner of the current AO4AADL annex

   procedure Init;
   --  Initialize AO4AADL internal routines

   procedure Reset;
   --  Reset AO4AADL internal routines

   function New_List (Kind : Node_Kind; Loc : Location) return List_Id;
   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   procedure Append_Node_To_List (E : Node_Id; L : List_Id);

   function Is_Empty (L : List_Id) return Boolean;
   --  List manipulation

end Ocarina.ME_AO4AADL.AO4AADL_Tree.Nutils;
