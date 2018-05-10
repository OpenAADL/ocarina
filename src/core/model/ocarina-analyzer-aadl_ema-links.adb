------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . A N A L Y Z E R . A A D L _ E M A . L I N K S       --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;

package body Ocarina.Analyzer.AADL_EMA.Links is

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package EMATN renames Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;

   ----------------------------------
   -- Link_Properties_Of_Component --
   ----------------------------------

   function Link_Properties_Of_Component
   (Root : Node_Id; Node : Node_Id) return Boolean
   is
   begin
      return Root = Node;
   end Link_Properties_Of_Component;

   ----------------------------------
   -- Link_Error_Type_Library_List --
   ----------------------------------

   procedure Link_Error_Type_Library_List
     (Root    : Node_Id;
      Node    : Node_Id)
   is
      use ATN;
      use EMATN;

      pragma Assert (ATN.Kind (Root)   = ATN.K_PACKAGE_SPECIFICATION);
      pragma Assert (EMATN.Kind (Node) =
                     EMATN.K_Error_Model_Library_Reference);
   begin
      Set_AADL_Package_Reference (Node, Root);
   end Link_Error_Type_Library_List;

   ----------------------------------
   -- Link_Error_Type_Library_List --
   ----------------------------------

   procedure Link_Error_Type_Reference
     (Error_Model_Library_Ref : Node_Id;
      Node_Referenced         : Node_Id)
   is
      use EMATN;

      pragma Assert (EMATN.Kind (Error_Model_Library_Ref)
                     = K_Error_Model_Library_Reference);
      pragma Assert (EMATN.Kind (Node_Referenced)
                     = K_Error_Model_Library_Reference);

      AADL_Package_Ref : Node_Id;
   begin
      AADL_Package_Ref := AADL_Package_Reference (Node_Referenced);
      Set_AADL_Package_Reference (Error_Model_Library_Ref, AADL_Package_Ref);
   end Link_Error_Type_Reference;

end Ocarina.Analyzer.AADL_EMA.Links;
