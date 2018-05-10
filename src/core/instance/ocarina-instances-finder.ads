------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . I N S T A N C E S . F I N D E R              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

package Ocarina.Instances.Finder is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   type Node_Kind_Array is
     array
       (Positive range <>) of Ocarina.ME_AADL.AADL_Instances.Nodes.Node_Kind;

   function Find_Instance
     (Instance_Root      : Node_Id;
      Reference_Instance : Node_Id;
      Path               : List_Id) return Node_Id;
   --  Return the instance pointed by Path, from Reference_Instance

   function Find_Instance_In_Instance
     (Instance_Root      : Node_Id;
      Reference_Instance : Node_Id;
      Path               : List_Id) return Node_Id;
   --  Return the instance pointed by Path, from Reference_Instance

   function Find_Local_Instance
     (Reference_Instance  : Node_Id;
      Instance_Identifier : Node_Id) return Node_Id;
   --  Return the instance having the same name as
   --  Instance_Identifier, or No_Node if no instance of that name can
   --  be found. Reference_Instance is (1) a component instance in
   --  which case the search is performed inside it or (2) a
   --  connection instance in which case the search is performed in
   --  its parent component.

   procedure Find_All_Instances
     (Instance_Root :        Node_Id;
      Kinds         :        Node_Kind_Array;
      First_Node    : in out Node_Id;
      Last_Node     : in out Node_Id);
   --  Search recursively in the instance hierarchy for instances entity
   --  within a provided set of kinds

   function Find_All_Component_Instances
     (Root      : Node_Id) return Node_List;

   function Filter_Instance_By_Category
     (Components : Node_List;
      Category : Component_Category)
     return Node_Array;
   --  Filter a Node_List made of component instances and by a given
   --  category.

   procedure Find_All_Flows
     (Instance_Root :        Node_Id;
      First_Node    : in out Node_Id;
      Last_Node     : in out Node_Id);
   --  Search recursively in the instance hierarchy for instances entity
   --  of end to end flows

end Ocarina.Instances.Finder;
