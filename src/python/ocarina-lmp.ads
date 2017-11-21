------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                          O C A R I N A . L M P                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2015 ESA & ISAE.                       --
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

pragma Warnings (Off);

with Ocarina.Types;                      use Ocarina.Types;
with GNATCOLL.Scripts;                   use GNATCOLL.Scripts;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

package Ocarina.Lmp is

   function Get_Packages return Node_List;
   function Get_Import_Declarations return Node_List;
   function Get_Alias_Declarations return Node_List;
   function Get_Component_Types (kind : String) return Node_List;
   function Get_Component_Implementations (kind : String) return Node_List;
   function Get_Annexes return Node_List;
   function Get_Prototype return Node_List;
   function Get_Prototype_Binding return Node_List;
   function Get_Flow_Specs return Node_List;
   function Get_Flow_Implementations return Node_List;
   function Get_Modes return Node_List;
   function Get_Mode_Transitions return Node_List;
   function Get_In_Modes return Node_List;
   --  function Get_PropertyBinding return Node_List;
   function Get_Property_Sets return Node_List;
   function Get_Property_Types (PropertySet : Node_Id) return Node_List;
   function Get_Property_Definitions (PropertySet : Node_Id) return Node_List;
   function Get_Property_Constants (PropertySet : Node_Id) return Node_List;

   function Get_Instances (kind : String) return Node_List;
   function Filter_Component_By_Category (components : Node_List;
      category : Ocarina.ME_AADL.Component_Category) return Node_List;
   function Filter_Node_By_Kind (components : List_Id;
      category : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind) return Node_List;
   function Filter_Instance_By_Category (components : Node_List;
      category : Ocarina.ME_AADL.Component_Category) return Node_List;

   procedure Get_Component_Name (Data : in out Callback_Data'Class;
      N : Node_Id);
   procedure Get_Component_Fullname (Data : in out Callback_Data'Class;
      N : Node_Id);
   procedure Get_Instance_Name (Data : in out Callback_Data'Class;
      N : Node_Id);

   function Find_All_Component_Implementations
     (Root      : Node_Id;
      Namespace : Node_Id := No_Node) return Node_List;
   function Find_All_Component_Instances
     (Root      : Node_Id) return Node_List;

end Ocarina.Lmp;
