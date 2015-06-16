------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.ME_AADL.AADL_INSTANCES.ENTITIES                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Types;

package Ocarina.ME_AADL.AADL_Instances.Entities is

   --
   --  This following section is relative to Entities
   --

   function Get_Name_Of_Entity
     (Entity           : Ocarina.Types.Node_Id;
      Get_Display_Name : Boolean := True) return Ocarina.Types.Name_Id;

   function Get_Name_Of_Entity
     (Entity           : Ocarina.Types.Node_Id;
      Get_Display_Name : Boolean := True) return String;

   function Get_Referenced_Entity
     (Entity_Ref : Ocarina.Types.Node_Id) return Ocarina.Types.Node_Id;
   --  Return the entity referenced by an entity reference, or No_Node
   --  if nothing is pointed

   procedure Add_Path_Element_To_Entity_Reference
     (Entity_Ref, Item : Ocarina.Types.Node_Id);
   --  Add Item to the end of the path that constitutes the reference
   --  to the entity.

   function Entity_Reference_Path_Has_Several_Elements
     (Entity_Ref : Ocarina.Types.Node_Id) return Boolean;
   --  return True if the path has more than one element.

   function Duplicate_Identifier
     (Identifier : Ocarina.Types.Node_Id) return Ocarina.Types.Node_Id;

   --
   --  This following section is relative to Entities Components
   --

   function Get_Category_Of_Component
     (Component : Ocarina.Types.Node_Id) return Component_Category;
   --  return the category of the component type, implementation or
   --  instance.

   --
   --  This following section is relative to Entities Components Connections
   --

   function Get_Category_Of_Connection
     (Connection : Ocarina.Types.Node_Id) return Connection_Type;

   --
   --  This following section is relative to Entities Components Subcomponents
   --

   function Get_Category_Of_Subcomponent
     (Subcomponent : Ocarina.Types.Node_Id) return Component_Category;
   --  Return the category of the subcomponent or subcomponent
   --  instance.

   --
   --  This following section is relative to Entities Messages
   --

   function Display_Node_Kind_Error (Node : Ocarina.Types.Node_Id)
                                    return Boolean;

   function DNKE
     (Node : Ocarina.Types.Node_Id) return Boolean renames
     Display_Node_Kind_Error;

end Ocarina.ME_AADL.AADL_Instances.Entities;
