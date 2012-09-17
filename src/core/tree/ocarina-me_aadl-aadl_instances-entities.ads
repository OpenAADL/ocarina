------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.ME_AADL.AADL_INSTANCES.ENTITIES                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

with Types;

package Ocarina.ME_AADL.AADL_Instances.Entities is

   --
   --  This following section is relative to Entities
   --

   function Get_Name_Of_Entity
     (Entity : Types.Node_Id;
      Get_Display_Name : Boolean := True)
     return Types.Name_Id;

   function Get_Name_Of_Entity
     (Entity : Types.Node_Id;
      Get_Display_Name : Boolean := True)
     return String;

   function Get_Referenced_Entity
     (Entity_Ref : Types.Node_Id)
     return Types.Node_Id;
   --  Return the entity referenced by an entity reference, or No_Node
   --  if nothing is pointed

   procedure Add_Path_Element_To_Entity_Reference
     (Entity_Ref, Item : Types.Node_Id);
   --  Add Item to the end of the path that constitutes the reference
   --  to the entity.

   function Entity_Reference_Path_Has_Several_Elements
     (Entity_Ref : Types.Node_Id)
     return Boolean;
   --  return True if the path has more than one element.

   function Duplicate_Identifier
     (Identifier : Types.Node_Id)
     return Types.Node_Id;

   --
   --  This following section is relative to Entities Components
   --

   function Get_Category_Of_Component
     (Component : Types.Node_Id)
     return Component_Category;
   --  return the category of the component type, implementation or
   --  instance.

   --
   --  This following section is relative to Entities Components Connections
   --

   function Get_Category_Of_Connection
     (Connection : Types.Node_Id)
     return Connection_Type;

   --
   --  This following section is relative to Entities Components Subcomponents
   --

   function Get_Category_Of_Subcomponent
     (Subcomponent : Types.Node_Id)
     return Component_Category;
   --  Return the category of the subcomponent or subcomponent
   --  instance.

   --
   --  This following section is relative to Entities Messages
   --

   function Display_Node_Kind_Error
     (Node : Types.Node_Id)
     return Boolean;

   function DNKE (Node : Types.Node_Id) return Boolean
     renames Display_Node_Kind_Error;

end Ocarina.ME_AADL.AADL_Instances.Entities;
