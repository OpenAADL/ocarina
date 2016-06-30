------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.BUILDER.AADL.COMPONENTS.SUBCOMPONENTS               --
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

with Ocarina.ME_AADL;

package Ocarina.Builder.AADL.Components.Subcomponents is

   function Add_Property_Association
     (Subcomponent         : Node_Id;
      Property_Association : Node_Id) return Boolean;
   --  Add a property association to the subcomponent
   --  declaration. Subcomponent must reference a Subcomponent
   --  declaration. Property_Association references the property
   --  association. Return True if everything went right, else False.

   function Add_New_Subcomponent
     (Loc                 : Location;
      Name                : Node_Id;
      Comp_Impl           : Node_Id;
      Category            : Ocarina.ME_AADL.Component_Category;
      Is_Refinement       : Boolean := False;
      In_Modes            : Node_Id := No_Node;
      Prototypes_Bindings : List_Id := No_List;
      Entity_Ref          : Node_Id := No_Node) return Node_Id;
   --  Create and add a new subcomponent into a component
   --  implementation. Loc is the location of the subcomponent in the
   --  parsed text. Name references an identifier which contains the
   --  name of the subcomponent. Comp_Impl references the component
   --  implementation. Category is the type of the
   --  subcomponent. Is_Refinement indicates wether the connection is
   --  a refinement or not. In_Modes contains the list of the modes
   --  associated to the connection. Return the Node_Id of the newly
   --  created subcomponent if everything went right, else No_Node.
   --  Entity_Ref indicates the entity referenced by the created
   --  subcomponent.

end Ocarina.Builder.AADL.Components.Subcomponents;
