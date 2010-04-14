------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.BUILDER.AADL.COMPONENTS.SUBCOMPONENTS               --
--                                                                          --
--                                 S p e c                                  --
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

with Ocarina.ME_AADL;

package Ocarina.Builder.AADL.Components.Subcomponents is

   function Add_Property_Association
     (Subcomponent         : Node_Id;
      Property_Association : Node_Id)
     return Boolean;
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
      Prototypes_Bindings : List_Id := No_List)
     return Node_Id;
   --  Create and add a new subcomponent into a component
   --  implementation. Loc is the location of the subcomponent in the
   --  parsed text. Name references an identifier which contains the
   --  name of the subcomponent. Comp_Impl references the component
   --  implementation. Category is the type of the
   --  subcomponent. Is_Refinement indicates wether the connection is
   --  a refinement or not. In_Modes contains the list of the modes
   --  associated to the connection. Return the Node_Id of the newly
   --  created subcomponent if everything went right, else No_Node.

end Ocarina.Builder.AADL.Components.Subcomponents;
