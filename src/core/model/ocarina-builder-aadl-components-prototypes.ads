------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BUILDER.AADL.COMPONENTS.PROTOTYPES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2012 ESA & ISAE.        --
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

package Ocarina.Builder.AADL.Components.Prototypes is

   function Add_New_Prototype
     (Loc            : Location;
      Name           : Node_Id;
      Container      : Node_Id;
      Classifier_Ref : Node_Id;
      Category       : Ocarina.ME_AADL.Component_Category;
      Is_Refinement  : Boolean := False)
     return Node_Id;
   --  Create and add a new prototype into a component
   --  implementation or component type. Loc is the location of the
   --  prototype in the parsed text. Name references an identifier
   --  which contains the name of the prototype. Container references
   --  the component implementation or component type. Classifier_Ref
   --  references the unique_classifier_reference. Category is the
   --  type of the prototype. Is_Refinement indicates if it is a refinement
   --  or not. Return the Node_Id of the newly created prototype if
   --  everything went right, else No_Node.

   function Add_New_Prototype_Binding
     (Loc            : Location;
      Name           : Node_Id;
      Container      : Node_Id;
      Classifier_Ref : Node_Id;
      Category       : Ocarina.ME_AADL.Component_Category)
     return Node_Id;
   --  Create a new prototype binding. Loc is the location of the prototype
   --  binding in the parsed text. Name references an identifier which
   --  contains the name of the prototype binding. Container references
   --  the component which contain it. Classifier_Ref references the
   --  unique_classifier_reference. Category is the type of the prototype
   --  binding. Return the Node_Id of the newly created prototype binding if
   --  everything went right, else No_Node.

end Ocarina.Builder.AADL.Components.Prototypes;
