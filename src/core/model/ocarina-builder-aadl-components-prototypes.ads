------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BUILDER.AADL.COMPONENTS.PROTOTYPES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

package Ocarina.Builder.AADL.Components.Prototypes is

   function Add_New_Prototype
     (Loc            : Location;
      Name           : Node_Id;
      Container      : Node_Id;
      Classifier_Ref : Node_Id;
      Category       : Ocarina.ME_AADL.Component_Category;
      Is_Refinement  : Boolean := False) return Node_Id;
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
      Category       : Ocarina.ME_AADL.Component_Category) return Node_Id;
   --  Create a new prototype binding. Loc is the location of the prototype
   --  binding in the parsed text. Name references an identifier which
   --  contains the name of the prototype binding. Container references
   --  the component which contain it. Classifier_Ref references the
   --  unique_classifier_reference. Category is the type of the prototype
   --  binding. Return the Node_Id of the newly created prototype binding if
   --  everything went right, else No_Node.

end Ocarina.Builder.AADL.Components.Prototypes;
