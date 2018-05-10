------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BUILDER.AADL.COMPONENTS.FLOWS                   --
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

with Ocarina.ME_AADL;

package Ocarina.Builder.AADL.Components.Flows is

   use Ocarina.ME_AADL;

   function Add_Property_Association
     (Flow                 : Node_Id;
      Property_Association : Node_Id) return Boolean;
   --  Add a property association to the flow declaration. Flow must
   --  reference a flow implementation or a flow
   --  specification. Property_Association references the property
   --  association. Return True if everything went right, else False.

   function Add_New_Flow_Spec
     (Loc           : Location;
      Name          : Node_Id;
      Comp_Type     : Node_Id;
      Category      : Flow_Category;
      Source_Flow   : Node_Id;
      Sink_Flow     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id;
   --  Create a new flow specification inside a component type

   function Add_New_Flow_Implementation
     (Loc           : Location;
      Container     : Node_Id;
      Name          : Node_Id;
      Category      : Flow_Category;
      In_Modes      : Node_Id;
      Is_Refinement : Boolean;
      Source_Flow   : Node_Id;
      Sink_Flow     : Node_Id) return Node_Id;
   --  Create a new flow implementation inside a component
   --  implementation

   function Add_New_End_To_End_Flow_Spec
     (Loc           : Location;
      Container     : Node_Id;
      Name          : Node_Id;
      In_Modes      : Node_Id;
      Is_Refinement : Boolean;
      Source_Flow   : Node_Id;
      Sink_Flow     : Node_Id) return Node_Id;
   --  Create a new end to end flow specification inside a component
   --  implementation

end Ocarina.Builder.AADL.Components.Flows;
