------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BUILDER.AADL.COMPONENTS.FLOWS                   --
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

package Ocarina.Builder.AADL.Components.Flows is

   use Ocarina.ME_AADL;

   function Add_Property_Association
     (Flow                 : Node_Id;
      Property_Association : Node_Id)
     return Boolean;
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
      Is_Refinement : Boolean := False)
     return Node_Id;
   --  Create a new flow specification inside a component type

   function Add_New_Flow_Implementation
     (Loc           : Location;
      Container     : Node_Id;
      Name          : Node_Id;
      Category      : Flow_Category;
      In_Modes      : Node_Id;
      Is_Refinement : Boolean;
      Source_Flow   : Node_Id;
      Sink_Flow     : Node_id)
     return Node_Id;
   --  Create a new flow implementation inside a component
   --  implementation

   function Add_New_End_To_End_Flow_Spec
     (Loc           : Location;
      Container     : Node_Id;
      Name          : Node_Id;
      In_Modes      : Node_Id;
      Is_Refinement : Boolean;
      Source_Flow   : Node_Id;
      Sink_Flow     : Node_id)
     return Node_Id;
   --  Create a new end to end flow specification inside a component
   --  implementation

end Ocarina.Builder.AADL.Components.Flows;
