------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BUILDER.AADL.COMPONENTS.FEATURES                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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

--  The core API for the feature subclause of the component types and
--  the port group types.

package Ocarina.Builder.AADL.Components.Features is

   function Add_Property_Association
     (Feature              : Node_Id;
      Property_Association : Node_Id) return Boolean;

   function Add_New_Port_Spec
     (Loc               : Location;
      Name              : Node_Id;
      Container         : Node_Id;
      Is_In             : Boolean;
      Is_Out            : Boolean;
      Is_Data           : Boolean;
      Is_Event          : Boolean;
      Is_Feature        : Boolean;
      Is_Refinement     : Boolean := False;
      Associated_Entity : Node_Id := No_Node) return Node_Id;

   function Add_New_Port_Group_Spec
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id;

   function Add_New_Feature_Group_Spec
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id;

   function Add_New_Server_Subprogram
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id;

   function Add_New_Data_Subprogram_Spec
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id;

   function Add_New_Subcomponent_Access
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean := False;
      Category      : Ocarina.ME_AADL.Component_Category;
      Is_Provided   : Boolean) return Node_Id;

   function Add_New_Parameter
     (Loc           : Location;
      Name          : Node_Id;
      Container     : Node_Id;
      Is_In         : Boolean := True;
      Is_Out        : Boolean := True;
      Is_Refinement : Boolean := False) return Node_Id;

end Ocarina.Builder.AADL.Components.Features;
