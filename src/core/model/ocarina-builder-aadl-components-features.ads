------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BUILDER.AADL.COMPONENTS.FEATURES                  --
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
