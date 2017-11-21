------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B U I L D E R . A A D L . P R O P E R T I E S       --
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

with Ocarina.Types;
with Locations;

package Ocarina.Builder.AADL.Properties is

   use Ocarina.Types;
   use Locations;

   function Add_New_Property_Set
     (Loc       : Location;
      Name      : Node_Id;
      Namespace : Node_Id) return Node_Id;

   function Add_New_Property_Constant_Declaration
     (Loc             : Location;
      Name            : Node_Id;
      Property_Set    : Node_Id;
      Constant_Type   : Node_Id;
      Unit_Identifier : Node_Id;
      Single_Value    : Node_Id;
      Multiple_Values : List_Id;
      Multiplicity    : Int) return Node_Id;
   --  Either Single_Value /= No_Node and Mulitple_Values = No_Node,
   --  then we have a single valued constant; or Single_Value =
   --  No_Node, then we have a muli valued constant

   function Add_New_Property_Type_Declaration
     (Loc             : Location;
      Name            : Node_Id;
      Property_Set    : Node_Id;
      Type_Designator : Node_Id) return Node_Id;

   function Add_New_Property_Definition_Declaration
     (Loc                     : Location;
      Name                    : Node_Id;
      Property_Set            : Node_Id;
      Is_Inherit              : Boolean;
      Is_Access               : Boolean;
      Single_Default_Value    : Node_Id;
      Multiple_Default_Value  : List_Id;
      Property_Name_Type      : Node_Id;
      Property_Type_Is_A_List : Boolean;
      Applies_To_All          : Boolean;
      Applies_To              : List_Id) return Node_Id;
   --  Either Applies_To_All is set to True and Applies_To is empty,
   --  or Applies_To_All is False and Applies_To is not empty

   function Add_New_Property_Association
     (Loc                 : Location;
      Name                : Node_Id;
      Property_Name       : Node_Id;
      Container           : Node_Id;
      In_Binding          : Node_Id;
      In_Modes            : Node_Id;
      Property_Value      : Node_Id;
      Is_Constant         : Boolean;
      Is_Access           : Boolean;
      Is_Additive         : Boolean;
      Applies_To          : List_Id;
      Check_For_Conflicts : Boolean := False;
      Override            : Boolean := False) return Node_Id;
   --  If Check_For_Conflicts is set to True, then the function checks
   --  whether there is a property association of that name
   --  already. If override is set to True and there is a conflict,
   --  then it is overridden by the new association. Else the new
   --  association is ignored. If Check_For_Conflicts is set to False,
   --  then the value of Override is ignored.

   function Add_New_Contained_Element_Path
     (Loc             : Location;
      Container       : Node_Id;
      Applies_To_Elts : List_Id;
      Annex_Path      : Node_Id) return Node_Id;

end Ocarina.Builder.AADL.Properties;
