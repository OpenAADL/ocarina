------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B U I L D E R . A A D L _ B A . A C T I O N S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2010-2014 ESA & ISAE.                    --
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
with Locations;

with Ocarina.ME_AADL_BA;

package Ocarina.Builder.Aadl_Ba.Actions is

   use Types;
   use Locations;
   use Ocarina.ME_AADL_BA;

   function Add_New_Behavior_Action
     (Loc         : Location;
      Container   : Node_Id;
      Action_Node : Node_Id) return Node_Id;

   function Add_New_If_Cond_Struct (Loc : Location) return Node_Id;

   procedure Add_New_If_Cond_Struct
     (If_Cond_Struct : Node_Id;
      Container      : Node_Id := No_Node;
      If_Stat        : Node_Id := No_Node;
      Elsif_Stat     : Node_Id := No_Node;
      Else_Stat      : Node_Id := No_Node);

   function Add_New_Conditional_Statement
     (Loc        : Location;
      Container  : Node_Id;
      Expression : Node_Id;
      Actions    : List_Id) return Node_Id;

   function Add_New_For_Cond_Struct (Loc : Location) return Node_Id;

   procedure Add_New_For_Cond_Struct
     (For_Cond_Struct : Node_Id;
      Container       : Node_Id;
      Variable_Id     : Node_Id;
      Range_Node      : Node_Id;
      Actions         : List_Id);

   function Add_New_Assignment_Action
     (Loc         : Location;
      Container   : Node_Id;
      Ident       : Node_Id;
      Value_Expr  : Node_Id;
      Is_Any_Bool : Boolean) return Node_Id;

   function Add_New_Communication_Action
     (Loc            : Location;
      Container      : Node_Id;
      Ident          : Node_Id;
      Target_Node    : Node_Id;
      Sub_Parameters : List_Id;
      Com_Kind       : Communication_Kind) return Node_Id;

   function Add_New_Timed_Action
     (Loc            : Location;
      Container      : Node_Id;
      Fst_Behav_Time : Node_Id;
      Scd_Behav_Time : Node_Id           := No_Node;
      Distribution   : Distribution_Kind := DK_No_Kind;
      Is_Comput      : Boolean           := False) return Node_Id;

   function Add_New_Data_Component_Reference
     (Loc       : Location;
      Container : Node_Id;
      Idents    : List_Id) return Node_Id;

   function Add_New_Parameter_Label
     (Loc       : Location;
      Container : Node_Id;
      Param     : Node_Id) return Node_Id;

   function Add_New_Id
     (Loc          : Location;
      Container    : Node_Id;
      Ident        : Node_Id;
      Value_Holder : Node_Id) return Node_Id;

end Ocarina.Builder.Aadl_Ba.Actions;
