------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B U I L D E R . A A D L _ B A . E X P R E S S I O N S   --
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

with Ocarina.Types;
with Locations;

with Ocarina.ME_AADL_BA;

package Ocarina.Builder.Aadl_Ba.Expressions is

   use Ocarina.Types;
   use Locations;
   use Ocarina.ME_AADL_BA;

   function Add_New_Value_Holder
     (Loc          : Location;
      Container    : Node_Id;
      Ident        : Node_Id;
      Target_Node  : Node_Id := No_Node;
      Is_A_Count   : Boolean := False;
      Is_A_Fresh   : Boolean := False;
      Is_A_Interro : Boolean := False) return Node_Id;

   function Add_New_Value_Expression
     (Loc           : Location;
      Container     : Node_Id;
      Relation_List : List_Id) return Node_Id;

   function Add_New_Relation
     (Loc           : Location;
      Container     : Node_Id;
      Bool_Value    : Boolean;
      Spl_Expr_List : List_Id := No_List) return Node_Id;

   function Add_New_Simple_Expression
     (Loc              : Location;
      Container        : Node_Id;
      Simple_Expr_List : List_Id) return Node_Id;

   function Add_New_Term
     (Loc         : Location;
      Container   : Node_Id;
      Factor_List : List_Id) return Node_Id;

   function Add_New_Factor
     (Loc         : Location;
      Container   : Node_Id;
      Is_Abs_Bool : Boolean;
      Is_Not_Bool : Boolean;
      Low_Primary : Node_Id;
      Upp_Primary : Node_Id) return Node_Id;

   function Add_New_Property_Constant
     (Loc             : Location;
      Container       : Node_Id;
      Property_Set_Id : Node_Id;
      Property_Cst_Id : Node_Id) return Node_Id;

   function Add_New_Integer_Range
     (Loc         : Location;
      Container   : Node_Id;
      Lower_Bound : Node_Id;
      Upper_Bound : Node_Id) return Node_Id;

   function Add_New_Integer_Value
     (Loc         : Location;
      Container   : Node_Id;
      Entity_Node : Node_Id) return Node_Id;

   function Add_New_Behavior_Time
     (Loc         : Location;
      Container   : Node_Id;
      Integer_Val : Node_Id;
      Unit_Ident  : Node_Id) return Node_Id;

   function Add_New_Operator
     (Loc         : Location;
      Container   : Node_Id;
      Operat_Kind : Operator_Kind) return Node_Id;

end Ocarina.Builder.Aadl_Ba.Expressions;
