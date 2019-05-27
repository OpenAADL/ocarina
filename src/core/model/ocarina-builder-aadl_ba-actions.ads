------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B U I L D E R . A A D L _ B A . A C T I O N S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2010-2016 ESA & ISAE.                    --
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

with Ocarina.ME_AADL_BA;

package Ocarina.Builder.AADL_BA.Actions is

   use Ocarina.Types;
   use Locations;
   use Ocarina.ME_AADL_BA;

   function Add_New_Behavior_Action_Block
     (Loc              : Location;
      Container        : Node_Id;
      Behavior_Actions : Node_Id;
      Behavior_Time    : Node_Id)
     return Node_Id;

   function Add_New_Behavior_Actions
     (Loc                      : Location;
      Container                : Node_Id;
      Behavior_Action          : Node_Id;
      Behavior_Action_Sequence : List_Id;
      Behavior_Action_Set      : List_Id)
     return Node_Id;

   function Add_New_Behavior_Action
     (Loc              : Location;
      Container        : Node_Id;
      Action_Node      : Node_Id)
     return Node_Id;

   function Add_New_If_Cond_Struct (Loc : Location) return Node_Id;

   procedure Add_New_If_Cond_Struct
     (If_Cond_Struct  : Node_Id;
      Container       : Node_Id  := No_Node;
      If_Stat         : Node_Id  := No_Node;
      Elsif_Stat      : List_Id  := No_List;
      Else_Stat       : Node_Id  := No_Node);

   function Add_New_Conditional_Statement
     (Loc        : Location;
      Container  : Node_Id;
      Expression : Node_Id;
      Actions    : Node_Id)
     return Node_Id;

   function Add_New_For_Cond_Struct (Loc : Location) return Node_Id;

   procedure Add_New_For_Cond_Struct
     (For_Cond_Struct          : Node_Id;
      Container                : Node_Id;
      Element_Idt              : Node_Id;
      Classifier_Ref           : Node_Id;
      Element_Values_Node      : Node_Id;
      Actions                  : Node_Id);

   function Add_New_While_Cond_Struct (Loc : Location) return Node_Id;

   procedure Add_New_While_Cond_Struct
     (While_Cond_Struct : Node_Id;
      Container       : Node_Id;
      Expression      : Node_Id;
      Actions         : Node_Id);

   function Add_New_Forall_Cond_Struct (Loc : Location) return Node_Id;

   procedure Add_New_Forall_Cond_Struct
     (ForAll_Cond_Struct : Node_Id;
      Container                : Node_Id;
      Element_Idt              : Node_Id;
      Classifier_Ref           : Node_Id;
      Element_Values_Node      : Node_Id;
      Actions                  : Node_Id);

   function Add_New_DoUntil_Cond_Struct (Loc : Location) return Node_Id;

   procedure Add_New_DoUntil_Cond_Struct
     (DoUntil_Cond_Struct : Node_Id;
      Container       : Node_Id;
      Expression      : Node_Id;
      Actions         : Node_Id);

   function Add_New_Assignment_Action
     (Loc              : Location;
      Container        : Node_Id;
      Ident            : Node_Id;
      Value_Expr       : Node_Id;
      Is_Any_Bool      : Boolean)
     return Node_Id;

   function Add_New_Communication_Action
     (Loc            : Location;
      Container      : Node_Id;
      Ident          : Node_Id;
      Target_Node    : Node_Id;
      Sub_Parameters : List_Id;
      Com_Kind       : Communication_Kind)
     return Node_Id;

   function Add_New_Timed_Action
     (Loc            : Location;
      Container      : Node_Id;
      Fst_Behav_Time : Node_Id;
      Scd_Behav_Time : Node_Id           := No_Node;
      Processor_Idt  : List_Id           := No_List;
      Is_InBinding   : Boolean           := False)
     return Node_Id;

   function Add_New_Data_Component_Reference
     (Loc            : Location;
      Container      : Node_Id;
      Idents         : List_Id)
     return Node_Id;

   function Add_New_Parameter_Label
     (Loc       : Location;
      Container : Node_Id;
      Param     : Node_Id)
     return Node_Id;

   function Add_New_Name
     (Loc              : Location;
      Container        : Node_Id;
      Idents           : List_Id;
      Array_Index_List : List_Id := No_List)
     return Node_Id;

end Ocarina.Builder.AADL_BA.Actions;
