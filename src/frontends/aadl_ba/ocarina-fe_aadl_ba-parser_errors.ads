------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . F E _ A A D L _ B A . P A R S E R _ E R R O R S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

with Ocarina.ME_AADL_BA.Tokens; use Ocarina.ME_AADL_BA.Tokens;

package Ocarina.FE_AADL_BA.Parser_Errors is

   --  Parsing Codes

   type Parsing_Code is
     (PC_None_Statement,
      PC_Items_List,

      PC_Behavior_Annex,
      PC_Behavior_Declaration,
      PC_Behavior_Specification,
      PC_Behavior_Variable,
      PC_Behavior_State,
      PC_Behavior_Transition,
      PC_Behavior_Condition,
      PC_Execute_Condition,
      PC_Mode_Condition,
      PC_Trigger_Logical_Expression,
      PC_Event_Trigger,
      PC_Port_Component_Reference,

      PC_Dispatch_Condition,
      PC_Dispatch_Trigger,
      PC_Dispatch_Trigger_Event,
      PC_Dispatch_Trigger_Conjunction,
      PC_Dispatch_Trigger_Condition,
      PC_Dispatch_Conjunction,
      PC_Execute_Behavior_Transition,

      PC_Behavior_Action_Block,
      PC_Behavior_Action,
      PC_Behavior_Actions,
      PC_Conditional_Statement,
      PC_If_Cond_Struct,
      PC_If_Cond_Statement,
      PC_Elsif_Cond_Statement,
      PC_Else_Cond_Statement,
      PC_For_Cond_Struct,
      PC_Forall_Cond_Struct,
      PC_While_Cond_Struct,
      PC_While_Cond_Statement,
      PC_DoUntil_Cond_Struct,
      PC_Basic_Action,
      PC_Timed_Action,
      PC_Assignment_Or_Communication_Action,
      PC_Range,
      PC_Element_Values,
      PC_Parameter_Label,
      PC_Variable_Label,
      PC_Array_Index,

      PC_Data_Component_Reference,
      PC_Unique_Component_Classifier_Ref,
      PC_Component_Element_Ref,

      PC_Value_Variable,
      PC_Value_Expression,
      PC_Relation,
      PC_Simple_Expressions,
      PC_Simple_Expression,
      PC_Term,
      PC_Factor,
      PC_Value,
      PC_Operator,
      PC_Property_Constant,
      PC_Property_Reference,
      PC_Property_Name,
      PC_Property_Field,
      PC_Integer_Range,
      PC_Integer_Value,
      PC_Behavior_Time,

      PC_Id,
      PC_Name,
      PC_Identifier_With_Value,

      PC_Defining_Identifier,
      PC_Defining_Name);

   --  Error Message Code

   type Error_Message_Code is
     (EMC_Error,
      EMC_Debug,
      EMC_Annex_Empty,
      EMC_Execute_Or_Mode_Transition,
      EMC_Invalid_Range,
      EMC_List_Is_Empty,
      EMC_Dispatch_Condition,
      EMC_Failed,
      EMC_Numeral_Failed,
      EMC_Unique_Classifier_Ref,
      EMC_Persistent_State_Container,
      EMC_Trigger_Conjunction_Failed,
      EMC_Operator_Unknown,
      EMC_No_Defining_Identifier,
      EMC_Illegal_Syntax,
      EMC_Missing_Unit);

   procedure Display_Parsing_Error
     (Code      : Parsing_Code;
      Error_Msg : Error_Message_Code;
      Fatal     : Boolean            := False;
      Warning   : Boolean            := False);
   procedure DPE (Code      : Parsing_Code;
                  Error_Msg : Error_Message_Code;
                  Fatal     : Boolean            := False;
                  Warning   : Boolean            := False)
     renames Display_Parsing_Error;
   --  Display an output message:
   --     Location: parsing ..., <Msg>

   procedure Display_Parsing_Error
     (Code           : Parsing_Code;
      Expected_Token : BA_Token_Type;
      Fatal          : Boolean       := False;
      Warning        : Boolean       := False);
   procedure DPE (Code           : Parsing_Code;
                  Expected_Token : BA_Token_Type;
                  Fatal          : Boolean       := False;
                  Warning        : Boolean       := False)
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code            : Parsing_Code;
      Expected_Tokens : BA_Token_List_Type;
      Fatal           : Boolean             := False;
      Warning         : Boolean             := False);
   procedure DPE (Code            : Parsing_Code;
                  Expected_Tokens : BA_Token_List_Type;
                  Fatal           : Boolean            := False;
                  Warning         : Boolean            := False)
     renames Display_Parsing_Error;

   function Image (Code : Parsing_Code) return String;
   --  Return corresponding string of given parsing code

   function Image (Code : Error_Message_Code) return String;
   --  Return corresponding string of given error message code

end Ocarina.FE_AADL_BA.Parser_Errors;
