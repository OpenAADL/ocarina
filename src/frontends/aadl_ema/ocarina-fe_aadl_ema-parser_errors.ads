------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L _ E M A . P A R S E R _ E R R O R S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2015-2016 ESA & ISAE.                    --
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

with Ocarina.ME_AADL_EMA.EMA_Tokens;  use Ocarina.ME_AADL_EMA.EMA_Tokens;

package Ocarina.FE_AADL_EMA.Parser_Errors is

   --  Parsing Codes

   type Parsing_Code is
     (PC_None_Statement,
      PC_Items_List,

      PC_Conditional_Statement,
      PC_If_Cond_Struct,
      PC_If_Cond_Statement,
      PC_Elsif_Cond_Statement,
      PC_Else_Cond_Statement,
      PC_For_Cond_Struct,
      PC_While_Cond_Struct,
      PC_While_Cond_Statement,

      PC_EMA_Annex,
      PC_Error_Model_Library_Constructs,
      PC_Error_Type_Library,
      PC_Error_Type_Library_List,
      PC_Error_Behavior_State_Machine,
      PC_Error_Type_Mappings,
      PC_Error_Type_Transformations,
      PC_Error_Type_Library_Element,
      PC_Properties_Error_Type_Library,
      PC_Error_Type_Transformation_Set_Reference,
      PC_Error_Behavior_Event,
      PC_Error_Behavior_State,
      PC_Error_Behavior_Transition,
      PC_Properties_Error_Behavior_State_Machine,
      PC_Error_Type_Mapping,
      PC_Use_Error_Types,
      PC_Error_Type_Transformation,
      PC_Error_Type_Library_Element_Node,
      PC_Error_Type_Definition,
      PC_Error_Type_Alias,
      PC_Error_Type_Set_Definition,
      PC_Error_Type_Set,
      PC_Source_Error_Type_Set,
      PC_Error_Type_Set_Alias,
      PC_Error_Event,
      PC_Recover_Event,
      PC_Repair_Event,
      PC_Error_Type_Set_Or_No_Error,
      PC_Target_Error_Type_Instance,
      PC_Transition,
      PC_Branching_Transition,
      PC_Type_Set_Element,
      PC_Error_Type_Or_Set_Reference,
      PC_Error_Type_Reference,
      PC_Error_Type_Set_Reference,
      PC_Initiator_Reference,
      PC_Port_Reference,
      PC_Self_Event_Reference,
      PC_Mode_Transition_Reference,
      PC_Event_Initiation,
      PC_Error_Source_State,
      PC_Operator,
      PC_Error_Condition,
      PC_Error_Transition_Target,
      PC_Error_Transition_Branch,
      PC_Branch_Probability,
      PC_Fixed_Probability_Value,
      PC_Error_Condition_Trigger,
      PC_Error_Propagation_Point,
      PC_Feature_Reference,
      PC_Binding_Reference,
      PC_Outgoing_Error_Propagation_Point,
      PC_Fault_Source_Error_Type_Set,
      PC_Error_Model_Library_Reference,

      PC_Error_Model_Component_Constructs,
      PC_Emv2_Contained_Property_Association,
      PC_Emv2_Containment_Path,
      PC_Property_Identifier,
      PC_Property_Value,
      PC_Assignment,
      PC_Enumeration_Type,
      PC_Error_Type_Mappings_Reference,
      PC_Error_Behavior_State_Machine_Reference,
      PC_Error_Propagations,
      PC_Error_Propagations_Element,
      PC_Propagation,
      PC_Error_Propagation,
      PC_Error_Containment,
      PC_Error_Flow,
      PC_Fault_Source,
      PC_Error_Sink,
      PC_Error_Path,
      PC_Component_Error_Behavior,
      PC_Component_Error_Behavior_Node,
      PC_Outgoing_Propagation_Condition,
      PC_Propagation_Target,
      PC_Error_Detection,
      PC_Error_Detection_Effect,
      PC_Internal_Event_Reference,
      PC_Error_Code_Value,
      PC_Property_Constant_Term,
      PC_Error_State_To_Mode_Mapping,
      PC_Composite_Error_State,
      PC_Composite_Error_Behavior,
      PC_Composite_State_Expression,
      PC_Composite_State_Element,
      PC_Subcomponent_Error_State,
      PC_Connection_Error_Behavior,
      PC_Error_Source_Parent,
      PC_Error_Source,
      PC_Connection_Error_Source,
      PC_Propagation_Paths,
      PC_Propagation_Point,
      PC_Propagation_Path,
      PC_Qualified_Propagation_Point,
      PC_Error_Event_Condition,
      PC_Recover_Event_Initiators,
      PC_Incoming_Error_Propagation_Point,
      PC_Failure_Mode_Description,
      PC_Fault_Condition,
      PC_Error_Type_Product,
      PC_Outgoing_Port_Reference,
      PC_Subcomponent_State_Expression,
      PC_Target_Qualified_Propagation_Point,
      PC_Source_Qualified_Propagation_Point,
      PC_EMV2_Properties_Section,
      PC_Time_Range
      );

   --  Error Message Code

   type Error_Message_Code is
     (EMC_Error,
      EMC_Debug,
      EMC_Failed,
      EMC_Numeral_Failed,
      EMC_Operator_Unknown,
      EMC_Expected_Prefix,
      EMC_No_Defining_Identifier,
      EMC_Illegal_Syntax,
      EMC_Invalid_Range,
      EMC_List_Is_Empty,
      EMC_At_Least_Tow_Elements_Expected,
      EMC_At_Least_Three_Elements_Expected);

   procedure Display_Parsing_Error;
   procedure DPE
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code           : Parsing_Code;
      Code_Expected  : Parsing_Code;
      Expected_Token : Token_Type);
   procedure DPE (Code           : Parsing_Code;
                  Code_Expected  : Parsing_Code;
                  Expected_Token : Token_Type)
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code            : Parsing_Code;
      Code_Expected   : Parsing_Code;
      Expected_Tokens : Token_List_Type);
   procedure DPE (Code            : Parsing_Code;
                  Code_Expected   : Parsing_Code;
                  Expected_Tokens : Token_List_Type)
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code            : Parsing_Code;
      Code_Expected_1  : Parsing_Code;
      Code_Expected_2  : Parsing_Code;
      Expected_Tokens : Token_List_Type);
   procedure DPE (Code             : Parsing_Code;
                  Code_Expected_1  : Parsing_Code;
                  Code_Expected_2  : Parsing_Code;
                  Expected_Tokens  : Token_List_Type)
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code             : Parsing_Code;
      Code_Expected_1  : Parsing_Code;
      Code_Expected_2  : Parsing_Code);
   procedure DPE (Code             : Parsing_Code;
                  Code_Expected_1  : Parsing_Code;
                  Code_Expected_2  : Parsing_Code)
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code             : Parsing_Code;
      Code_Expected_1  : Parsing_Code;
      Code_Expected_2  : Parsing_Code;
      Expected_Token   : Token_Type);
   procedure DPE (Code             : Parsing_Code;
                  Code_Expected_1  : Parsing_Code;
                  Code_Expected_2  : Parsing_Code;
                  Expected_Token   : Token_Type)
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code             : Parsing_Code;
      Code_Expected_1  : Parsing_Code;
      Code_Expected_2  : Parsing_Code;
      Code_Expected_3  : Parsing_Code);
   procedure DPE (Code             : Parsing_Code;
                  Code_Expected_1  : Parsing_Code;
                  Code_Expected_2  : Parsing_Code;
                  Code_Expected_3  : Parsing_Code)
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code           : Parsing_Code;
      Code_Expected  : Parsing_Code);
   procedure DPE (Code          : Parsing_Code;
                  Code_Expected : Parsing_Code)
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error_Unexpected
     (Code : Parsing_Code);
   procedure DPE (Code      : Parsing_Code)
     renames Display_Parsing_Error_Unexpected;

   procedure Display_Parsing_Error
     (Code      : Parsing_Code;
      Error_Msg : Error_Message_Code;
      Msg_Add   : String;
      Fatal     : Boolean            := False;
      Warning   : Boolean            := False);
   procedure DPE (Code      : Parsing_Code;
                  Error_Msg : Error_Message_Code;
                  Msg_Add   : String;
                  Fatal     : Boolean            := False;
                  Warning   : Boolean            := False)
     renames Display_Parsing_Error;
   --  Display an output message:
   --     Location: parsing ..., <Msg>

   procedure Display_Parsing_Error
     (Code           : Parsing_Code;
      Expected_Token : Token_Type;
      Fatal          : Boolean       := False;
      Warning        : Boolean       := False);
   procedure DPE (Code           : Parsing_Code;
                  Expected_Token : Token_Type;
                  Fatal          : Boolean       := False;
                  Warning        : Boolean       := False)
     renames Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code            : Parsing_Code;
      Expected_Tokens : Token_List_Type;
      Fatal           : Boolean             := False;
      Warning         : Boolean             := False);
   procedure DPE (Code            : Parsing_Code;
                  Expected_Tokens : Token_List_Type;
                  Fatal           : Boolean            := False;
                  Warning         : Boolean            := False)
     renames Display_Parsing_Error;

   function Image (Code : Parsing_Code) return String;
   --  Return corresponding string of given parsing code

   function Image (Code : Error_Message_Code) return String;
   --  Return corresponding string of given error message code

end Ocarina.FE_AADL_EMA.Parser_Errors;
