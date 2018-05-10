------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.ME_AADL.AADL_TREE.ENTITIES.PROPERTIES               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

--  This package provides functions to create or read property names,
--  types, constants and associations.

with Ocarina.AADL_Values;             use Ocarina.AADL_Values;
with Ocarina.ME_AADL.AADL_Tree.Nodes; use Ocarina.ME_AADL.AADL_Tree.Nodes;

package Ocarina.ME_AADL.AADL_Tree.Entities.Properties is
   use Ocarina.Types;

   type Property_Type is
     (PT_Boolean,
      PT_Boolean_Expression,
      PT_Integer,
      PT_Unsigned_Integer,
      PT_Float,
      PT_Unsigned_Float,
      PT_String,
      PT_Enumeration,
      PT_Reference,
      PT_Classifier,
      PT_Range,
      PT_List,
      Pt_Record,
      PT_Other);

   type Named_Element is
     (PO_Error,

   --  The following elements are used only for AADL_V1
      PO_Port_Group,
      PO_Server_Subprogram,
      PO_Parameter,
      PO_Connections,
      PO_Port_Connections,
      PO_Port_Group_Connections,
      PO_Event_Port_Connections,
      PO_Data_Port_Connections,
      PO_Event_Data_Port_Connections,
      PO_Parameter_Connections,

   --  The following elements are used for AADL_V1 and AADL_V2
      PO_Component_Category,
      PO_Mode,
      PO_Flow,
      PO_Port,
      PO_Event_Port,
      PO_Data_Port,
      PO_Event_Data_Port,

      PO_Access_Connection,    --  POC_Access_Connections

   --  The following elements are used only for AADL_V2
      PO_Identifier,

      PO_Named_Element,
      PO_Classifier,
      PO_Instance,
      PO_Subcomponent,

      PO_Component_Implementation,
      PO_Component_Classifier,
      PO_Component_Instance,
      PO_Component_Type,
      PO_Component_Subcomponent,
      PO_Component_Access,
      PO_Component_Access_Connection,

      PO_Feature,
      PO_Feature_Instance,
      PO_Feature_Group,
      PO_Feature_Group_Type,
      PO_Feature_Group_Connection,
      PO_Feature_Group_Instance,

      PO_Connection,
      PO_Connection_Instance,

      PO_Flow_Specification,
      PO_Flow_Specification_Instance,
      PO_Flow_Source_Specification,
      PO_Flow_Sink_Specification,
      PO_Flow_Path_Specification,
      PO_End_To_End_Flow,
      PO_End_To_End_Flow_Instance,

      PO_Access,

      PO_Port_Instance,
      PO_Port_Connection,
      PO_Port_Connection_Instance,
      PO_Access_Instance,
      PO_Access_Connection_Instance,

      PO_Mode_Transition,
      PO_Mode_Instance,
      PO_Mode_Transition_Instance,
      PO_Mode_Transition_Connection_Instance,

      PO_Subprogram_Call_Sequence,
      PO_Provides_Subprogram_Access,

      PO_Prototype,
      PO_Package,

      PO_Alien_Meta_Model -- Not support alien meta-model elements, e.g. EMV2
      );

   type Referable_Element_Category is
     (REC_Component_Category,
      REC_Connections,
      REC_Server_Subprogram,       --  only in AADL_V1

      REC_Subprogram_Call_Sequence,
      REC_Identifier);

   -----------------------------
   -- Interrogation functions --
   -----------------------------

   function Value_Of_Property_Association_Is_Undefined
     (Property : Node_Id) return Boolean;

   function Type_Of_Property_Is_A_List (Property : Node_Id) return Boolean;

   function Get_Type_Of_Property
     (Property             : Node_Id;
      Use_Evaluated_Values : Boolean := True) return Property_Type;

   function Get_Type_Of_Property_Value
     (Property_Value       : Node_Id;
      Use_Evaluated_Values : Boolean := True) return Property_Type;

   function Get_Integer_Of_Property_Value
     (Property_Value : Node_Id) return Unsigned_Long_Long;

   function Get_Float_Of_Property_Value
     (Property_Value : Node_Id) return Long_Long_Float;

   function Get_String_Of_Property_Value
     (Property_Value : Node_Id) return Name_Id;

   function Get_String_Of_Property_Value
     (Property_Value : Node_Id) return String;

   function Get_Enumeration_Of_Property_Value
     (Property_Value : Node_Id) return Name_Id;

   function Get_Enumeration_Of_Property_Value
     (Property_Value : Node_Id) return String;

   function Get_Boolean_Of_Property_Value
     (Property_Value : Node_Id) return Boolean;

   function Get_Classifier_Of_Property_Value
     (Property_Value : Node_Id) return Node_Id;

   function Get_Reference_Of_Property_Value
     (Property_Value : Node_Id) return Node_Id;

   function Get_Record_Of_Property_Value
     (Property_Value : Node_Id) return List_Id;
   --  For record, values as stored as a list

   function Get_Value_Of_Property_Association
     (Property : Node_Id) return Value_Type;

   function Find_Property_Association_From_Name
     (Property_List : List_Id;
      Property_Name : Name_Id;
      In_Mode       : Name_Id := No_Name) return Node_Id;

   function Find_Property_Association_From_Name
     (Property_List : List_Id;
      Property_Name : String;
      In_Mode       : Name_Id := No_Name) return Node_Id;

   procedure Resolve_Term_In_Property
     (Property  : Node_Id;
      Value     : Node_Id;
      Kind_Node : Node_Kind);

end Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
