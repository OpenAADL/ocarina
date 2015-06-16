------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . M E _ A A D L                       --
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

package Ocarina.ME_AADL is

   type Entity_Category is
     (EC_Feature,
      EC_Subcomponent,
      EC_Property_Association,
      EC_Flow_Spec,
      EC_Flow_Implementation,
      EC_Call_Sequence,
      EC_Subprogram_Call,
      EC_Connection,
      EC_Mode,
      EC_Component,
      EC_Feature_Group_Type,
      EC_Package,
      EC_Property_Set,
      EC_Property_Name,
      EC_Property_Type,
      EC_Annex,
      EC_Undefined);

   type Component_Category is
     (CC_Abstract,
      CC_Data,
      CC_Subprogram,
      CC_Subprogram_Group,
      CC_Thread,
      CC_Thread_Group,
      CC_Process,
      CC_Memory,
      CC_Processor,
      CC_Virtual_Processor,
      CC_Bus,
      CC_Virtual_Bus,
      CC_Device,
      CC_System,
      CC_Unknown);

   type Connection_Type is
     (CT_Error,

   --  AADL_V1
      CT_Event_Data,
      CT_Data_Delayed,

   --  AADL_V1 and AADL_V2
      CT_Data,
      CT_Event,
      CT_Feature_Group,
      CT_Parameter,
      CT_Access_Bus,
      CT_Access_Data,
      CT_Access_Subprogram,

   --  AADL_V2
      CT_Access_Virtual_Bus,
      CT_Feature,
      CT_Port_Connection,
      CT_Access_Subprogram_Group,
      CT_Access);

   --  Note: CT_Data represents Data_Immediate in AADL_V1

   subtype Port_Connection_Type is
     Connection_Type range
       CT_Data ..
         CT_Event_Data; --  XXX is this type empty ????

   type Flow_Category is (FC_Source, FC_Sink, FC_Path);

end Ocarina.ME_AADL;
