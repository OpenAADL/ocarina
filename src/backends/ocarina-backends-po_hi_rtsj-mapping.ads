------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ H I _ R T S J . M A P P I N G   --
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

--  This package contains routines to map entities from the AADL tree
--  into entities of the PolyORB-HI Java tree

with Ocarina.Backends.Properties; use Ocarina.Backends.Properties;

package Ocarina.Backends.PO_HI_RTSJ.Mapping is

   function Map_Distributed_Application (E : Node_Id) return Node_Id;

   function Map_HI_Node (E : Node_Id) return Node_Id;

   function Map_HI_Unit (E : Node_Id) return Node_Id;

   function Map_RTSJ_Defining_Identifier
     (E      : Node_Id;
      Is_Obj : Boolean := False) return Node_Id;

   function Map_RTSJ_Enumerator_Name (E : Node_Id) return Name_Id;

   function Map_RTSJ_Subprogram_Identifier (E : Node_Id) return Node_Id;

   function Map_RTSJ_Subprogram_Spec (E : Node_Id) return Node_Id;

   function Map_RTSJ_Subprogram_Body (E : Node_Id) return Node_Id;

   function Map_Handler_Identifier
     (E            : Node_Id;
      TaskHandler  : Boolean := False;
      EventHandler : Boolean := False) return Node_Id;

   function Map_Handler_Class_Identifier
     (E            : Node_Id;
      TaskHandler  : Boolean := False;
      EventHandler : Boolean := False) return Node_Id;

   function Map_Priority_Identifier (E : Node_Id) return Node_Id;

   function Map_Task_Job_Identifier (E : Node_Id) return Node_Id;

   function Map_Task_Ports_Router_Identifier (E : Node_Id) return Node_Id;

   function Map_Task_Port_Identifier (E : Node_Id) return Node_Id;

   function Map_Task_Entries_Identifier (E : Node_Id) return Node_Id;

   function Map_Task_Deliver_Identifier (E : Node_Id) return Node_Id;

   function Map_Task_Send_Identifier (E : Node_Id) return Node_Id;

   function Map_Port_Default_Entry (E : Node_Id) return Node_Id;

   function Map_Port_Default_Value (E : Node_Id) return Node_Id;

   function Map_Port_Destinations_Tab (E : Node_Id) return Node_Id;

   function Map_Subprogram_Param_Identifier (E : Node_Id) return Node_Id;

   function Map_Time_Unit (T : Time_Type) return Node_Id;

   function Map_Time_Value (T : Time_Type) return Node_Id;

   procedure Bind_AADL_To_Main (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Activity (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Deployment (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Subprogram (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Naming (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Generated_Types (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Transport_High_Level (G : Node_Id; A : Node_Id);

end Ocarina.Backends.PO_HI_RTSJ.Mapping;
