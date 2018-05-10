------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . E V E R _ X M L          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2014-2015 ESA & ISAE.                    --
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

package Ocarina.Backends.Ever_XML is

   procedure Init;
   --  Initialize Ocarina.
   --  To be called before any action.

   procedure Generate (AADL_Root : Node_Id);
   --  Generate TPO file for Bound-T

   procedure Reset;
   --  Reset the internal data

   type Tag_XML is
     (Tag_System,
      Tag_Name,
      Tag_Type,
      Tag_Category,
      Tag_Namespace,
      Tag_Features,
      Tag_Feature,
      Tag_Feature_Name,
      Tag_Feature_Direction,
      Tag_Feature_Category,
      Tag_Feature_Port_Data_Type,
      Tag_Feature_Port_Data_Type_Namespace,
      Tag_Feature_Port_Type,
      Tag_Data_Info,
      Tag_Properties,
      Tag_Property,
      Tag_Property_Name,
      Tag_Property_Namespace,
      Tag_Property_Value,
      Tag_Property_Unit,
      Tag_Subcomponents,
      Tag_Subcomponent,
      Tag_Connections,
      Tag_Connection,
      Tag_Connection_Name,
      Tag_Connection_Kind,
      Tag_Connection_Category,
      Tag_Connection_Port_Info,
      Tag_Connection_Port_Info_Source,
      Tag_Connection_Port_Info_Parent_Source,
      Tag_Connection_Port_Info_Parent_Source_Name,
      Tag_Connection_Port_Info_Dest,
      Tag_Connection_Port_Info_Parent_Dest,
      Tag_Connection_Port_Info_Parent_Dest_Name
     );

end Ocarina.Backends.Ever_XML;
