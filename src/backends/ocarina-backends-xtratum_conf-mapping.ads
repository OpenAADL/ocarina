------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.XTRATUM_CONF.MAPPING                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2011-2014 ESA & ISAE.                    --
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

with Ocarina.Backends.Properties; use Ocarina.Backends.Properties;

package Ocarina.Backends.Xtratum_Conf.Mapping is

   function Map_Distributed_Application (E : Node_Id) return Node_Id;
   function Map_HI_Node (E : Node_Id) return Node_Id;
   function Map_HI_Unit (E : Node_Id) return Node_Id;
   function Map_Port (F : Node_Id) return Node_Id;
   function Map_Data (E : Node_Id) return Node_Id;
   function Map_Data_Access (E : Node_Id) return Node_Id;
   function Map_Bus_Access (E : Node_Id) return Node_Id;
   function Map_System (E : Node_Id) return Node_Id;
   function Map_Process
     (E                    : Node_Id;
      Partition_Identifier : Unsigned_Long_Long) return Node_Id;
   function Map_Data_Size (T : Size_Type) return Unsigned_Long_Long;
   function Map_Virtual_Processor (E : Node_Id) return Node_Id;
   function Map_Processor (E : Node_Id) return Node_Id;
   function Map_Bus (E : Node_Id) return Node_Id;
   function Map_Port_Connection (E : Node_Id) return Node_Id;
   function Map_Process_Memory (Process : Node_Id) return Node_Id;
   function Map_System_Description (System : Node_Id) return Node_Id;
   function Map_Connection
     (Connection         : Node_Id;
      Channel_Identifier : Unsigned_Long_Long) return Node_Id;

   function Map_Process_HM_Table (Process : Node_Id) return Node_Id;

   function Map_Processor_HM_Table (Processor : Node_Id) return Node_Id;

   function Map_System_HM_Table (System : Node_Id) return Node_Id;

end Ocarina.Backends.Xtratum_Conf.Mapping;
