------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . B A C K E N D S . D E O S _ C O N F . M A P P I N G    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2015 ESA & ISAE.                       --
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

with Ocarina.Backends.Properties; use Ocarina.Backends.Properties;

package Ocarina.Backends.Deos_Conf.Mapping is

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
   function Map_Partition (Process : Node_Id;
                           Runtime : Node_Id;
                           Partition_Identifier : Integer;
                           Nb_Threads : Unsigned_Long_Long;
                           Nb_Buffers : Unsigned_Long_Long;
                           Nb_Events : Unsigned_Long_Long;
                           Nb_Lock_Objects : Unsigned_Long_Long;
                           Nb_Blackboards : Unsigned_Long_Long;
                           Blackboards_Size : Unsigned_Long_Long;
                           Buffers_Size : Unsigned_Long_Long)
                           return Node_Id;
   function Map_Bus (E : Node_Id) return Node_Id;
   function Map_Port_Connection (E : Node_Id) return Node_Id;
   function Map_Process_Memory (Process : Node_Id) return Node_Id;
   procedure Map_Process_Scheduling
     (Process       :        Node_Id;
      Window_Number : in out Unsigned_Long_Long;
      N             :    out Node_Id);

   function Map_Connection
     (Connection         : Node_Id;
      Channel_Identifier : Unsigned_Long_Long) return Node_Id;

   function Map_Process_HM_Table (Process : Node_Id) return Node_Id;

   function Map_Processor_HM_Table (Processor : Node_Id) return Node_Id;

   function Map_Sampling_Port (Port : Node_Id) return Node_Id;

   function Map_Queuing_Port (Port : Node_Id) return Node_Id;

   function Map_Port_Name (E : Node_Id) return Name_Id;

end Ocarina.Backends.Deos_Conf.Mapping;
