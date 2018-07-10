------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BACKENDS.VXWORKS653_CONF.MAPPING                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2015-2018 ESA & ISAE.                    --
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

package Ocarina.Backends.Vxworks653_Conf.Mapping is

   function Map_HI_Node (E : Node_Id) return Node_Id;
   function Map_HI_Unit (E : Node_Id) return Node_Id;
   function Map_Partition
     (Process              : Node_Id;
      Runtime              : Node_Id;
      Partition_Identifier : Integer;
      Nb_Threads           : Unsigned_Long_Long;
      Nb_Buffers           : Unsigned_Long_Long;
      Nb_Events            : Unsigned_Long_Long;
      Nb_Lock_Objects      : Unsigned_Long_Long;
      Nb_Blackboards       : Unsigned_Long_Long;
      Blackboards_Size     : Unsigned_Long_Long;
      Buffers_Size         : Unsigned_Long_Long) return Node_Id;

   function Map_Partition_Name
     (Runtime         : Node_Id;
      Use_Source_Name : Boolean := True) return Name_Id;

   function Map_Application_Name
     (Runtime         : Node_Id;
      Use_Source_Name : Boolean := True) return Name_Id;

end Ocarina.Backends.Vxworks653_Conf.Mapping;
