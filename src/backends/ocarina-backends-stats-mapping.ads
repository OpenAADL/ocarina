------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . S T A T S . M A P P I N G        --
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

--  This package contains routines to map entities from the AADL tree
--  into entities of the Stats XML tree.

package Ocarina.Backends.Stats.Mapping is

   function Map_Distributed_Application (E : Node_Id) return Node_Id;
   function Map_HI_Node (E : Node_Id) return Node_Id;
   function Map_HI_Unit (E : Node_Id) return Node_Id;
   function Map_Port (F : Node_Id) return Node_Id;
   function Get_Virtual_Bus (E : Node_Id) return Node_Id;
   function Map_Data (E : Node_Id) return Node_Id;
   function Map_Subprogram (E : Node_Id) return Node_Id;
   function Map_Data_Access (E : Node_Id) return Node_Id;
   function Map_Thread (E : Node_Id) return Node_Id;
   function Map_Subprogram_Access (E : Node_Id) return Node_Id;
   function Map_Bus_Access (E : Node_Id) return Node_Id;
   function Map_System
     (E       : Node_Id;
      Is_Root : Boolean := False) return Node_Id;
   function Map_Process (E : Node_Id) return Node_Id;
   function Map_Virtual_Processor (E : Node_Id) return Node_Id;
   function Map_Processor (E : Node_Id) return Node_Id;
   function Map_Bus (E : Node_Id) return Node_Id;
   function Map_Port_Connection (E : Node_Id) return Node_Id;

end Ocarina.Backends.Stats.Mapping;
