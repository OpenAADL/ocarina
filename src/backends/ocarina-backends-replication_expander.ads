------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.REPLICATION_EXPANDER                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

--  This package holds the replication tree expander which converts the
--  AADL replication property set into an expanded model with different
--  replicas.

with Ocarina.Backends.Replication_Properties;
use Ocarina.Backends.Replication_Properties;

package Ocarina.Backends.Replication_Expander is

   type Replication_Info is record
     Replicated_Component : Node_Id := No_Node;
     Description          : Name_Id := No_Name;
     Number               : Unsigned_Long_Long := 0;
     Replication_Type     : Replication_Types := Replication_None;
     Identifiers          : List_Id := No_List;
     Consensus_Algo       : Node_List;
   end record;

   procedure Expand_With_Replicas (Root : Node_Id);
   --  Expands the AADL node E into another instance enriched with
   --  replication Concepts. Its is based on model transformation
   --  and analysis of the replication property set.

   procedure Generate (AADL_Root : Types.Node_Id);

   procedure Init;

end Ocarina.Backends.Replication_Expander;
