------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.REPLICATION_PROPERTIES                  --
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

package Ocarina.Backends.Replication_Properties is

   procedure Init;
   --  Initialize some internal parts of the package

   ----------------------------
   -- Replication_Properties --
   ----------------------------

   type Replication_Types is
     (Replication_Active,
      Replication_Passive,
      Replication_None); --  Unspecified

   function Get_Replication_Description (C : Node_Id) return Name_Id;
   --  Returns the value of the Description property.

   function Get_Replication_Description
     (Property_Node    : Node_Id;
      Applies_To_Node  : Node_Id)
      return String;
   --  returns the value of the description property when its is
   --  applied to the Applies_To_Node node.

   function Get_Replication_Replica_Number
      (Property_Node   : Node_Id;
       Applies_To_Node : Node_Id)
      return Unsigned_Long_Long;
   --  Returns the value of the Replica_Number property when its is
   --  applied to the Applies_To_Node node.

   function Get_Replication_Replica_Number (C : Node_Id)
      return Unsigned_Long_Long;
   --  Returns the value of the Replica_Number property.

   function Get_Replication_Min_Nbr_Replica (C : Node_Id)
      return Unsigned_Long_Long;
   --  Returns the value of the Min_Nbr_Replica property.

   function Get_Replication_Max_Nbr_Replica (C : Node_Id)
      return Unsigned_Long_Long;
   --  Returns the value of the Max_Nbr_Replica property.

   function Get_Replication_Replica_Identifiers (C : Node_Id)
      return List_Id;
   --  Returns the value of the Replica_Identifiers property.

   function Get_Replication_Replica_Identifiers
   (Property_Node   : Node_Id;
    Applies_To_Node : Node_Id)
   return List_Id;
   --  Returns the value of the Replica_Identifiers property
   --  when its is applied to the Applies_To_Node node.

   function Get_Replication_Replica_Type (C : Node_Id)
      return Replication_Types;
   --  Returns the value of the Replica_Type property.

   function Get_Replication_Replica_Type
   (Property_Node   : Node_Id;
    Applies_To_Node : Node_Id)
   return Replication_Types;
   --  Returns the value of the Replica_Type property
   --  when its is applied to the Applies_To_Node node

   function Get_Replication_Consensus_Algorithm
     (C       : Node_Id;
      In_Mode : Name_Id := No_Name)
     return Name_Id;
   --  Return the consensus algorithm of the given replicated component.
   --  Return No_Name in case the property is not defined for the replicated
   --  component.
   --  If In_Mode is a valid mode name for the replicated component, return the
   --  property association declared in the context of this mode.

   procedure Get_Replication_Consensus_Algorithm
   (Property_Node   : Node_Id;
    Consensus_Node_List : in out Node_List);
   --  updates the consensus algorithm list of the given replicated component.
   --  the list contains the diffrent types of the consensus algorithm
   --  (source_Text, reference or classifier), the feature to each is applied
   --  and the subprogram description.

   function Get_Replication_Consensus_Algorithm
     (C       : Node_Id;
      In_Mode : Name_Id := No_Name)
     return Node_Id;
   --  Same as above, but returns the whole property association

   function Is_Defined_Consensus_Algorithm_Property
     (C       : Node_Id;
      In_Mode : Name_Id := No_Name)
     return Boolean;
   --  Returns True IF the property describing the consensus algorithm
   --  is defined for the replicated component.

end Ocarina.Backends.Replication_Properties;
