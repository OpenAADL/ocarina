------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . P N . C O M P O N E N T S        --
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

package Ocarina.Backends.PN.Components is

   function Process_Architecture_Instance
     (Architecture_Instance : Types.Node_Id;
      F                     : Unsigned_Long_Long)  --  0 for CPN, 1 for TPN
      return Types.Node_Id;
   --  Transform the Ocarina tree of the distributed application nodes
   --  into a Petri Net (pn) tree.

private

   type PN_Init_Node is access procedure
     (N    : Types.Node_Id;
      A    : Types.Node_Id;
      Name : Types.Name_Id;
      PN_G : Types.Node_Id;
      M    : Unsigned_Long_Long);

   type PN_Init_Arc is access procedure
     (N : Types.Node_Id;
      A : Types.Node_Id;
      F : Types.Node_Id;
      T : Types.Node_Id;
      K : Unsigned_Long_Long);

   type PN_Dup_Arc is access procedure
     (A        : Types.Node_Id;
      A_Inst   : Types.Node_Id;
      Endpoint : Types.Node_Id;
      From     : Boolean);

   type PN_New_Node is access function return Types.Node_Id;

end Ocarina.Backends.PN.Components;
