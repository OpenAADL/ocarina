------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.INSTANCES.COMPONENTS.SUBPROGRAM_CALLS               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

package Ocarina.Instances.Components.Subprogram_Calls is

   function Instantiate_Call_Sequence
     (Instance_Root : Node_Id;
      Call_Sequence : Node_Id) return Node_Id;

   function Instantiate_Subprogram_Call
     (Instance_Root   : Node_Id;
      Subprogram_Call : Node_Id) return Node_Id;

   function Duplicate_Subprogram_Call_Instance
     (Instance_Root : Node_Id;
      Call_Instance : Node_Id) return Node_Id;
   --  Create a new instance of the corresponding subprogram. This is
   --  useful if some properties apply to a given subprogram call.

end Ocarina.Instances.Components.Subprogram_Calls;
