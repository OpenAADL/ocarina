------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . I N S T A N C E S . A N N E X E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2010-2016 ESA & ISAE.                    --
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

package Ocarina.Instances.Annexes is

   function Apply_Annexes
     (Instance_Root : Node_Id;
      Instance      : Node_Id;
      Annex_List    : List_Id;
      Override_Mode : Boolean)
     return Boolean;
   --  Add annexes to the entity instance. If 'Override_Mode' is set
   --  any previous homonym annex under the same mode will be
   --  overriden. Otherwise, the old value will be kept.

   function Add_Annex_Instance
     (Instance_Root   : Node_Id;
      Entity_Instance : Node_Id;
      Annex_Subclause : Node_Id;
      Override_Mode   : Boolean)
     return Boolean;
   --  Same as above but for one single annex

end Ocarina.Instances.Annexes;
