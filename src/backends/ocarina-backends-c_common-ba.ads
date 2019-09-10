------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . C _ C O M M O N . B A          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2019 ESA & ISAE.                       --
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

package Ocarina.Backends.C_Common.BA is

   procedure Map_C_Behavior_Variables (S            : Node_Id;
                                       Declarations : List_Id);
   --  This function maps variables declared in the BA variables
   --  section to variables in the corresponding genereted C-subprogram
   --  having the same identifiers. It returns the declarations list of
   --  the generated C-subprogram.

   procedure Map_C_Behavior_Transitions (S            : Node_Id;
                                         Declarations : List_Id;
                                         Statements   : List_Id);

   function Get_Behavior_Specification (S : Node_Id) return Node_Id;

   function Is_To_Make_Init_Sequence (S : Node_Id) return Boolean;

end Ocarina.Backends.C_Common.BA;
