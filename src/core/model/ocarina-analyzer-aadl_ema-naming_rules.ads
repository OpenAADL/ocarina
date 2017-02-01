------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.ANALYZER.AADL_EMA.NAMING_RULES                   --
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

package Ocarina.Analyzer.AADL_EMA.Naming_Rules is

   procedure Put_In_A_List_Node
      (Id_First_Node : in out Node_Id;
       Id_Last_Node  : in out Node_Id;
       Parent_Node   : Node_Id;
       Is_Identifier : Boolean := False);

   function Check_Names_In_Library
     (Root : Node_Id;
      EMA_Root : Node_Id;
      Package_Spec : Node_Id)
     return Boolean;

   function Check_Names_In_Subclause
     (Root : Node_Id;
      EMA_Root : Node_Id;
      Package_Spec : Node_Id)
     return Boolean;

end Ocarina.Analyzer.AADL_EMA.Naming_Rules;
