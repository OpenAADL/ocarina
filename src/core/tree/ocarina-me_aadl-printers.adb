------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . M E _ A A D L . P R I N T E R S              --
--                                                                          --
--                                 B o d y                                  --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ocarina.Namet;           use Ocarina.Namet;
with Ocarina.Output;          use Ocarina.Output;
with Ocarina.Options; use Ocarina.Options;

package body Ocarina.ME_AADL.Printers is

   ---------------------
   -- Print_AADL_Tree --
   ---------------------

   procedure Print_AADL_Tree (Node : Node_Id; W : W_Node_Spg) is
   begin
      if Output_Filename /= No_Name then
         Get_Name_String (Output_Filename);
         Set_Output (Create_File (Name_Buffer (1 .. Name_Len), Binary));
      end if;
      W.all (Node);
      Set_Standard_Output;
   end Print_AADL_Tree;

end Ocarina.ME_AADL.Printers;
