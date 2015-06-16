------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . R E A L . D E B U G           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

with Ocarina.Output;
with Ocarina.Namet;
with Ocarina.ME_REAL.REAL_Tree.Nodes;

package body Ocarina.Backends.REAL.Debug is
   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Ocarina.Namet;
   use Ocarina.Output;

   --------------------
   -- Print_All_Sets --
   --------------------

   procedure Print_All_Sets (R : Node_Id) is
      pragma Assert (Kind (R) = K_Theorem);

      N : Node_Id;
   begin
      N := First_Node (Used_Set (R));
      while Present (N) loop

         Write_Line (Get_Name_String (Name (Identifier (N))));

         Display_Set (Set_Array (Integer (Index (Annotation (N)))));

         Write_Line ("...");

         N := Next_Node (N);
      end loop;
   end Print_All_Sets;

end Ocarina.Backends.REAL.Debug;
