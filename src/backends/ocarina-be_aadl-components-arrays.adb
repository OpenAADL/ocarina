------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B E _ A A D L . C O M P O N E N T S . A R R A Y S     --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Output;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.BE_AADL.Identifiers;
with Ocarina.BE_AADL.Properties.Values;

package body Ocarina.BE_AADL.Components.Arrays is

   use Ocarina.Output;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.BE_AADL.Components;
   use Ocarina.BE_AADL.Identifiers;
   use Ocarina.BE_AADL.Properties.Values;

   ----------------------------
   -- Print_Array_Dimensions --
   ----------------------------

   procedure Print_Array_Dimensions (Array_Dimensions : Node_Id) is
      pragma Assert (Kind (Array_Dimensions) = K_Array_Dimensions);
      List_Node : Node_Id;

   begin
      List_Node := First_Node (Array_List_Dim (Array_Dimensions));

      while Present (List_Node) loop
         Print_Token (T_Left_Square_Bracket);

         if Present (Size (List_Node)) then
            Print_Signed_AADLNumber (Size (List_Node));
         end if;

         Print_Token (T_Right_Square_Bracket);

         List_Node := Next_Node (List_Node);
      end loop;

   end Print_Array_Dimensions;

   ----------------------------
   -- Print_Array_Selections --
   ----------------------------

   procedure Print_Array_Selections (Array_Selections : Node_Id) is

      pragma Assert (Kind (Array_Selections) = K_Array_Selection);

      List_Node : Node_Id;
   begin
      Print_Identifier (Identifier (Array_Selections));

      if not Is_Empty (Range_Selections (Array_Selections)) then
         List_Node := First_Node (Range_Selections (Array_Selections));
         Write_Space;
      end if;

      while Present (List_Node) loop
         Print_Token (T_Left_Square_Bracket);
         Print_Numeric_Term (Lower_Bound (List_Node));

         if Present (Upper_Bound (List_Node)) then
            Write_Space;
            Print_Token (T_Interval);
            Write_Space;

            Print_Numeric_Term (Upper_Bound (List_Node));
         end if;

         Print_Token (T_Right_Square_Bracket);

         List_Node := Next_Node (List_Node);
      end loop;

   end Print_Array_Selections;

end Ocarina.BE_AADL.Components.Arrays;
