------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B E _ A A D L . C O M P O N E N T S . A R R A Y S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2008, GET-Telecom Paris.                   --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Output;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.BE_AADL.Identifiers;
with Ocarina.BE_AADL.Properties.Values;

package body Ocarina.BE_AADL.Components.Arrays is

   use Output;
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
