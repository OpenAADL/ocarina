------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . R E A L . D E B U G           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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
with Namet;
with Ocarina.ME_REAL.REAL_Tree.Nodes;

package body Ocarina.Backends.REAL.Debug is
   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Namet;
   use Output;

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
