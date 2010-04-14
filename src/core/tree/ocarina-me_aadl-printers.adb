------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . M E _ A A D L . P R I N T E R S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2005-2008, GET-Telecom Paris.                --
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

with GNAT.OS_Lib;                     use GNAT.OS_Lib;

with Namet;                           use Namet;
with Output;                          use Output;
with Ocarina.Options;                 use Ocarina.Options;

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
