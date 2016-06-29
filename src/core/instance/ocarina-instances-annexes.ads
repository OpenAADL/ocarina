------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . I N S T A N C E S . A N N E X E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010, GET-Telecom Paris.                   --
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
