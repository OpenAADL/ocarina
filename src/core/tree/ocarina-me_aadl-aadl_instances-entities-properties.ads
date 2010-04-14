------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           OCARINA.ME_AADL.AADL_INSTANCES.ENTITIES.PROPERTIES             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2008-2009, GET-Telecom Paris.                --
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

--  This package provides functions to create or read property names,
--  types, constants and associations.

with Types;                           use Types;
with Ocarina.AADL_Values;             use Ocarina.AADL_Values;

package Ocarina.ME_AADL.AADL_Instances.Entities.Properties is

   -----------------------------
   -- Interrogation functions --
   -----------------------------

   function Get_Value_Of_Property_Association
     (Property : Node_Id)
     return Value_Type;

   function Find_Property_Association_From_Name
     (Property_List : List_Id;
      Property_Name : Name_Id;
      In_Mode       : Name_Id := No_Name)
     return Node_Id;

   function Find_Property_Association_From_Name
     (Property_List : List_Id;
      Property_Name : String;
      In_Mode       : Name_Id := No_Name)
     return Node_Id;

end Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
