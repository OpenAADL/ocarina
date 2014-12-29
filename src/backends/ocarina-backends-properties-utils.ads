------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P R O P E R T I E S . U T I L S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2014 ESA & ISAE.                       --
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
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

package Ocarina.Backends.Properties.Utils is

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Unsigned_Long_Long := 0)
     return Unsigned_Long_Long;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Name_Id := No_Name)
     return Name_Id;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Node_Id := No_Node)
     return Node_Id;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : List_Id := No_List)
     return List_Id;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Boolean := False)
     return Boolean;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Names : Name_Array;
      Default_Value : Int := Int'First)
     return Int;

   --  Check Property_Name is set on node E, if so returns its value
   --  otherwise return the default value.

end Ocarina.Backends.Properties.Utils;
