------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P R O P E R T I E S . U T I L S     --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Instances.Queries;

package body Ocarina.Backends.Properties.Utils is
   use Ocarina.Instances.Queries;

   ----------------------------
   -- Check_And_Get_Property --
   ----------------------------

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Unsigned_Long_Long := 0)
  return Unsigned_Long_Long is
   begin
      if Is_Defined_Integer_Property (E, Property_Name) then
         return Get_Integer_Property (E, Property_Name);
      else
         return Default_Value;
      end if;
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Name_Id := No_Name)
     return Name_Id is
   begin
      if Is_Defined_String_Property (E, Property_Name) then
         return Get_String_Property (E, Property_Name);
      else
         return Default_Value;
      end if;
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Node_Id := No_Node)
     return Node_Id is
   begin
      if Is_Defined_Property (E, Property_Name) then
         return Get_Classifier_Property (E, Property_Name);
      else
         return Default_Value;
      end if;
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Boolean := False)
     return Boolean is
   begin
      if Is_Defined_Boolean_Property (E, Property_Name) then
         return Get_Boolean_Property (E, Property_Name);
      else
         return Default_Value;
      end if;
   end Check_And_Get_Property;

end Ocarina.Backends.Properties.Utils;
