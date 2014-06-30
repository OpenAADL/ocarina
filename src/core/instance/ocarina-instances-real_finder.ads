------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . I N S T A N C E S . R E A L _ F I N D E R         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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

with Ocarina.ME_REAL.REAL_Tree.Utils;

package Ocarina.Instances.REAL_Finder is
   use Ocarina.ME_REAL.REAL_Tree.Utils;

   function Get_Property_Value
     (Property_Name : Name_Id;
      Node          : Node_Id) return Value_Id;
   --  Return the value of the property for a single element Node
   --  Return No_Value if property not found

   function Get_Property_Value_Function
     (Property : Value_Id;
      T        : Return_Type;
      Var      : Node_Id) return Value_Id;
   --  Return the value of the property for a single element Var
   --  T contains the expected property type
   --  If T is a list type, then if the property is not defined
   --  an empty list will be returned instead of No_Value

end Ocarina.Instances.REAL_Finder;
