------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL.COMPONENTS.ARRAYS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2012 ESA & ISAE.        --
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

package Ocarina.Builder.AADL.Components.Arrays is

   function Add_New_Array_Dimension_Size
     (Loc            : Location;
      Container      : Node_Id;
      Dimension_Size : Node_Id)
     return Node_Id;
   --  Create a new array_dimension node. Container is the array which
   --  contain one or many dimensions. Dimension_Size is the size
   --  associate to the dimension. When the Dimension_Size is not specify
   --  the declaration is incomplete.

   function Add_New_Array_Selection
     (Loc             : Location;
      Container       : Node_Id;
      Identifier      : Node_Id;
      Range_List      : List_Id)
     return Node_Id;

   function Add_New_Range_Selection
     (Container   : Node_Id;
      Lower_Bound : Node_Id;
      Upper_Bound : Node_Id)
     return Node_Id;

end Ocarina.Builder.AADL.Components.Arrays;
