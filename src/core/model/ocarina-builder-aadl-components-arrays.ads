------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL.COMPONENTS.ARRAYS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

package Ocarina.Builder.AADL.Components.Arrays is

   function Add_New_Array_Dimension_Size
     (Loc            : Location;
      Container      : Node_Id;
      Dimension_Size : Node_Id) return Node_Id;
   --  Create a new array_dimension node. Container is the array which
   --  contain one or many dimensions. Dimension_Size is the size
   --  associate to the dimension. When the Dimension_Size is not specify
   --  the declaration is incomplete.

   function Add_New_Array_Selection
     (Loc        : Location;
      Container  : Node_Id;
      Identifier : Node_Id;
      Range_List : List_Id) return Node_Id;

   function Add_New_Range_Selection
     (Container   : Node_Id;
      Lower_Bound : Node_Id;
      Upper_Bound : Node_Id) return Node_Id;

end Ocarina.Builder.AADL.Components.Arrays;
