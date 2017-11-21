------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . I N S T A N C E S . R E A L _ F I N D E R         --
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
