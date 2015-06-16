------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P R O P E R T I E S . U T I L S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2014-2015 ESA & ISAE.                    --
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
