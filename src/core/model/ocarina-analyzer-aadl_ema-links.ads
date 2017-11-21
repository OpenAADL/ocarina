------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . A N A L Y Z E R . A A D L _ E M A . L I N K S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

package Ocarina.Analyzer.AADL_EMA.Links is

   function Link_Properties_Of_Component
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link of a property association to a component

   procedure Link_Error_Type_Library_List
     (Root    : Node_Id;
      Node    : Node_Id);
   --  Perform the link between the given package in the
   --  error_type_library_list to the library contained in the package.

   procedure Link_Error_Type_Reference
     (Error_Model_Library_Ref : Node_Id;
      Node_Referenced         : Node_Id);

end Ocarina.Analyzer.AADL_EMA.Links;
