------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L . P A R S E R . C O M P O N E N T S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

--  This package gathers all functions related to component parsing.

with Ocarina.Types;
with Ocarina.ME_AADL;
with Locations;

use Ocarina.ME_AADL;

package Ocarina.FE_AADL.Parser.Components is
   --  Component categories

   function P_Component
     (Namespace           : Ocarina.Types.Node_Id;
      Private_Declaration : Boolean := False) return Node_Id;
   --  Parse Component_Type, Component_Type_Extension
   --        Component_Implementation, Component_Implementation_Extension

   function P_Feature_Group_Type
     (Namespace           : Ocarina.Types.Node_Id;
      Start_Loc           : Locations.Location;
      Private_Declaration : Boolean := False) return Node_Id;
   --  Parse _Group_Type and _Group_Type_Extension

   function P_Component_Category return Component_Category;
   function P_Component_Category (Container : Ocarina.Types.Node_Id)
                                 return Node_Id;
   --  Parse Component_Category, current token is the first token of
   --  Component_Category

end Ocarina.FE_AADL.Parser.Components;
