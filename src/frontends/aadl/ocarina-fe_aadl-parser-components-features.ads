------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.FE_AADL.PARSER.COMPONENTS.FEATURES                 --
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

package Ocarina.FE_AADL.Parser.Components.Features is

   function P_Feature
     (Container : Types.Node_Id;
      Refinable : Boolean) return Node_Id;
   --  Parse Feature and Feature_Refinement
   --  If Refinable = FALSE, Feature_Refinement is not allowed

   function P_Feature_Refinement (Container : Types.Node_Id) return Node_Id;
   --  Parse Feature_Refinement exclusively, useful in parsing 'refines type'

   function P_Feature_Group_Or_Port_Group_Or_Port_Spec
     (Container : Types.Node_Id;
      Refinable : Boolean) return Node_Id;
   --  Parse Feature_Group_Spec or Feature_Group_Refinement or
   --        Port_Spec or Port_Refinement or
   --        Port_Group_Spec or Port_Group_Refinement

end Ocarina.FE_AADL.Parser.Components.Features;
