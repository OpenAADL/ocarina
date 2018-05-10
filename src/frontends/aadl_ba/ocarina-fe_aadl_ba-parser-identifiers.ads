------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.FE_AADL_BA.PARSER.IDENTIFIERS                   --
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

package Ocarina.FE_AADL_BA.Parser.Identifiers is

   function Make_Current_Identifier (Entity : Node_Id) return Node_Id;
   --  Make an identifier from current token

   function P_Identifier (Container : Types.Node_Id) return Node_Id;

   function P_Identifier_With_Value (Container : Types.Node_Id) return Node_Id;
   --  Return an Identifier or an Identifier_With_Value node

   function P_Unique_Classifier_Reference
     (Container : Types.Node_Id) return Node_Id;

end Ocarina.FE_AADL_BA.Parser.Identifiers;
