------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . F E _ A A D L _ E M A . P A R S E R            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                  Copyright (C) 2009 Telecom ParisTech,                   --
--                 2010-2019 ESA & ISAE, 2019-2020 OpenAADL                 --
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
--                    Ocarina is maintained by OpenAADL team                --
--                              (info@openaadl.org)                         --
--                                                                          --
------------------------------------------------------------------------------

--  This package gathers the functions that parse the global elements
--  of error model specification.

with Ocarina.Types; use Ocarina.Types;
with Locations;     use Locations;

package Ocarina.FE_AADL_EMA.Parser is

   procedure Init;
   --  Initialize the parser and register it to the general Ocarina
   --  parser.

   function Process (AADL_Root  : Node_Id;
                     From       : Location;
                     To         : Location := No_Location;
                     Container  : Node_Id)
                    return Node_Id;
   --  Proceed to parsing an Error Model Annex. The parsed annex will be
   --  returned and linked to the given AADL node.

end Ocarina.FE_AADL_EMA.Parser;
