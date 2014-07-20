------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.FE_AADL.PARSER.COMPONENTS.FLOWS                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

--  This package gathers all functions related to flows (which are
--  used within components).

package Ocarina.FE_AADL.Parser.Components.Flows is

   function P_Flow_Implementation_Or_End_To_End_Flow_Spec
     (Container : Node_Id;
      Refinable : Boolean) return Node_Id;
   --  Parse Flow_Implementation, Flow_Implementation_Refinement,
   --        End_To_End_Flow_Spec and End_To_End_Flow_Refinement.
   --  If Refinable = FALSE, refinements are not allowed

   function P_Flow_Spec
     (Container : Node_Id;
      Refinable : Boolean) return Node_Id;
   --  Parse Flow_Spec and Flow_Spec_Refinement
   --  If Refinable = FALSE, Flow_Spec_Refinement is not allowed

end Ocarina.FE_AADL.Parser.Components.Flows;
