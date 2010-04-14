------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.ANALYZER.AADL.LEGALITY_RULES                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

--  This package gathers all the functions which check that the AADL
--  tree is comformant to what is said in the "legality rules"
--  sections of the standard.

package Ocarina.Analyzer.AADL.Legality_Rules is

   function Check_Legality_Rules
     (Root    : Node_Id)
     return Boolean;
   --  Verify that the given AADL architecture respects all the
   --  legality rules specified by the AADL standard.

   function A_Component_Feature
     (Component : Node_Id;
      Feature   : Node_Id)
     return Boolean;
   --  Return True if Feature can be a feature of Component, else
   --  return False.

   function A_Component_Subcomponent
     (Component    : Node_Id;
      Subcomponent : Node_Id)
     return Boolean;
   --  Return True if Subcomponent can be a subcomponent of Component,
   --  else return False.

   function A_Component_Connection
     (Component  : Node_Id;
      Connection : Node_Id)
     return Boolean;
   --  Return True if Connection can be a connection of Component,
   --  else return False.

   function A_Component_Flow
     (Component : Node_Id;
      Flow      : Node_Id)
     return Boolean;
   --  Return True if Flow can be a flow of Component, else return
   --  False

   function A_Component_Flow_Specification
     (Component : Node_Id;
      Flow_Spec : Node_Id)
     return Boolean;
   --  Return True if Flow_Spec can be a flow specification of
   --  Component, else return False

   function A_Component_Subprogram_Call_Sequence
     (Component           : Node_Id;
      Subprogram_Call_Seq : Node_Id)
     return Boolean;
   --  Return True if Subprogram_Call can be a subprogram_call of
   --  Component, else return False

end Ocarina.Analyzer.AADL.Legality_Rules;
