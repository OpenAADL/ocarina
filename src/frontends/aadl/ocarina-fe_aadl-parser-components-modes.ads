------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.FE_AADL.PARSER.COMPONENTS.MODES                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2008, GET-Telecom Paris.                   --
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

--  This package gathers all functions that are related to modes

package Ocarina.FE_AADL.Parser.Components.Modes is

   function P_In_Modes (Code : Parsing_Code) return Node_Id;
   --  Parse In_Modes and In_Modes_And_Transitions, of syntax
   --        ( in modes ( ( Item { , Item }* | none_statement ) ) )
   --  Current token is 'in' and will be ignored in this function

   function P_Mode_Or_Mode_Transition
     (Container : Types.Node_Id;
      Refinable : Boolean)
     return Node_Id;
   --  Parse Mode, Mode_Refinement and Mode_Transition
   --  If Refinable = FALSE, Mode_Refinement is not allowed

   function P_Mode_Or_Transition (Container : Types.Node_Id) return Node_Id;
   pragma Inline (P_Mode_Or_Transition);
   --  Parse Mode_Or_Transition return a node of K_Identifier_Identifier

end Ocarina.FE_AADL.Parser.Components.Modes;
