------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.FE_AADL.PARSER.COMPONENTS.MODES                  --
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

--  This package gathers all functions that are related to modes

package Ocarina.FE_AADL.Parser.Components.Modes is

   function P_In_Modes (Code : Parsing_Code) return Node_Id;
   --  Parse In_Modes and In_Modes_And_Transitions, of syntax
   --        ( in modes ( ( Item { , Item }* | none_statement ) ) )
   --  Current token is 'in' and will be ignored in this function

   function P_Mode_Or_Mode_Transition
     (Container : Types.Node_Id;
      Refinable : Boolean) return Node_Id;
   --  Parse Mode, Mode_Refinement and Mode_Transition
   --  If Refinable = FALSE, Mode_Refinement is not allowed

   function P_Mode_Or_Transition (Container : Types.Node_Id) return Node_Id;
   pragma Inline (P_Mode_Or_Transition);
   --  Parse Mode_Or_Transition return a node of K_Identifier_Identifier

end Ocarina.FE_AADL.Parser.Components.Modes;
