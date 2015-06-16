------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.ANALYZER.AADL.LEGALITY_RULES                    --
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

--  This package gathers all the functions which check that the AADL
--  tree is comformant to what is said in the "legality rules"
--  sections of the standard.

package Ocarina.Analyzer.AADL.Legality_Rules is

   function Check_Legality_Rules (Root : Node_Id) return Boolean;
   --  Verify that the given AADL architecture respects all the
   --  legality rules specified by the AADL standard.

   function A_Component_Feature
     (Component : Node_Id;
      Feature   : Node_Id) return Boolean;
   --  Return True if Feature can be a feature of Component, else
   --  return False.

   function A_Component_Subcomponent
     (Component    : Node_Id;
      Subcomponent : Node_Id) return Boolean;
   --  Return True if Subcomponent can be a subcomponent of Component,
   --  else return False.

   function A_Component_Connection
     (Component  : Node_Id;
      Connection : Node_Id) return Boolean;
   --  Return True if Connection can be a connection of Component,
   --  else return False.

   function A_Component_Flow
     (Component : Node_Id;
      Flow      : Node_Id) return Boolean;
   --  Return True if Flow can be a flow of Component, else return
   --  False

   function A_Component_Flow_Specification
     (Component : Node_Id;
      Flow_Spec : Node_Id) return Boolean;
   --  Return True if Flow_Spec can be a flow specification of
   --  Component, else return False

   function A_Component_Subprogram_Call_Sequence
     (Component           : Node_Id;
      Subprogram_Call_Seq : Node_Id) return Boolean;
   --  Return True if Subprogram_Call can be a subprogram_call of
   --  Component, else return False

end Ocarina.Analyzer.AADL.Legality_Rules;
