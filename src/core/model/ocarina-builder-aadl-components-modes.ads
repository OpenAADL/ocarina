------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BUILDER.AADL.COMPONENTS.MODES                   --
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

--  This package provides functions to handle modes in the component
--  implementations.

package Ocarina.Builder.AADL.Components.Modes is

   function Add_Property_Association
     (Mode                 : Node_Id;
      Property_Association : Node_Id) return Boolean;
   --  Add a property association to the mode declaration or mode
   --  transition. Mode must either reference a mode declaration or a
   --  mode transition. Property_Association references the property
   --  association. Return True if everything went right, else False.

   function Add_New_Mode
     (Loc        : Location;
      Identifier : Node_Id;
      Component  : Node_Id) return Node_Id;
   --  Add a new mode declaration into a component implementation. Loc
   --  is the location of the mode declaration in the parsed
   --  text. Identifier references an identifier containing the name
   --  of the mode. Component references the component
   --  implementation. Is_Implicit is used by other parts of the
   --  builder API, for "in modes" clauses. You should always keep it
   --  False. Return a Node_Id referencing the newly created mode if
   --  everything went right, else False.

   function Add_New_Mode_Transition
     (Loc       : Location;
      Component : Node_Id) return Node_Id;
   --  Add a new empty mode transition into a component
   --  implementation. Source, Destination, etc. of the mode
   --  transition must be added manually after the node has been
   --  created. Loc is the location of the mode transition in the
   --  parsed text. Identifier references an identifier containing the
   --  name of the mode. Component references the component
   --  implementation. Return a Node_Id referencing the newly created
   --  mode if everything went right, else False.

   function Add_New_Mode_Transition_Trigger
     (Loc          : Locations.Location;
      Identifier   : Types.Node_Id;
      Is_Self      : Boolean;
      Is_Processor : Boolean) return Types.Node_Id;

end Ocarina.Builder.AADL.Components.Modes;
