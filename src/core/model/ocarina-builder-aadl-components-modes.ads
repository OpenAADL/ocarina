------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BUILDER.AADL.COMPONENTS.MODES                   --
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

--  This package provides functions to handle modes in the component
--  implementations.

package Ocarina.Builder.AADL.Components.Modes is

   function Add_Property_Association
     (Mode : Node_Id;
      Property_Association : Node_Id)
     return Boolean;
   --  Add a property association to the mode declaration or mode
   --  transition. Mode must either reference a mode declaration or a
   --  mode transition. Property_Association references the property
   --  association. Return True if everything went right, else False.

   function Add_New_Mode
     (Loc : Location;
      Identifier : Node_Id;
      Component : Node_Id)
     return Node_Id;
   --  Add a new mode declaration into a component implementation. Loc
   --  is the location of the mode declaration in the parsed
   --  text. Identifier references an identifier containing the name
   --  of the mode. Component references the component
   --  implementation. Is_Implicit is used by other parts of the
   --  builder API, for "in modes" clauses. You should always keep it
   --  False. Return a Node_Id referencing the newly created mode if
   --  everything went right, else False.

   function Add_New_Mode_Transition
     (Loc : Location;
      Component : Node_Id)
     return Node_Id;
   --  Add a new empty mode transition into a component
   --  implementation. Source, Destination, etc. of the mode
   --  transition must be added manually after the node has been
   --  created. Loc is the location of the mode transition in the
   --  parsed text. Identifier references an identifier containing the
   --  name of the mode. Component references the component
   --  implementation. Return a Node_Id referencing the newly created
   --  mode if everything went right, else False.

   function Add_New_Mode_Transition_Trigger
     (Loc : Locations.Location;
      Identifier : Types.Node_Id;
      Is_Self : Boolean;
      Is_Processor : Boolean)
     return Types.Node_Id;

end Ocarina.Builder.AADL.Components.Modes;
