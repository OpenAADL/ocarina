------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . I N S T A N C E S . C O M P O N E N T S . M O D E S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2007-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

--  This package contains all the necessary routines to instantiate
--  AADL component modes.

package Ocarina.Instances.Components.Modes is

   function Instantiate_Mode
     (Instance_Root      : Node_Id;
      Component_Instance : Node_Id;
      Mode               : Node_Id) return Node_Id;
   --  Create a new mode instance corresponding to the mode
   --  declaration 'Mode' of the corresponding component of
   --  'Component_Instance'.

   function Instantiate_Mode_Transition
     (Instance_Root      : Node_Id;
      Component_Instance : Node_Id;
      Mode_Transition    : Node_Id) return Node_Id;
   --  Same as above but for mode transitions

   procedure Instantiate_In_Modes
     (Component_Instance : Node_Id;
      Subclause_Instance : Node_Id);
   --  Resolve the "in modes" clause of the declaration correspoding
   --  to Subclause_Instance basing on the Component_Instance mode
   --  instances. Do nothing if the declaration corresponding to
   --  Subclause_Instance is mode independant.

   procedure Instantiate_In_Modes
     (Component_Instance : Node_Id;
      Subclause_List     : List_Id);
   --  If Subclause_List is not empty, invoke Instantiate_In_Modes on each
   --  element of the list. otherwise, do nothing.

end Ocarina.Instances.Components.Modes;
