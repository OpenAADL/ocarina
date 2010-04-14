------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . B A C K E N D S . M E S S A G E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2005-2008, GET-Telecom Paris.                --
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

with Locations; use Locations;

package Ocarina.Backends.Messages is

   procedure Display_Error
     (Message : String;
      Fatal   : Boolean;
      Warning : Boolean := False);
   --  Displays an error message on the standard error output.
   --  If the fatal parameter is set to TRUE, the procedure
   --  exits to the OS after displaying the message.
   --  At the end of this procedure, the current output is the
   --  standard output

   procedure Display_Located_Error
     (Loc     : Location;
      Message : String;
      Fatal   : Boolean;
      Warning : Boolean := False);
   --  Same as above but the message is prefixed with the location Loc

   procedure Display_Debug_Message
     (Message : String;
      Force   : Boolean := False);
   --  Display a message on the standard error. The message is not
   --  necessarily an error message. If Force is True, the value of
   --  the Output_Debug_Messages will not be taken into account

end Ocarina.Backends.Messages;
