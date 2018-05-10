------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . B A C K E N D S . M E S S A G E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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
