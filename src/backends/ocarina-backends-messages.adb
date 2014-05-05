------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . B A C K E N D S . M E S S A G E S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

with GNAT.OS_Lib;     use GNAT.OS_Lib;
with Ocarina.Output;          use Ocarina.Output;
with Ocarina.Options; use Ocarina.Options;

package body Ocarina.Backends.Messages is

   procedure Display_Error_Message
     (Message : String;
      Fatal   : Boolean;
      Warning : Boolean);

   ---------------------------
   -- Display_Error_Message --
   ---------------------------

   procedure Display_Error_Message
     (Message : String;
      Fatal   : Boolean;
      Warning : Boolean)
   is
      Current_FD : constant File_Descriptor := Current_Output;
   begin
      Set_Standard_Error;

      Write_Str ("Backends:");

      if Fatal then
         Write_Str (" fatal");
      end if;

      if Warning then
         Write_Str (" warning : ");
      else
         Write_Str (" error : ");
      end if;

      Write_Line (Message);
      Set_Output (Current_FD);
   end Display_Error_Message;

   -------------------
   -- Display_Error --
   -------------------

   procedure Display_Error
     (Message : String;
      Fatal   : Boolean;
      Warning : Boolean := False) is
   begin
      Display_Error_Message (Message, Fatal, Warning);

      if Fatal then
         OS_Exit (2);
      end if;
   end Display_Error;

   ---------------------------
   -- Display_Located_Error --
   ---------------------------

   procedure Display_Located_Error
     (Loc     : Location;
      Message : String;
      Fatal   : Boolean;
      Warning : Boolean := False)
   is
      Current_FD : constant File_Descriptor := Current_Output;
   begin
      Set_Standard_Error;
      Write_Str (Image (Loc) & ' ');
      Display_Error_Message (Message, Fatal, Warning);
      Set_Output (Current_FD);

      if Fatal then
         OS_Exit (2);
      end if;
   end Display_Located_Error;

   ---------------------------
   -- Display_Debug_Message --
   ---------------------------

   procedure Display_Debug_Message
     (Message : String;
      Force   : Boolean := False) is
   begin
      if Debug_Mode or else Force then
         Write_Str ("Backends message: ");
         Write_Line (Message);
      end if;
   end Display_Debug_Message;

end Ocarina.Backends.Messages;
