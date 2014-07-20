------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . F E _ A A D L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

with Ocarina.Output;

with Ocarina.FE_AADL.Parser;
with Ocarina.ME_AADL.Tokens;

package body Ocarina.FE_AADL is

   use Ocarina.Output;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Ocarina.ME_AADL.Tokens.Init_Tokens;
      Ocarina.ME_AADL.Tokens.Init_Property_Owner_Tokens;
      FE_AADL.Parser.Init;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      --  Nothing to be done
      null;
   end Reset;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Write_Line ("   -y  Automatically load AADL files on demand");
      Write_Line ("   -f  Parse predefined non standard property sets");
      Write_Line ("   -i  Instantiate the AADL model");
      Write_Line ("   -r  <name> The name of the instance tree root");
      Write_Line ("   -o  Specify output file");
      Write_Line ("   -I  Specify the inclusion paths");
      Write_Str ("   -aadlv1  Use AADL v1 standard");
      if Default_AADL_Version = AADL_V1 then
         Write_Str (" (default)");
      end if;
      Write_Eol;
      Write_Str ("   -aadlv2  Use AADL v2 standard");
      if Default_AADL_Version = AADL_V2 then
         Write_Str (" (default)");
      end if;
      Write_Eol;
   end Usage;

end Ocarina.FE_AADL;
