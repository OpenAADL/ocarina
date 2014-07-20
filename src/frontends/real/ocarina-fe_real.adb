------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . F E _ R E A L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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

with Ocarina.FE_REAL.Parser;
with Ocarina.ME_REAL.Tokens;

package body Ocarina.FE_REAL is

   use Ocarina.Output;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Ocarina.ME_REAL.Tokens.Init_Tokens;
      Ocarina.FE_REAL.Parser.Init;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      --  FIXME
      --  TODO
      null;
   end Reset;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Write_Line
        ("   -real_lib Add a REAL file to be used as a theorem " &
         "library by REAL annexes");
      Write_Line ("   -real_theorem <theorem> Evaluate only theorem");
      Write_Line
        ("   -real_continue_eval Continue evaluation in case of failures");
   end Usage;

end Ocarina.FE_REAL;
