------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . F E _ R E A L                       --
--                                                                          --
--                                 B o d y                                  --
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
