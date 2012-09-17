------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                              O C A R I N A                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

with Namet;
with Errors;
with Ocarina.Me_AADL.AADL_Tree.Nutils;
with Ocarina.Me_AADL.AADL_Instances.Nutils;
with Ocarina.AADL_Values; use Ocarina.AADL_Values;

package body Ocarina is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Namet;
   begin
      Namet.Initialize;
      Errors.Initialize;
      V_Zero := New_Integer_Value (0);
      V_One  := New_Integer_Value (1);
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Namet.Initialize;
      Errors.Initialize;
      Ocarina.Me_AADL.AADL_Tree.Nutils.Reset_Nodes;
      Ocarina.Me_AADL.AADL_Instances.Nutils.Reset_Nodes;
      Ocarina.AADL_Values.Reset;
   end Reset;

   ------------------
   -- GNU_Make_Cmd --
   ------------------

   function GNU_Make_Cmd return String is
   begin
      return GNU_Make_Ptr.all;
   end GNU_Make_Cmd;

   ----------------------
   -- Default_GNU_Make --
   ----------------------

   function Default_GNU_Make return String is
   begin
      return "make";
   end Default_GNU_Make;

end Ocarina;
