------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                              O C A R I N A                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Ocarina.Namet;
with Errors;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.AADL_Values; use Ocarina.AADL_Values;

package body Ocarina is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Ocarina.Namet.Initialize;
      Errors.Initialize;
      V_Zero := New_Integer_Value (0);
      V_One  := New_Integer_Value (1);
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Ocarina.Namet.Initialize;
      Errors.Initialize;
      Ocarina.ME_AADL.AADL_Tree.Nutils.Reset_Nodes;
      Ocarina.ME_AADL.AADL_Instances.Nutils.Reset_Nodes;
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
