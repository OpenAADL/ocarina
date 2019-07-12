------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . P O _ H I _ A D A . J O B        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2019 ESA & ISAE.                       --
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

--  This package contains routine to build the subtree relative to the
--  <Node>_Job package that contains the mapping of periodic
--  threads and shared data.

package Ocarina.Backends.PO_HI_Ada.Job is

   package Package_Spec is

      procedure Visit (E : Node_Id);

   end Package_Spec;

   package Package_Body is

      procedure Visit (E : Node_Id);

   end Package_Body;

end Ocarina.Backends.PO_HI_Ada.Job;
