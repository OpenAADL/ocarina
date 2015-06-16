------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.PROPERTIES.ARINC653                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2014-2015 ESA & ISAE.                    --
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

---  This package provides accessors to the property set of the
---  ARINC653 annex document, published as part of AS5506/1A standard.

package Ocarina.Backends.Properties.ARINC653 is

   type Schedule_Window_Record_Term is record
      Partition : Node_Id;
      Duration : Time_Type;
      Periodic_Processing_Start : Boolean;
   end record;

   type Schedule_Window_Record_Term_Array is array (Natural range <>)
     of Schedule_Window_Record_Term;

   Empty_Schedule_Window_Record_Term_Array :
     constant Schedule_Window_Record_Term_Array (1 .. 0)
     := (others => (No_Node, (0, Picosecond), False));

   function Get_Module_Schedule_Property
     (E : Node_Id)
     return Schedule_Window_Record_Term_Array;

end Ocarina.Backends.Properties.ARINC653;
