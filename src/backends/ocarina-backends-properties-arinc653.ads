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
