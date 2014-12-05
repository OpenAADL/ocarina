------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . P O K _ C                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

package Ocarina.Backends.POK_C is

   type POK_Flavor_Type is (POK, ARINC653, DEOS);

   POK_Flavor : POK_Flavor_Type := POK;

   Add_Assertions : Boolean := True;

   procedure Generate (AADL_Root : Node_Id);
   --  The main entry point of the POK Code generator

   procedure Init;
   --  Fills the corresponding location in the generator table by the
   --  information on this generator and execute some initialization
   --  routines necessary for its work.

   procedure Reset;
   --  Reset the internal units of the POK generator

   procedure Display_Communication_Error;
   --  Display a message when a modeling pattern is not correct
   --  about ports.

   function Use_ARINC653_API return Boolean;
   --  Indicate if the Flavor uses the ARINC653 API.

private
   C_Root : Node_Id;
   --  The root of the C trees

end Ocarina.Backends.POK_C;
