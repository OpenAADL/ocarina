------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . A R I N C 6 5 3 _ C O N F        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

package Ocarina.Backends.ARINC653_Conf is

--  This backend generates the XML file useful for ARINC653 implementation

   procedure Generate (AADL_Root : Node_Id);
   --  The main entry point of the ARINC653 configuration generator

   procedure Init;
   --  Fills the corresponding location in the generator table by the
   --  information on this generator and execute some initialization
   --  routines necessary for its work.

   procedure Reset;

   procedure Visit_Architecture_Instance (E : Node_Id);

   function Get_XML_Root return Node_Id;

private
   XML_Root                : Node_Id;
   Current_XML_Node        : Node_Id;
   Distributed_Application : Node_Id;
   HI_Node                 : Node_Id;
   HI_Unit                 : Node_Id;
   --  The root of the XML trees

end Ocarina.Backends.ARINC653_Conf;
