------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                              O C A R I N A                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2004-2009, GET-Telecom Paris.                --
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

package Ocarina is

   type AADL_Version_Type is (AADL_V1, AADL_V2);
   --  Define AADL Version 1.0 and AADL Version 2.0

   Default_AADL_Version : AADL_Version_Type;
   --  Default AADL version initialised by main with Get_Default_AADL_Version.

   AADL_Version : AADL_Version_Type;

   procedure Initialize;
   --  Initialize the Ocarina core

   procedure Reset;
   --  Reset the node tree. All the information related to the node
   --  will be lost.

   function GNU_Make_Cmd return String;
   --  Return the command used to invoke the GNU make on the current
   --  platform. Fetched at configure time.

private

   function Default_GNU_Make return String;
   --  Return the default executable name for GNU make

   type GNU_Make_Access is access function return String;
   GNU_Make_Ptr : GNU_Make_Access := Default_GNU_Make'Access;

end Ocarina;
