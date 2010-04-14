------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . E X E C U T I O N _ U T I L S     --
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

with GNAT.Dynamic_Tables;

with Ocarina.Backends.Properties;

package Ocarina.Backends.Execution_Utils is

   use Ocarina.Backends.Properties;

   type Process_Rec is record
      Appli_Name         : Name_Id;
      --  The distributed application name

      Node_Name          : Name_Id;
      --  The node name (in lower case)

      Execution_Platform : Supported_Execution_Platform := Platform_None;
      --  The execution platform of the processor the current node
      --  is bound to.

   end record;
   --  This structure gathers all the information needed to
   --  invoke a compiled program from ocarina

   type Process_Type is access all Process_Rec;

   package Ref_Name_Tables is new GNAT.Dynamic_Tables (Process_Type, Nat,
                                                       1, 10, 10);
   --  Provides a flexible Makefile_Type list

   Process_List             : Ref_Name_Tables.Instance;
   --  List of all programs to invoke

   procedure Visit (E : Node_Id);
   procedure Init;
   procedure Reset;

   function Get_Binary_Location
     (Backend   : Backend_Kind;
      Node_Name : Name_Id)
     return String;

end Ocarina.Backends.Execution_Utils;
