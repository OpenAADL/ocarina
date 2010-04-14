------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 O C A R I N A . T R A N S F O . M O V E                  --
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

--  Perform thread move from a process to another

package Ocarina.Transfo.Move is

   procedure Init;

   procedure Reset (AADL_Root : Node_Id);

   procedure Move_Thread
     (Thread_Name      : Name_Id;
      Old_Process_Name : Name_Id;
      New_Process_Name : Name_Id);
   --  Move <thread_name> from <old_process_name>
   --  to  <new_process_name>

   function Clean_Obsolete_Features
     (System_Inst : Node_Id; Process_Name : Name_Id) return Boolean;
   --  Remove features which are not connected to any
   --  component. return true if any modification is done to the model

private
   type Node_Value is record
      Node : Node_Id;
   end record;

end Ocarina.Transfo.Move;
