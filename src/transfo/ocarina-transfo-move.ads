------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 O C A R I N A . T R A N S F O . M O V E                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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
     (System_Inst  : Node_Id;
      Process_Name : Name_Id) return Boolean;
   --  Remove features which are not connected to any
   --  component. return true if any modification is done to the model

private
   type Node_Value is record
      Node : Node_Id;
   end record;

end Ocarina.Transfo.Move;
