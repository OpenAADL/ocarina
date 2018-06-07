------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . L N T . T R E E _ G E N E R A T O R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2016-2018 ESA & ISAE.                    --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.Properties;

use Ocarina.Backends.Properties;
use Ocarina.ME_AADL;
use Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Debug;
use Ocarina.ME_AADL.AADL_Instances.Debug;

package body Ocarina.Backends.LNT.Tree_Generator is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINu renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   use AIN;

   procedure Get_N_Thread (Sys : Node_Id;
             Thread_Number : in out Natural;
             Not_Periodic_Thread_Number : in out Natural) is
      Sys_N : Node_Id;
      Proc : Node_Id;
      Proc_N : Node_Id;
      Thr : Node_Id;
   begin
      if Get_Category_Of_Component (Sys) = CC_System then
         if not AINU.Is_Empty (Subcomponents (Sys)) then
            Sys_N := AIN.First_Node (Subcomponents (Sys));
            while Present (Sys_N) loop
               Proc := Corresponding_Instance (Sys_N);
               if Get_Category_Of_Component (Proc) = CC_System then
                  Get_N_Thread (Proc,
                                Thread_Number,
                                Not_Periodic_Thread_Number);
               end if;
               if Get_Category_Of_Component (Proc) = CC_Process then
                  if not AINU.Is_Empty (Subcomponents (Proc)) then
                     Proc_N := AIN.First_Node (Subcomponents (Proc));
                     while Present (Proc_N) loop
                        Thr := Corresponding_Instance (Proc_N);
                        if Get_Category_Of_Component (Thr) = CC_Thread then
                           Thread_Number := Thread_Number + 1;
                           if (Get_Thread_Dispatch_Protocol (Thr)
                              /= Thread_Periodic)
                           then
                              Not_Periodic_Thread_Number :=
                                Not_Periodic_Thread_Number + 1;
                           end if;
                        end if;
                        Proc_N := AIN.Next_Node (Proc_N);
                     end loop;
                  end if;
               end if;
               Sys_N := AIN.Next_Node (Sys_N);
            end loop;
         end if;
      end if;
   end Get_N_Thread;

end Ocarina.Backends.LNT.Tree_Generator;
