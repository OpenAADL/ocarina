------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 O C A R I N A . B A C K E N D S . L N T                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

--  This package generates a pn tree

with Ocarina.Types;
with Ocarina.Backends.Properties;

package Ocarina.Backends.LNT is

   procedure Generate (AADL_Root : Types.Node_Id);

   procedure Init;
   --  Initializes the LNT module

   procedure Reset;
   --  Resets the LNT module

   type Thread is record
      Identifier : Name_Id;
      Period : Natural := 0;
      Capacity : Natural := 0;
      Event_Port_Number : Natural := 0;
      Dispatch_Protocol : Properties.Supported_Thread_Dispatch_Protocol;
   end record;

   type Thread_Array is array (Natural range <>) of Thread;
   type Period_Array is array (Natural range <>) of Natural;

   System_Name   : Name_Id;
   The_Processor : Node_Id;
   Thread_Number : Natural := 0;
   Not_Periodic_Thread_Number : Natural := 0;
   Hyperperiod   : Integer := 0;
   LNT_Thread_Instance_List   : List_Id := No_List;
   LNT_Processor_Gate_Declaration_List   : List_Id := No_List;
   LNT_States_List   : List_Id := No_List;
private
   Separator : Types.Name_Id;
   LNT_Threads  : Node_Id;
   LNT_Types : Node_Id;
   LNT_Processor : Node_Id;
   LNT_Port : Node_Id;
   LNT_Main : Node_Id;
   SVL      : Node_Id;
end Ocarina.Backends.LNT;
