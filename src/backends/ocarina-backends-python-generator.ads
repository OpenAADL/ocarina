------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P Y T H O N . G E N E R A T O R     --
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

pragma Warnings (Off);

with Fifo;

package Ocarina.Backends.Python.Generator is

   type Port is record
      name      : Name_Id;
      port_type : Name_Id;
      c_type    : Name_Id;
      direction : Name_Id;
   end record;

   package Port_List is new Fifo (Element_Type => Port);

   type Funct is record
      source : Name_Id;
      name   : Name_Id;
      ports  : Port_List.Fifo_Type;
   end record;

   type Call_Funct is record
      has_before      : Boolean := False;
      has_loop        : Boolean := False;
      has_after       : Boolean := False;
      Before_Function : Funct;
      Loop_Function   : Funct;
      After_Function  : Funct;
   end record;

   type Binding is record
      first_name        : Name_Id;
      instance_name     : Name_Id;
      Simulation_Model  : Name_Id;
      second_name       : Name_Id;
      component_type    : Name_Id;
      priority          : Name_Id;
      period            : Name_Id;
      scheduling_policy : Name_Id;
      dispatch_protocol : Name_Id;
      function_calls    : Call_Funct;
      ports             : Port_List.Fifo_Type;
   end record;

   package Binding_List is new Fifo (Element_Type => Binding);

   type ss_project is record
      project_name   : Name_Id;
      Module_List    : Binding_List.Fifo_Type;
      Device_List    : Binding_List.Fifo_Type;
      Processor_List : Binding_List.Fifo_Type;
      Bus_List       : Binding_List.Fifo_Type;
      Memory_List    : Binding_List.Fifo_Type;
   end record;

   Var_Name_Len    : Natural := 0;
   project         : ss_project;
   Print_On_Stdout : Boolean := False;

   procedure Generate (N : Node_Id);
   --  All code generation is performed in the current directory. It
   --  is up to the caller to change the working directory before
   --  calling Generate.

end Ocarina.Backends.Python.Generator;
