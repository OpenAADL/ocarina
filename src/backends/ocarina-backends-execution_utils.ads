------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . E X E C U T I O N _ U T I L S      --
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

with GNAT.Dynamic_Tables;

with Ocarina.Backends.Properties;

package Ocarina.Backends.Execution_Utils is

   use Ocarina.Backends.Properties;

   type Process_Rec is record
      Appli_Name : Name_Id;
      --  The distributed application name

      Node_Name : Name_Id;
      --  The node name (in lower case)

      Execution_Platform : Supported_Execution_Platform := Platform_None;
      --  The execution platform of the processor the current node
      --  is bound to.

   end record;
   --  This structure gathers all the information needed to
   --  invoke a compiled program from ocarina

   type Process_Type is access all Process_Rec;

   package Ref_Name_Tables is new GNAT.Dynamic_Tables
     (Process_Type,
      Nat,
      1,
      10,
      10);
   --  Provides a flexible Makefile_Type list

   Process_List : Ref_Name_Tables.Instance;
   --  List of all programs to invoke

   procedure Visit (E : Node_Id);
   procedure Init;
   procedure Reset;

   function Get_Binary_Location
     (Backend   : Backend_Kind;
      Node_Name : Name_Id) return String;

end Ocarina.Backends.Execution_Utils;
