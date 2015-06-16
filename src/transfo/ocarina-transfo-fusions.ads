------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              O C A R I N A . T R A N S F O . F U S I O N S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

--  Perform thread merging

package Ocarina.Transfo.Fusions is

   procedure Init;
   --  Fix the name allocation for features, components and subprograms
   --  Sporadic threads

   procedure Reset;
   --  To be called between each execution of fusion_threads

   procedure Fusion_Threads
     (Root                              :     Node_Id;
      Owner_Process, Thread_1, Thread_2 :     Name_Id;
      New_Thread                        : out Node_Id;
      Success                           : out Boolean);
   --  Perfom the fusion of the two threads passed as arguments

   Transfo_Original_Name   : Name_Id;
   Raw_Original_Name       : Name_Id;
   Transfo_Scheduler_Name  : Name_Id;
   Raw_Scheduler_Name      : Name_Id;
   Dispatch_Protocol       : Name_Id;
   Deadline                : Name_Id;
   WCET                    : Name_Id;
   Deployment_Priority     : Name_Id;
   Cheddar_Priority        : Name_Id;
   Transfo_Priority        : Name_Id;
   Transfo_Occurred        : Name_Id;
   Scheduler_Call          : Name_Id;
   Iterator_Call           : Name_Id;
   Compute_Entrypoint      : Name_Id;
   CS_Period               : Name_Id;
   Period                  : Name_Id;
   Priority_Shifter        : Name_Id;
   Raw_Priority_Shifter    : Name_Id;
   Cheddar_Pkg             : Name_Id;
   Transfo_Pkg             : Name_Id;
   Raw_Priority            : Name_Id;
   Raw_Fixed_Priority      : Name_Id;
   Raw_Occurred            : Name_Id;
   Raw_Data_Representation : Name_Id;
   Data_Representation     : Name_Id;
   Data_Model_Pkg          : Name_Id;
   --  name of corresponding AADL properties

   function Is_Defined_Property
     (Component : Node_Id;
      Property  : Name_Id) return Boolean;
   --  Search a property in component implementation and declaration

   function Get_Integer_Property
     (Component : Node_Id;
      Property  : Name_Id) return Unsigned_Long_Long;
   --  Search a property in component implementation and declaration

   function Get_Boolean_Property
     (Component : Node_Id;
      Property  : Name_Id) return Boolean;
   --  Search a property in component implementation and declaration

private

   type Node_Only is record
      List_Node : List_Id;
   end record;

   type Node_Association is record
      Old_Node : Node_Id;
      New_Node : Node_Id;
   end record;

   type Node_Counter is record
      Node : Node_Id;
      Cnt  : Int;
   end record;

   type Node_Priority is record
      Node   : Node_Id;
      Cnt    : Int;
      Object : Node_Id;
   end record;

end Ocarina.Transfo.Fusions;
