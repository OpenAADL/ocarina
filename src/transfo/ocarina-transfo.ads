------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . T R A N S F O                       --
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

with Ocarina.Types; use Ocarina.Types;

package Ocarina.Transfo is

   procedure Init;

   function Build_Unique_Name
     (List   : List_Id;
      Prefix : Name_Id;
      Shift  : Natural := 0) return Name_Id;
   --  Return <prefix>_<id> so that the name is unique
   --  in the identifier list, adding shift to the result

   function Build_Unique_Component_Name
     (Pkg    : Node_Id;
      Prefix : Name_Id) return Name_Id;
   --  Create a new component type unique name
   --  following the pattern <prefix>_<Id>, with <prefix> is
   --  typically the component category (thread, data,
   --  subprogram...) and <id> a unique integer for all
   --  components in the system sharing the same prefix

   function Build_Unique_Subcomponent_Name
     (Container : Node_Id;
      Prefix    : Name_Id) return Name_Id;
   --  Create a new subcomponent in container unique name
   --  following the pattern <prefix>_<Id>, with <prefix>
   --  typically depends on the component category (thread,
   --  data, subprogram...) and <id> a unique integer for
   --  all subcomponents in the container sharing the same
   --  prefix

   function Concat_Names (N1, N2 : Node_Id) return Name_Id;
   --  Build a name_id which is teh concatenatino of the
   --  parameters' names.
   --  The two parameters must be components declaration or
   --  implementation.

   function Search_Process_By_Name (Process_Name : String) return Node_Id;
   --  Find a process from its name
   --  return No_Node if no such process was found

   function Search_Thread_By_Name
     (Process     : Node_Id;
      Thread_Name : String) return Node_Id;
   --  Find a thread from its parent process and its name
   --  return No_Node if no such thread was found

   function Build_Name_From_Path (Path : List_Id) return Name_Id;
   --  Compute the name corresponding to a path

   Thread_Prefix     : Name_Id;
   Process_Prefix    : Name_Id;
   Subprogram_Prefix : Name_Id;
   Data_Prefix       : Name_Id;
   Connection_Prefix : Name_Id;
   Feature_Prefix    : Name_Id;
   Wrapper_Prefix    : Name_Id;
end Ocarina.Transfo;
