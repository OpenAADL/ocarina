------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . E X E C U T I O N _ U T I L S      --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Namet;

with GNAT.Directory_Operations;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nodes;

with Ocarina.Backends;
with Ocarina.Backends.Utils;

package body Ocarina.Backends.Execution_Utils is

   use Ocarina.Namet;
   use GNAT.Directory_Operations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends;
   use Ocarina.Backends.Utils;

   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Ref_Name_Tables.Free (Process_List);
   end Reset;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Ref_Name_Tables.Init (Process_List);
   end Init;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Visit_Architecture_Instance (E);

         when K_Component_Instance =>
            Visit_Component_Instance (E);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      Visit (Root_System (E));
   end Visit_Architecture_Instance;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------

   procedure Visit_Component_Instance (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Process =>
            Visit_Process_Instance (E);

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process_Instance (E : Node_Id) is
      S : constant Node_Id      := Parent_Subcomponent (E);
      A : constant Node_Id      := Parent_Component (Parent_Subcomponent (E));
      M : constant Process_Type := new Process_Rec;
   begin
      if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
         M.Appli_Name := Normalize_Name (Name (Identifier (A)));
         M.Node_Name  := Normalize_Name (Name (Identifier (S)));

         --  Get the execution platform of the processor this node is
         --  bound to.
         M.Execution_Platform :=
           Get_Execution_Platform (Get_Bound_Processor (E));

         Ref_Name_Tables.Append (Process_List, M);
      end if;
   end Visit_Process_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      S : constant Node_Id      := Parent_Subcomponent (E);
      M : constant Process_Type := new Process_Rec;
   begin
      if Get_Current_Backend_Kind = PolyORB_Kernel_C then
         Set_Str_To_Name_Buffer ("generated-code/");
         Get_Name_String_And_Append (Name (Identifier (S)));
         M.Appli_Name := Name_Find;
         Set_Str_To_Name_Buffer ("pok.elf");
         M.Node_Name := Name_Find;

         Ref_Name_Tables.Append (Process_List, M);
      end if;
   end Visit_Processor_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      --  Visit all the subcomponents of the system
      if not AAU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_System_Instance;

   -------------------------
   -- Get_Binary_Location --
   -------------------------

   function Get_Binary_Location
     (Backend   : Backend_Kind;
      Node_Name : Name_Id) return String
   is
   begin
      Set_Str_To_Name_Buffer ("");

      case Backend is
         when PolyORB_HI_Ada | PolyORB_HI_C =>
            Get_Name_String_And_Append (Node_Name);
            Add_Str_To_Name_Buffer (Dir_Separator & "");
            Get_Name_String_And_Append (Node_Name);
         when PolyORB_Kernel_C =>
            Get_Name_String_And_Append (Node_Name);
         when others =>
            null;
      end case;

      return Get_Name_String (Name_Find);
   end Get_Binary_Location;

end Ocarina.Backends.Execution_Utils;
