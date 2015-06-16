------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . C A R T S . M A I N           --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Carts.Mapping;
with Ocarina.Backends.Properties;

package body Ocarina.Backends.Carts.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Carts.Mapping;
   use Ocarina.Backends.Properties;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   procedure Visit_Component (E : Node_Id);
   procedure Visit_System (E : Node_Id);
   procedure Visit_Processor (E : Node_Id);
   procedure Visit_Bus (E : Node_Id);
   procedure Look_For_Threads (E : Node_Id);
   procedure Visit_Virtual_Processor (E : Node_Id);

   Root_System_Node               : Node_Id := No_Node;
   Current_Processor_Node         : Node_Id;
   Current_System_Node            : Node_Id;
   Current_Virtual_Processor_Node : Node_Id;
   Current_AADL_Virtual_Processor : Node_Id;

   ----------------------
   -- Look_For_Threads --
   ----------------------

   procedure Look_For_Threads (E : Node_Id) is
      S               : Node_Id;
      Bound_Processor : Node_Id;
   begin
      if AINU.Is_Thread (E) then
         Bound_Processor :=
           Get_Bound_Processor (Parent_Component (Parent_Subcomponent (E)));

         if Bound_Processor /= No_Node
           and then Bound_Processor = Current_AADL_Virtual_Processor
         then
            Append_Node_To_List
              (Map_Thread (E),
               XTN.Subitems (Current_Virtual_Processor_Node));
         end if;
      else
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.
               Look_For_Threads (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;
      end if;
   end Look_For_Threads;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Root_System_Node := Root_System (E);
            Visit (Root_System_Node);

         when K_Component_Instance =>
            Visit_Component (E);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------
   -- Visit_Component --
   ---------------------

   procedure Visit_Component (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System (E);

         when CC_Processor =>
            Visit_Processor (E);

         when CC_Bus =>
            Visit_Bus (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor (E);

         when others =>
            null;
      end case;
   end Visit_Component;

   ---------------
   -- Visit_Bus --
   ---------------

   procedure Visit_Bus (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Bus;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor (E : Node_Id) is
   begin
      Current_AADL_Virtual_Processor := E;
      Current_Virtual_Processor_Node := Map_Virtual_Processor (E);
      XTU.Append_Node_To_List
        (Current_Virtual_Processor_Node,
         XTN.Subitems (Current_Processor_Node));

      Look_For_Threads (Root_System_Node);
   end Visit_Virtual_Processor;

   ---------------------
   -- Visit_Processor --
   ---------------------

   procedure Visit_Processor (E : Node_Id) is
      S : Node_Id;
   begin
      Current_Processor_Node := Map_Processor (E);
      Append_Node_To_List
        (Current_Processor_Node,
         XTN.Subitems (Current_System_Node));

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Processor;

   ------------------
   -- Visit_System --
   ------------------

   procedure Visit_System (E : Node_Id) is
      S : Node_Id;
      P : Node_Id;
      U : Node_Id;
   begin
      P := Map_HI_Node (E);
      Push_Entity (P);

      U := Map_HI_Unit (E);
      Push_Entity (U);

      Current_System_Node := XTN.Root_Node (XTN.XML_File (U));

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      Pop_Entity;
      Pop_Entity; --  A
   end Visit_System;
end Ocarina.Backends.Carts.Main;
