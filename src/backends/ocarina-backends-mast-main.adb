------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . M A S T . M A I N            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2010, European Space Agency (ESA).              --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.MAST_Tree.Nodes;
with Ocarina.Backends.MAST_Tree.Nutils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;

package body Ocarina.Backends.MAST.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.MAST_Tree.Nutils;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package MTN renames Ocarina.Backends.MAST_Tree.Nodes;
   package MTU renames Ocarina.Backends.MAST_Tree.Nutils;

   procedure Visit_Component (E : Node_Id);
   procedure Visit_System (E : Node_Id);
   procedure Visit_Processor (E : Node_Id);
   procedure Visit_Process (E : Node_Id);
   procedure Visit_Thread (E : Node_Id);
   procedure Visit_Bus (E : Node_Id);
   procedure Visit_Virtual_Processor (E : Node_Id);

   Root_System_Node                 : Node_Id := No_Node;

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
      Category : constant Component_Category
        := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System (E);

         when CC_Processor =>
            Visit_Processor (E);

         when CC_Process =>
            Visit_Process (E);

         when CC_Thread =>
            Visit_Thread (E);

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
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Virtual_Processor;

   ---------------------
   -- Visit_Processor --
   ---------------------

   procedure Visit_Processor (E : Node_Id) is
      S        : Node_Id;
      N        : Node_Id;
   begin
      N := MTU.Make_Processing_Resource
         (Normalize_Name (Name (Identifier (E))),
         PR_Regular_Processor);

      MTU.Append_Node_To_List (N, MTN.Declarations (MAST_File));

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

   -------------------
   -- Visit_Process --
   -------------------

   procedure Visit_Process (E : Node_Id) is
      S        : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Process;

   ------------------
   -- Visit_Thread --
   ------------------

   procedure Visit_Thread (E : Node_Id) is
      S        : Node_Id;
      N        : Node_Id;
   begin
      N := Make_Scheduling_Server
         (Normalize_Name (Name (Identifier (E))),
          Normalize_Name (Name (Identifier (Get_Bound_Processor
          (Parent_Component (Parent_Subcomponent (E)))))));
      Append_Node_To_List (N, MTN.Declarations (MAST_File));

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Thread;

   ------------------
   -- Visit_System --
   ------------------

   procedure Visit_System (E : Node_Id) is
      S                    : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_System;
end Ocarina.Backends.MAST.Main;
