------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            OCARINA.BACKENDS.XTRATUM_CONF.SYSTEM_DESCRIPTION              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2011-2012 ESA & ISAE.                    --
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
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Locations;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.XML_Values;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Xtratum_Conf.Mapping;

package body Ocarina.Backends.Xtratum_Conf.System_Description is

   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Xtratum_Conf.Mapping;

   package AINU   renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIN    renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package XTN    renames Ocarina.Backends.XML_Tree.Nodes;
   package XV     renames Ocarina.Backends.XML_Values;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Bus_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);

   Partition_Identifier : Unsigned_Long_Long := 0;

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
      Category : constant Component_Category
        := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Process =>
            Visit_Process_Instance (E);

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when CC_Bus =>
            Visit_Bus_Instance (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process_Instance (E : Node_Id) is
      S : Node_Id;
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
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S     : Node_Id;
      P     : Node_Id;
      U     : Node_Id;
      N     : Node_Id;
      Processes : List_Id;
   begin
      P := Map_HI_Node (E);
      Push_Entity (P);

      U := Map_HI_Unit (E);
      Push_Entity (U);

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
         --  Visit the component instance corresponding to the
         --  subcomponent S.
            if AINU.Is_Processor (Corresponding_Instance (S)) then
               Visit (Corresponding_Instance (S));
            end if;
            S := Next_Node (S);
         end loop;
      end if;
      N := New_Node (XTN.K_HI_Tree_Bindings);

      Processes := AINU.New_List (K_Node_Id, No_Location);

      XTN.Set_Processes (N, Processes);

      XTN.Set_Unit (N, U);
      XTN.Set_Node (N, P);

      AIN.Set_Backend_Node (Identifier (E), N);

      Pop_Entity;
      Pop_Entity;
   end Visit_System_Instance;

   ------------------------
   -- Visit_Bus_Instance --
   ------------------------

   procedure Visit_Bus_Instance (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Bus_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------
   procedure Visit_Processor_Instance (E : Node_Id) is
      S        : Node_Id;
      N         : Node_Id;
      Processes : List_Id;
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

      N := New_Node (XTN.K_HI_Tree_Bindings);

      Processes := AINU.New_List (K_Node_Id, No_Location);

      XTN.Set_Processes (N, Processes);

      AIN.Set_Backend_Node (Identifier (E), N);
   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      S                          : Node_Id;
      Partition_Identifier_Node  : Node_Id;
   begin
      Partition_Identifier_Node :=
         Make_Literal (XV.New_Numeric_Value
               (Partition_Identifier, 1, 10));

      AIN.Set_Backend_Node
         (Identifier (E), Partition_Identifier_Node);

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
         --  Visit the component instance corresponding to the
         --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      Partition_Identifier := Partition_Identifier + 1;

   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Xtratum_Conf.System_Description;
