------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.INSTANCES.COMPONENTS.SUBCOMPONENTS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Locations;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Instances.Components;
with Ocarina.Instances.Messages;

package body Ocarina.Instances.Components.Subcomponents is

   use Locations;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   use Ocarina.Instances.Messages;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;

   ------------------------------
   -- Instantiate_Subcomponent --
   ------------------------------

   function Instantiate_Subcomponent
     (Instance_Root : Node_Id;
      Subcomponent  : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Kind (Subcomponent) = K_Subcomponent);

      New_Instance    : Node_Id := No_Node;
      New_Subinstance : Node_Id;
   begin
      New_Instance :=
        New_Node (K_Subcomponent_Instance, ATN.Loc (Subcomponent));
      AIN.Set_Identifier
        (New_Instance,
         Duplicate_Identifier (ATN.Identifier (Subcomponent)));
      AIN.Set_Corresponding_Declaration (New_Instance, Subcomponent);
      Set_Destinations (New_Instance, New_List (K_List_Id, No_Location));

      if Present (Entity_Ref (Subcomponent))
        and then Present
          (ATE.Get_Referenced_Entity (Entity_Ref (Subcomponent)))
      then
         --  Verify whether the component has been instantiateed or not

         New_Subinstance :=
           Instantiate_Component
             (Instance_Root,
              ATE.Get_Referenced_Entity (Entity_Ref (Subcomponent)));

         if Present (New_Subinstance) then
            Set_Corresponding_Instance (New_Instance, New_Subinstance);
            Set_Parent_Subcomponent (New_Subinstance, New_Instance);
         end if;

         if ATN.Kind (ATE.Get_Referenced_Entity (Entity_Ref (Subcomponent))) =
           K_Component_Type
         then
            Display_Entity_Is_A_Component_Type (Subcomponent);
         end if;
      else
         Display_No_Entity_Ref (Subcomponent);
      end if;

      return New_Instance;
   end Instantiate_Subcomponent;

end Ocarina.Instances.Components.Subcomponents;
