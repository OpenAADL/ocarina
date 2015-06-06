------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.INSTANCES.COMPONENTS.SUBCOMPONENTS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

   use Ocarina.Instances.Components;
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
