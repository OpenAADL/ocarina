------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.INSTANCES.COMPONENTS.CONNECTIONS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Instances.Properties;

package body Ocarina.Instances.Components.Connections is

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   use Ocarina.Instances.Properties;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   function Find_Connection_End
     (Component_Instance : Node_Id;
      Connection_End     : Node_Id) return Node_Id;
   --  Find the entity (feature, subcomponent, etc.) instance pointed
   --  by the connection end.

   -------------------------
   -- Find_Connection_End --
   -------------------------

   function Find_Connection_End
     (Component_Instance : Node_Id;
      Connection_End     : Node_Id) return Node_Id
   is
      pragma Assert
        (AIN.Kind (Component_Instance) = K_Component_Instance
         and then ATN.Kind (Connection_End) = K_Entity_Reference);

      Entity      : Node_Id;
      Entity_Path : Node_Id;
   begin
      if No (ATN.First_Node (ATN.Path (Connection_End))) then
         Entity_Path := No_Node;
      else
         Entity_Path :=
           New_Node (K_Entity_Reference_Instance, ATN.Loc (Connection_End));

         case ATN.Kind
           (ATN.Corresponding_Entity
              (ATN.Item (ATN.First_Node (ATN.Path (Connection_End)))))
         is
            when K_Port_Spec        |
              K_Subcomponent_Access |
              K_Feature_Group_Spec  |
              K_Parameter           |
              K_Subprogram_Spec     =>
               Entity :=
                 Get_First_Homonym_Instance
                   (AIN.Features (Component_Instance),
                    ATN.Name
                      (ATN.Item (ATN.First_Node (ATN.Path (Connection_End)))));

            when K_Subcomponent =>
               Entity :=
                 Get_First_Homonym_Instance
                   (AIN.Subcomponents (Component_Instance),
                    ATN.Name
                      (ATN.Item (ATN.First_Node (ATN.Path (Connection_End)))));

            when K_Subprogram_Call =>
               if not Is_Empty (AIN.Calls (Component_Instance)) then
                  declare
                     List_Node : Node_Id :=
                       AIN.First_Node (AIN.Calls (Component_Instance));
                  begin
                     while Present (List_Node) loop
                        Entity :=
                          Get_First_Homonym_Instance
                            (AIN.Subprogram_Calls (List_Node),
                             ATN.Name
                               (ATN.Item
                                  (ATN.First_Node
                                     (ATN.Path (Connection_End)))));
                        exit when Present (Entity);
                        List_Node := AIN.Next_Node (List_Node);
                     end loop;
                  end;
               else
                  Entity := No_Node;
               end if;

            when others =>
               raise Program_Error;
         end case;

         if Present (Entity) then
            --  Add the pointed entity into the entity instance path

            Add_Path_Element_To_Entity_Reference (Entity_Path, Entity);

            if Present
                (ATN.Next_Node (ATN.First_Node (ATN.Path (Connection_End))))
            then
               case ATN.Kind
                 (ATN.Corresponding_Entity
                    (ATN.Item (ATN.First_Node (ATN.Path (Connection_End)))))
               is
                  when K_Subcomponent_Access =>
                     Entity := No_Node;
                  --  XXX we have to find the feature instance of the
                  --  corresponding subcomponent instance.

                  when K_Feature_Group_Spec =>
                     Entity :=
                       Get_First_Homonym_Instance
                         (AIN.Features (Entity),
                          ATN.Name
                            (ATN.Item
                               (ATN.Next_Node
                                  (ATN.First_Node
                                     (ATN.Path (Connection_End))))));

                  when K_Subcomponent | K_Subprogram_Call =>
                     Entity :=
                       Get_First_Homonym_Instance
                         (AIN.Features (Corresponding_Instance (Entity)),
                          ATN.Name
                            (ATN.Item
                               (ATN.Next_Node
                                  (ATN.First_Node
                                     (ATN.Path (Connection_End))))));

                  when others =>
                     raise Program_Error;
               end case;

               --  Add the pointed entity into the entity instance path

               Add_Path_Element_To_Entity_Reference (Entity_Path, Entity);
            end if;
         end if;
      end if;

      return Entity_Path;
   end Find_Connection_End;

   ----------------------------
   -- Instantiate_Connection --
   ----------------------------

   function Instantiate_Connection
     (Instance_Root      : Node_Id;
      Component_Instance : Node_Id;
      Connection         : Node_Id) return Node_Id
   is
      pragma Assert
        (AIN.Kind (Instance_Root) = K_Architecture_Instance
         and then ATN.Kind (Connection) = K_Connection);

      New_Instance : constant Node_Id :=
        New_Node (K_Connection_Instance, ATN.Loc (Connection));
      Connection_Src : Node_Id;
      Connection_Dst : Node_Id;
      Success        : Boolean;
   begin
      Set_Corresponding_Declaration (New_Instance, Connection);
      Set_Parent_Component (New_Instance, Component_Instance);
      AIN.Set_Identifier
        (New_Instance,
         Duplicate_Identifier (ATN.Identifier (Connection)));

      if AIN.Identifier (New_Instance) /= No_Node then
         AIN.Set_Corresponding_Entity
           (AIN.Identifier (New_Instance),
            New_Instance);
      end if;

      --  Set the connection ends and link them to the the connection
      --  instance.

      Connection_Src :=
        Find_Connection_End (Component_Instance, ATN.Source (Connection));
      AIN.Set_Source (New_Instance, Connection_Src);

      Connection_Dst :=
        Find_Connection_End (Component_Instance, ATN.Destination (Connection));
      AIN.Set_Destination (New_Instance, Connection_Dst);

      --  Make the connection ends aware of themselves and of the
      --  connection in which they are involved.

      --  FIXME XXX : Update Connection_Dst instead of if then else case
      Append_Node_To_List
        (Make_Node_Container
           (AIN.Item (AIN.Last_Node (AIN.Path (Connection_Dst))),
            New_Instance),
         Destinations (AIN.Item (AIN.Last_Node (AIN.Path (Connection_Src)))));

      Append_Node_To_List
        (Make_Node_Container
           (AIN.Item (AIN.Last_Node (AIN.Path (Connection_Src))),
            New_Instance),
         Sources (AIN.Item (AIN.Last_Node (AIN.Path (Connection_Dst)))));

      --  Set the connection type

      Set_Associated_Type (New_Instance, No_Node);

      --  Apply the properties of the connection to the connection
      --  instance.

      Success :=
        Apply_Properties
          (Instance_Root,
           New_Instance,
           ATN.Properties (Connection),
           Override_Mode => True);

      if not Success then
         return No_Node;
      end if;

      return New_Instance;
   end Instantiate_Connection;

end Ocarina.Instances.Components.Connections;
