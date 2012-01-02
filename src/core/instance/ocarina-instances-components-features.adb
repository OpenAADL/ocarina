------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.INSTANCES.COMPONENTS.FEATURES                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2005-2012, European Space Agency (ESA).           --
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

with Locations;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

with Ocarina.Annotations;
with Ocarina.Instances.Messages;
with Ocarina.Instances.Namespaces;

package body Ocarina.Instances.Components.Features is

   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.Annotations;
   use Ocarina.Instances.Messages;
   use Ocarina.Instances.Namespaces;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;

   function Instantiate_Corresponding_Entity
     (Instance_Root : Node_Id;
      Feature       : Node_Id;
      Container     : Node_Id)
     return Node_Id;

   function Instantiate_Port_Group_Type
     (Instance_Root : Node_Id;
      Port_Group    : Node_Id;
      Feature_List  : List_Id;
      Container     : Node_Id;
      Inverse       : Boolean := False)
     return Boolean;
   --  Recursively instantiate a port group type, with extensions, features
   --  and inverses. Return True if everything was OK, else False

   -------------------------
   -- Instantiate_Feature --
   -------------------------

   function Instantiate_Feature
     (Instance_Root : Node_Id;
      Feature       : Node_Id;
      Inverse       : Boolean := False)
     return Node_Id
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Kind (Feature) = K_Subcomponent_Access
                     or else Kind (Feature) = K_Feature_Group_Spec
                     or else Kind (Feature) = K_Subprogram_Spec
                     or else Kind (Feature) = K_Parameter
                     or else Kind (Feature) = K_Port_Spec);

      New_Instance    : Node_Id := No_Node;
      New_Subinstance : Node_Id := No_Node;
      Container       : constant Node_Id := Container_Component (Feature);
      Port_Group      : Node_Id := No_Node;
      Success         : Boolean := True;
   begin
      case ATN.Kind (Feature) is
         when K_Port_Spec =>
            New_Instance := Ocarina.ME_AADL.AADL_Instances.Nutils.New_Node
              (K_Port_Spec_Instance, ATN.Loc (Feature));
            AIN.Set_Is_In (New_Instance, ATN.Is_In (Feature));
            AIN.Set_Is_Out (New_Instance, ATN.Is_Out (Feature));
            AIN.Set_Is_Event (New_Instance, ATN.Is_Event (Feature));
            AIN.Set_Is_Data (New_Instance, ATN.Is_Data (Feature));
            AIN.Set_Identifier (New_Instance,
                            Duplicate_Identifier (ATN.Identifier (Feature)));
            AIN.Set_Sources (New_Instance, New_List (K_List_Id, No_Location));
            AIN.Set_Destinations (New_Instance,
                                  New_List (K_List_Id, No_Location));

            if ATN.Is_Data (Feature) then
               --  We instantiate the corresponding Data component for data
               --  and event data ports.

               New_Subinstance := Instantiate_Corresponding_Entity
                 (Instance_Root,
                  Feature,
                  Container);

               if Present (New_Subinstance) then
                  Set_Corresponding_Instance (New_Instance, New_Subinstance);
               else
                  Success := False;
               end if;
            end if;

         when K_Feature_Group_Spec =>
            if Entity_Ref (Feature) /= No_Node
              and then ATE.Get_Referenced_Entity
              (Entity_Ref (Feature)) /= No_Node
            then
               New_Instance := New_Node (K_Feature_Group_Spec_Instance,
                                         ATN.Loc (Feature));
               AIN.Set_Features (New_Instance,
                             New_List (K_List_Id, ATN.Loc (Feature)));
               AIN.Set_Identifier (New_Instance,
                              Duplicate_Identifier (ATN.Identifier (Feature)));
               Port_Group := ATE.Get_Referenced_Entity (Entity_Ref (Feature));
               AIN.Set_Sources
                  (New_Instance, New_List (K_List_Id, No_Location));
               AIN.Set_Destinations (New_Instance,
                                  New_List (K_List_Id, No_Location));

               Success := Instantiate_Port_Group_Type
                 (Instance_Root,
                  Port_Group,
                  Ocarina.ME_AADL.AADL_Instances.Nodes.Features (New_Instance),
                  Container,
                  Inverse);
            elsif Inverse_Of (Feature) /= No_Node
              and then ATE.Get_Referenced_Entity
              (Inverse_Of (Feature)) /= No_Node
            then
               New_Instance := New_Node (K_Feature_Group_Spec_Instance,
                                         ATN.Loc (Feature));
               AIN.Set_Features (New_Instance,
                             New_List (K_List_Id, ATN.Loc (Feature)));
               AIN.Set_Identifier (New_Instance,
                              Duplicate_Identifier (ATN.Identifier (Feature)));
               Port_Group := ATE.Get_Referenced_Entity (Inverse_Of (Feature));
               AIN.Set_Sources
                  (New_Instance, New_List (K_List_Id, No_Location));
               AIN.Set_Destinations (New_Instance,
                                  New_List (K_List_Id, No_Location));

               Success := Instantiate_Port_Group_Type
                 (Instance_Root,
                  Port_Group,
                  Ocarina.ME_AADL.AADL_Instances.Nodes.Features (New_Instance),
                  Container,
                  Inverse);
            else
               Display_No_Entity_Ref (Feature);
               Success := False;
            end if;

         when K_Parameter =>
            New_Instance := New_Node (K_Parameter_Instance, ATN.Loc (Feature));
            AIN.Set_Is_In (New_Instance, ATN.Is_In (Feature));
            AIN.Set_Is_Out (New_Instance, ATN.Is_Out (Feature));
            AIN.Set_Identifier (New_Instance,
                            Duplicate_Identifier (ATN.Identifier (Feature)));
            AIN.Set_Sources (New_Instance, New_List (K_List_Id, No_Location));
            AIN.Set_Destinations (New_Instance,
                                  New_List (K_List_Id, No_Location));

            New_Subinstance := Instantiate_Corresponding_Entity
              (Instance_Root, Feature, Container);

            if Present (New_Subinstance) then
               Set_Corresponding_Instance (New_Instance, New_Subinstance);
            else
               Success := False;
            end if;

         when K_Subprogram_Spec =>
            New_Instance := New_Node (K_Subprogram_Spec_Instance,
                                      ATN.Loc (Feature));
            AIN.Set_Is_Server (New_Instance, ATN.Is_Server (Feature));
            AIN.Set_Identifier (New_Instance,
                            Duplicate_Identifier (ATN.Identifier (Feature)));
            Set_Sources (New_Instance, New_List (K_List_Id, No_Location));
            Set_Destinations (New_Instance, New_List (K_List_Id, No_Location));

            --  Instantiate the corresponding subprogram component

            New_Subinstance := Instantiate_Corresponding_Entity
              (Instance_Root, Feature, Container);

            if Present (New_Subinstance) then
               Set_Corresponding_Instance (New_Instance, New_Subinstance);
            else
               Success := False;
            end if;

         when K_Subcomponent_Access =>
            New_Instance := New_Node (K_Subcomponent_Access_Instance,
                                      ATN.Loc (Feature));
            AIN.Set_Is_Provided (New_Instance, ATN.Is_Provided (Feature));
            AIN.Set_Is_Data (New_Instance,
                         Component_Category'Val
                         (Subcomponent_Category (Feature)) = CC_Data);

            AIN.Set_Identifier
              (New_Instance,
               Duplicate_Identifier (ATN.Identifier (Feature)));
            Set_Sources (New_Instance, New_List (K_List_Id, No_Location));
            Set_Destinations (New_Instance, New_List (K_List_Id, No_Location));

            --  Instantiate the corresponding component

            --  FIXME: This is definitely a WRONG design. We MUST:

            --  1 - NOT instantiate the component corresponding to an
            --  access.

            --  2 - POSTPONE the resolution of this at the connection
            --  instantiation.

            if Component_Category'Val
              (Subcomponent_Category (Feature)) = CC_Data
              and then Present (Entity_Ref (Feature))
              and then Present (Get_Instance (ATE.Get_Referenced_Entity
                                                (Entity_Ref (Feature))))
            then
               --  XXX If the component type denotes a data type, we
               --  recycle an existing instance. this is done to avoid
               --  infinite recursion, may lead to incorrect
               --  properties being set. We should definitely
               --  implement the recommandation above.  Yet, this
               --  removes a dirtier hack in Instantiate_Component

               Set_Corresponding_Instance
                 (New_Instance,
                  Get_Instance (ATE.Get_Referenced_Entity
                                  (Entity_Ref (Feature))));

            else
               New_Subinstance := Instantiate_Corresponding_Entity
                 (Instance_Root, Feature, Container);

               if Present (New_Subinstance) then
                  Set_Corresponding_Instance (New_Instance, New_Subinstance);
               else
                  Success := False;
               end if;
            end if;

         when others =>
            raise Program_Error with "Unknown feature kind "
              & ATN.Kind (Feature)'Img;
      end case;

      if Success then
         Set_Corresponding_Declaration (New_Instance, Feature);
         return New_Instance;
      else
         return No_Node;
      end if;
   end Instantiate_Feature;

   --------------------------------------
   -- Instantiate_Corresponding_Entity --
   --------------------------------------

   function Instantiate_Corresponding_Entity
     (Instance_Root : Node_Id;
      Feature       : Node_Id;
      Container     : Node_Id)
     return Node_Id
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Kind (Feature) = K_Subcomponent_Access
                     or else Kind (Feature) = K_Feature_Group_Spec
                     or else Kind (Feature) = K_Subprogram_Spec
                     or else Kind (Feature) = K_Parameter
                     or else Kind (Feature) = K_Port_Spec);

      Namespace_Model    : Node_Id;
      Namespace_Instance : Node_Id;
      C                  : Node_Id;
      New_Subinstance    : Node_Id := No_Node;
   begin
      if No (Entity_Ref (Feature)) then
         --  Abort the instantiation of the corresponding entity if there
         --  is no such corresponding entity.

         return No_Node;
      else
         C := ATE.Get_Referenced_Entity (Entity_Ref (Feature));

         --  Annotate the component with container

         Annotate (C, Container);

         --  Getting the component namespace

         Namespace_Model := ATN.Namespace (C);
         Namespace_Instance := Instantiate_Namespace
           (Instance_Root, Namespace_Model);

         --  Verify whether the component has been instantiateed or not

         New_Subinstance := Get_First_Contained_Homonym
           (AIN.Declarations (Namespace_Instance),
            ATE.Get_Referenced_Entity (Entity_Ref (Feature)));

         --  If the component is already instantiateed, return it

         if Present (New_Subinstance) then
            return New_Subinstance;
         end if;

         --  If the component isn't instantiated yet, instantiate it...

         New_Subinstance := Instantiate_Component (Instance_Root, C);

         if Present (New_Subinstance) then
            --  Instantiation is successful, append the component to the
            --  declarations of its namespace. If the component has
            --  subcomponents, they will be added recursivly.

            --  The namespace declaration list is a node container
            --  list because we cannot append the same node in two
            --  different lists.

            Append_To_Namespace_Instance (Instance_Root, New_Subinstance);

         else
            --  Something went wrong, propagate the information by
            --  returning No_Node.

            return No_Node;
         end if;
      end if;

      return New_Subinstance;
   end Instantiate_Corresponding_Entity;

   ---------------------------------
   -- Instantiate_Port_Group_Type --
   ---------------------------------

   function Instantiate_Port_Group_Type
     (Instance_Root : Node_Id;
      Port_Group    : Node_Id;
      Feature_List  : List_Id;
      Container     : Node_Id;
      Inverse       : Boolean := False)
     return Boolean
   is
      pragma Assert (Feature_List /= No_List);
      pragma Assert (AIN.Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (ATN.Kind (Port_Group) = K_Feature_Group_Type);

      Success         : Boolean := True;
      List_Node       : Node_Id := No_Node;
      Instantiateable_Node : Node_Id := No_Node;
      New_Subinstance : Node_Id := No_Node;
   begin
      --  Annotate the parent port group with the container node

      Annotate (Port_Group, Container);

      --  Parent port group

      if Present (ATN.Parent (Port_Group))
        and then Present (ATE.Get_Referenced_Entity
                            (ATN.Parent (Port_Group)))
      then
         --  Instantiate the parent port group

         Success := Instantiate_Port_Group_Type
           (Instance_Root,
            ATE.Get_Referenced_Entity
              (ATN.Parent (Port_Group)),
            Feature_List,
            Container,
            Inverse);
      end if;

      --  Features

      if not ATNU.Is_Empty (ATN.Features (Port_Group)) then
         List_Node := ATN.First_Node (ATN.Features (Port_Group));

         while Present (List_Node) loop
            if not Is_Implicit_Inverse (List_Node) then
               if Inverse then
                  Instantiateable_Node := Inversed_Entity (List_Node);
               else
                  Instantiateable_Node := List_Node;
               end if;

               New_Subinstance := Instantiate_Feature
                 (Instance_Root,
                  Instantiateable_Node,
                  Inverse);

               if Present (New_Subinstance) then
                  Append_Node_To_List (New_Subinstance, Feature_List);
               else
                  Success := False;
               end if;
            end if;

            List_Node := ATN.Next_Node (List_Node);
         end loop;
      end if;

      --  Inverse Of

      if Present (Inverse_Of (Port_Group))
        and then Present (ATE.Get_Referenced_Entity (Inverse_Of (Port_Group)))
      then
         Success := Instantiate_Port_Group_Type
           (Instance_Root,
            ATE.Get_Referenced_Entity (Inverse_Of (Port_Group)),
            Feature_List,
            Container,
            not Inverse)
           and then Success;
      end if;

      return Success;
   end Instantiate_Port_Group_Type;

end Ocarina.Instances.Components.Features;
