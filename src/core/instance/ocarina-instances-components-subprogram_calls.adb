------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.INSTANCES.COMPONENTS.SUBPROGRAM_CALLS               --
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

with Locations;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Annotations;
with Ocarina.Instances.Messages;
with Ocarina.Instances.Components;
with Ocarina.Instances.Namespaces;

package body Ocarina.Instances.Components.Subprogram_Calls is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   use Ocarina.Annotations;
   use Ocarina.Instances.Messages;
   use Ocarina.Instances.Components;
   use Ocarina.Instances.Namespaces;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package AIE renames Ocarina.ME_AADL.AADL_Instances.Entities;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;

   -------------------------------
   -- Instantiate_Call_Sequence --
   -------------------------------

   function Instantiate_Call_Sequence
     (Instance_Root : Node_Id;
      Call_Sequence : Node_Id) return Node_Id
   is

      pragma Assert (AIN.Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (ATN.Kind (Call_Sequence) = K_Subprogram_Call_Sequence);

      New_Instance : constant Node_Id :=
        New_Node (K_Call_Sequence_Instance, ATN.Loc (Call_Sequence));
      New_Subinstance : Node_Id;
      List_Node       : Node_Id;
      Success         : Boolean := True;
   begin
      AIN.Set_Identifier
        (New_Instance,
         AIE.Duplicate_Identifier (ATN.Identifier (Call_Sequence)));
      Set_Corresponding_Declaration (New_Instance, Call_Sequence);

      if ATN.Subprogram_Calls (Call_Sequence) /= No_List then
         AIN.Set_Subprogram_Calls
           (New_Instance,
            New_List
              (K_List_Id,
               ATN.Loc (Node_Id (ATN.Subprogram_Calls (Call_Sequence)))));
         List_Node := ATN.First_Node (ATN.Subprogram_Calls (Call_Sequence));

         while Present (List_Node) loop
            New_Subinstance :=
              Instantiate_Subprogram_Call (Instance_Root, List_Node);

            if Present (New_Subinstance) then
               AIN.Set_Parent_Sequence (New_Subinstance, New_Instance);
               Append_Node_To_List
                 (New_Subinstance,
                  AIN.Subprogram_Calls (New_Instance));
            else
               Success := False;
            end if;

            List_Node := ATN.Next_Node (List_Node);
         end loop;
      end if;

      if Success then
         return New_Instance;
      else
         return No_Node;
      end if;
   end Instantiate_Call_Sequence;

   ---------------------------------
   -- Instantiate_Subprogram_Call --
   ---------------------------------

   function Instantiate_Subprogram_Call
     (Instance_Root   : Node_Id;
      Subprogram_Call : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Kind (Subprogram_Call) = K_Subprogram_Call);

      New_Instance : constant Node_Id :=
        New_Node (K_Call_Instance, ATN.Loc (Subprogram_Call));
      Container : constant Node_Id :=
        Container_Component (ATN.Parent_Sequence (Subprogram_Call));
      New_Subinstance    : Node_Id := No_Node;
      Namespace_Instance : Node_Id := No_Node;
      Namespace_Model    : Node_Id;
      Owner_Data         : Node_Id;
   begin
      AIN.Set_Identifier
        (New_Instance,
         AIE.Duplicate_Identifier (ATN.Identifier (Subprogram_Call)));
      AIN.Set_Corresponding_Declaration (New_Instance, Subprogram_Call);

      if Present (Entity_Ref (Subprogram_Call))
        and then Present
          (ATE.Get_Referenced_Entity (Entity_Ref (Subprogram_Call)))
      then
         --  Getting the component namespace

         if ATN.Kind
             (ATE.Get_Referenced_Entity (Entity_Ref (Subprogram_Call))) =
           K_Subcomponent_Access
         then
            Namespace_Model :=
              ATN.Namespace
                (ATE.Get_Referenced_Entity
                   (Entity_Ref
                      (ATE.Get_Referenced_Entity
                         (Entity_Ref (Subprogram_Call)))));
         else
            Namespace_Model :=
              ATN.Namespace
                (ATE.Get_Referenced_Entity (Entity_Ref (Subprogram_Call)));
         end if;

         Namespace_Instance :=
           Instantiate_Namespace (Instance_Root, Namespace_Model);

         --  If this is a method call, get the owner data component

         if ATNU.Length (ATN.Path (Entity_Ref (Subprogram_Call))) > 1 then
            Owner_Data :=
              ATN.Corresponding_Entity
                (ATN.Item
                   (ATN.First_Node (ATN.Path (Entity_Ref (Subprogram_Call)))));

            --  If we deal with a call to a subprogram
            --  implementation, the length of the path is 2 but the
            --  first identifier does not reference anything. Thus,
            --  Owner_Data is nul for subprogram implementation calls.

            if Present (Owner_Data)
              and then
              (Kind (Owner_Data) = K_Component_Type
               or else Kind (Owner_Data) = K_Component_Implementation)
              and then ATE.Get_Category_Of_Component (Owner_Data) = CC_Data
            then
               declare
                  D : Node_Id;
                  F : Node_Id;
               begin
                  AIN.Set_Path
                    (New_Instance,
                     New_List (K_List_Id, Locations.No_Location));
                  D := Instantiate_Component (Instance_Root, Owner_Data);

                  --  Append the data that owns the method

                  Append_Node_To_List
                    (Make_Node_Container (D),
                     AIN.Path (New_Instance));

                  --  Append the feature that correspods to the call

                  F := AIN.First_Node (AIN.Features (D));

                  while Present (F) loop
                     --  This case occurs in AADLv1
                     if Kind (F) = K_Subprogram_Spec_Instance
                       and then
                         Corresponding_Declaration (F) =
                         Entity (Entity_Ref (Subprogram_Call))
                     then
                        Append_Node_To_List
                          (Make_Node_Container (F),
                           AIN.Path (New_Instance));
                     end if;

                     --  This case occurs in AADLv2
                     if Kind (F) = K_Subcomponent_Access_Instance
                       and then
                         Corresponding_Declaration (F) =
                         Entity (Entity_Ref (Subprogram_Call))
                     then
                        Append_Node_To_List
                          (Make_Node_Container (F),
                           AIN.Path (New_Instance));
                     end if;

                     F := AIN.Next_Node (F);
                  end loop;

                  --  If no feature matched, there is a serious problem

                  if Length (AIN.Path (New_Instance)) < 2 then
                     raise Program_Error with "Owner data without features";
                  end if;

                  --  Annotate the data component with parent component
                  --  of the subprogram call

                  Annotate (Owner_Data, Container);
               end;
            end if;
         end if;

         --  In AADLv1
         --  Each subprogram call instantiation leads to the creation of a
         --  new subprogram component instance.

         declare
            Component : constant Node_Id :=
              ATE.Get_Referenced_Entity (Entity_Ref (Subprogram_Call));
            Spg_Component : Node_Id := No_Node;
         begin
            if ATN.Kind (Component) = K_Subcomponent_Access
              and then Present (ATN.Entity (Entity_Ref (Component)))
            then
               Spg_Component := ATN.Entity (Entity_Ref (Component));
            else
               Spg_Component := Component;
            end if;

            New_Subinstance :=
              Instantiate_Component (Instance_Root, Spg_Component);

            if Present (New_Subinstance) then
               Set_Corresponding_Instance (New_Instance, New_Subinstance);
               Set_Parent_Subcomponent (New_Subinstance, New_Instance);

               --  The namespace declaration list is a node container
               --  list because we cannot append the same node in two
               --  different lists.

               Append_Node_To_List
                 (Make_Node_Container (New_Subinstance),
                  AIN.Declarations (Namespace_Instance));

               --  Annotate the subprogram component with container
               --  component of the subprogram call.

               Annotate (Spg_Component, Container);

               return New_Instance;
            else
               return No_Node;
            end if;
         end;

      else
         Display_No_Entity_Ref (New_Instance);
         return No_Node;
      end if;
   end Instantiate_Subprogram_Call;

   ----------------------------------------
   -- Duplicate_Subprogram_Call_Instance --
   ----------------------------------------

   function Duplicate_Subprogram_Call_Instance
     (Instance_Root : Node_Id;
      Call_Instance : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Call_Instance) = K_Call_Instance);

      New_Subinstance : Node_Id;
   begin
      if Default_Instance
          (Corresponding_Declaration
             (Corresponding_Instance (Call_Instance))) =
        Corresponding_Instance (Call_Instance)
      then
         --  We only create a new instance of the subprogram if the
         --  current instance is the default one.

         New_Subinstance :=
           Instantiate_Component
             (Instance_Root,
              Corresponding_Declaration
                (Corresponding_Instance (Call_Instance)));

         if Present (New_Subinstance) then
            Set_Corresponding_Instance (Call_Instance, New_Subinstance);
            return Call_Instance;

         else
            return No_Node;
         end if;

      else
         return Call_Instance;
      end if;
   end Duplicate_Subprogram_Call_Instance;

end Ocarina.Instances.Components.Subprogram_Calls;
