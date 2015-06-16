------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . I N S T A N C E S . F I N D E R              --
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
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Instances.Components.Subprogram_Calls;
with Errors;
with Locations;

package body Ocarina.Instances.Finder is

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Locations;
   use Errors;

   use Ocarina.Instances.Components.Subprogram_Calls;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   procedure Select_Nodes
     (Decl_List  :        List_Id;
      Kinds      :        Node_Kind_Array;
      First_Node : in out Node_Id;
      Last_Node  : in out Node_Id);
   --  Build a list (chained using the accessor Next_Entity) from
   --  Decl_List and appends it to Last_Node. This list will contain
   --  the nodes whose kinds correspond to Kinds.

   procedure Select_Single_Node
     (Node       :        Node_Id;
      Kinds      :        Node_Kind_Array;
      First_Node : in out Node_Id;
      Last_Node  : in out Node_Id);
   --  Same as previous select_nodes, yet only appends a single node if
   --  its kind belong to the kinds array

   -------------------
   -- Find_Instance --
   -------------------

   function Find_Instance
     (Instance_Root      : Node_Id;
      Reference_Instance : Node_Id;
      Path               : List_Id) return Node_Id
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Present (Reference_Instance));

      List_Node                  : Node_Id;
      Actual_List_Node           : Node_Id;
      Pointed_Instance           : Node_Id;
      Use_Contained_Element_Path : Boolean := False;

   begin
      --  Path may be the path of an entity reference; in this case it
      --  is a list of node_container that contain identifiers. Else
      --  it is a list of identifiers.

      if Path = No_List then
         Pointed_Instance := Reference_Instance;

      else
         Pointed_Instance := Reference_Instance;
         List_Node        := ATN.First_Node (Path);

         while Present (List_Node) loop
            if not Use_Contained_Element_Path then
               if ATN.Kind (List_Node) = K_Node_Container then
                  Actual_List_Node := ATN.Item (List_Node);

               elsif ATN.Kind (List_Node) = K_Identifier then
                  Actual_List_Node := List_Node;

               else
                  --  XXX check whether AADLv2 always implies
                  --  Use_Contained_Element_Path to be true.

                  Actual_List_Node := ATN.First_Node (List_Items (List_Node));
                  Use_Contained_Element_Path := True;
               end if;
            end if;

            pragma Assert (ATN.Kind (Actual_List_Node) = K_Identifier);

            Pointed_Instance :=
              Find_Local_Instance (Pointed_Instance, Actual_List_Node);

            if No (Pointed_Instance) then
               exit;

            elsif Kind (Pointed_Instance) = K_Call_Instance then
               Pointed_Instance :=
                 Duplicate_Subprogram_Call_Instance
                   (Instance_Root,
                    Pointed_Instance);
               Pointed_Instance := Corresponding_Instance (Pointed_Instance);

            elsif Kind (Pointed_Instance) = K_Subcomponent_Instance then
               Pointed_Instance := Corresponding_Instance (Pointed_Instance);

            end if;

            if not Use_Contained_Element_Path then
               List_Node := ATN.Next_Node (List_Node);
            else
               Actual_List_Node := ATN.Next_Node (Actual_List_Node);
               if No (Actual_List_Node) then
                  exit;
               end if;
            end if;
         end loop;
      end if;

      return Pointed_Instance;
   end Find_Instance;

   --------------------
   -- Find_All_Flows --
   --------------------

   procedure Find_All_Flows
     (Instance_Root :        Node_Id;
      First_Node    : in out Node_Id;
      Last_Node     : in out Node_Id)
   is
      use Ocarina.ME_AADL.AADL_Instances.Nutils;

      pragma Assert (AIN.Kind (Instance_Root) = K_Architecture_Instance);

      R : constant Node_Id := AIN.Root_System (Instance_Root);
      N : Node_Id;
   begin
      if Ocarina.ME_AADL.AADL_Tree.Nutils.Is_Empty (AIN.Flows (R))
        and then Present (First_Node)
        and then Present (Last_Node)
      then
         First_Node := No_Node;
         Last_Node  := No_Node;
         return;
      end if;

      N := ATN.First_Node (AIN.Flows (R));
      while Present (N) loop
         if ATN.Kind (N) = K_End_To_End_Flow_Spec then
            if No (First_Node) then
               First_Node := N;
               Last_Node  := N;
               ATN.Set_Next_Entity (N, No_Node);
            else
               ATN.Set_Next_Entity (N, No_Node);
               ATN.Set_Next_Entity (Last_Node, N);
               Last_Node := N;
            end if;
         end if;

         N := ATN.Next_Node (N);
      end loop;
   end Find_All_Flows;

   ------------------------
   -- Find_All_Instances --
   ------------------------

   procedure Find_All_Instances
     (Instance_Root :        Node_Id;
      Kinds         :        Node_Kind_Array;
      First_Node    : in out Node_Id;
      Last_Node     : in out Node_Id)
   is
      use Ocarina.ME_AADL.AADL_Instances.Nutils;

      pragma Assert
        (Kind (Instance_Root) = K_Architecture_Instance
         or else Kind (Instance_Root) = K_Component_Instance
         or else Kind (Instance_Root) = K_Subcomponent_Instance
         or else Kind (Instance_Root) = K_Call_Sequence_Instance
         or else Kind (Instance_Root) = K_Call_Instance);

      R         : Node_Id;
      List_Node : List_Id;
      N         : Node_Id;
   begin
      case AIN.Kind (Instance_Root) is
         when AIN.K_Architecture_Instance =>
            R := AIN.Root_System (Instance_Root);
            Select_Single_Node (R, Kinds, First_Node, Last_Node);

            --  We first get the declarations of the unnamed namespace

            if Present (AIN.Unnamed_Namespace (Instance_Root))
              and then not Is_Empty
                (AIN.Declarations (AIN.Unnamed_Namespace (Instance_Root)))
            then
               List_Node :=
                 AIN.Declarations (AIN.Unnamed_Namespace (Instance_Root));
               Select_Nodes (List_Node, Kinds, First_Node, Last_Node);
            end if;

         when AIN.K_Subcomponent_Instance =>
            R := AIN.Corresponding_Instance (Instance_Root);
            Select_Single_Node (R, Kinds, First_Node, Last_Node);

         when AIN.K_Component_Instance =>
            R := Instance_Root;
            Select_Single_Node (Instance_Root, Kinds, First_Node, Last_Node);

         when AIN.K_Call_Sequence_Instance =>
            --  Special case :
            --  we parse recursively subprogram calls

            R := Instance_Root;
            Select_Nodes
              (AIN.Subprogram_Calls (R),
               Kinds,
               First_Node,
               Last_Node);
            if not Is_Empty (AIN.Subprogram_Calls (R)) then
               N := AIN.First_Node (AIN.Subprogram_Calls (R));
               while Present (N) loop
                  Find_All_Instances (N, Kinds, First_Node, Last_Node);
                  N := AIN.Next_Node (N);
               end loop;
            end if;
            return;

         when AIN.K_Call_Instance =>
            R := AIN.Corresponding_Instance (Instance_Root);
            Select_Single_Node (R, Kinds, First_Node, Last_Node);

         when others =>
            DE (Locations.Image (AIN.Loc (Instance_Root)) &
               " : unexpected instance kind");
            return;
      end case;

      Select_Nodes (AIN.Calls (R), Kinds, First_Node, Last_Node);
      Select_Nodes (AIN.Connections (R), Kinds, First_Node, Last_Node);

      --  Recursively search in call sequences and subcomponents instances

      if not Is_Empty (AIN.Subcomponents (R)) then
         N := AIN.First_Node (AIN.Subcomponents (R));
         while Present (N) loop
            Find_All_Instances (N, Kinds, First_Node, Last_Node);
            N := AIN.Next_Node (N);
         end loop;
      end if;

      if not Is_Empty (AIN.Calls (R)) then
         N := AIN.First_Node (AIN.Calls (R));
         while Present (N) loop
            Find_All_Instances (N, Kinds, First_Node, Last_Node);
            N := AIN.Next_Node (N);
         end loop;
      end if;
   end Find_All_Instances;

   -------------------------------
   -- Find_Instance_In_Instance --
   -------------------------------

   function Find_Instance_In_Instance
     (Instance_Root      : Node_Id;
      Reference_Instance : Node_Id;
      Path               : List_Id) return Node_Id
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Present (Reference_Instance));

      List_Node        : Node_Id;
      Actual_List_Node : Node_Id;
      Pointed_Instance : Node_Id;
   begin
      --  Path may be the path of an entity reference; in this case it
      --  is a list of node_container that contain identifiers. Else
      --  it is a list of identifiers.

      if Path = No_List then
         Pointed_Instance := Reference_Instance;
      else
         Pointed_Instance := Reference_Instance;
         List_Node        := AIN.First_Node (Path);

         while Present (List_Node) loop
            if ATN.Kind (List_Node) = K_Node_Container then
               Actual_List_Node := ATN.Item (List_Node);
            elsif ATN.Kind (List_Node) = K_Identifier then
               Actual_List_Node := List_Node;
            else
               Actual_List_Node := ATN.First_Node (List_Items (List_Node));
            end if;

            pragma Assert (ATN.Kind (Actual_List_Node) = K_Identifier);

            Pointed_Instance :=
              Find_Local_Instance (Pointed_Instance, Actual_List_Node);

            if No (Pointed_Instance) then
               exit;
            elsif Kind (Pointed_Instance) = K_Call_Instance then
               Pointed_Instance :=
                 Duplicate_Subprogram_Call_Instance
                   (Instance_Root,
                    Pointed_Instance);
               Pointed_Instance := Corresponding_Instance (Pointed_Instance);
            elsif Kind (Pointed_Instance) = K_Subcomponent_Instance then
               Pointed_Instance := Corresponding_Instance (Pointed_Instance);
            end if;

            List_Node := ATN.Next_Node (List_Node);
         end loop;
      end if;

      return Pointed_Instance;
   end Find_Instance_In_Instance;

   -------------------------
   -- Find_Local_Instance --
   -------------------------

   function Find_Local_Instance
     (Reference_Instance  : Node_Id;
      Instance_Identifier : Node_Id) return Node_Id
   is
      pragma Assert
        (AIN.Kind (Reference_Instance) = K_Component_Instance
         or else AIN.Kind (Reference_Instance) = K_Connection_Instance);
      pragma Assert (ATN.Kind (Instance_Identifier) = K_Identifier);

      List_Node          : Node_Id;
      Sequence_List_Node : Node_Id;
      Instance_Name      : constant Name_Id := ATN.Name (Instance_Identifier);
      Component_Instance : Node_Id;
   begin
      if Kind (Reference_Instance) = K_Component_Instance then
         Component_Instance := Reference_Instance;
      else
         Component_Instance := Parent_Component (Reference_Instance);
      end if;

      --  We compare lower case names. So, when we use
      --  Get_Name_Of_Entity, we set the Get_Display_Name flag to
      --  'False'.

      if AIN.Features (Component_Instance) /= No_List then
         List_Node := AIN.First_Node (AIN.Features (Component_Instance));

         while Present (List_Node) loop
            if Get_Name_Of_Entity (List_Node, False) = Instance_Name then
               return List_Node;
            end if;

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      if AIN.Subcomponents (Component_Instance) /= No_List then
         List_Node := AIN.First_Node (AIN.Subcomponents (Component_Instance));

         while Present (List_Node) loop
            if Get_Name_Of_Entity (List_Node, False) = Instance_Name then
               return List_Node;
            end if;

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      if AIN.Modes (Component_Instance) /= No_List then
         List_Node := AIN.First_Node (AIN.Modes (Component_Instance));

         while Present (List_Node) loop
            if Get_Name_Of_Entity (List_Node, False) = Instance_Name then
               return List_Node;
            end if;

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      if AIN.Connections (Component_Instance) /= No_List then
         List_Node := AIN.First_Node (AIN.Connections (Component_Instance));

         while Present (List_Node) loop
            if Get_Name_Of_Entity (List_Node, False) = Instance_Name then
               return List_Node;
            end if;

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      if AIN.Calls (Component_Instance) /= No_List then
         List_Node := AIN.First_Node (AIN.Calls (Component_Instance));

         while Present (List_Node) loop
            if Get_Name_Of_Entity (List_Node, False) = Instance_Name then
               return List_Node;

            elsif AIN.Subprogram_Calls (List_Node) /= No_List then
               Sequence_List_Node :=
                 AIN.First_Node (AIN.Subprogram_Calls (List_Node));

               while Present (Sequence_List_Node) loop
                  if Get_Name_Of_Entity (Sequence_List_Node, False) =
                    Instance_Name
                  then
                     return Sequence_List_Node;
                  end if;

                  Sequence_List_Node := AIN.Next_Node (Sequence_List_Node);
               end loop;
               List_Node := AIN.Next_Node (List_Node);
            end if;
         end loop;
      end if;

      if AIN.Flows (Component_Instance) /= No_List then
         List_Node := AIN.First_Node (AIN.Flows (Component_Instance));

         while Present (List_Node) loop
            if Get_Name_Of_Entity (List_Node, False) = Instance_Name then
               return List_Node;
            end if;

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      return No_Node;
   end Find_Local_Instance;

   ------------------
   -- Select_Nodes --
   ------------------

   procedure Select_Nodes
     (Decl_List  :        List_Id;
      Kinds      :        Node_Kind_Array;
      First_Node : in out Node_Id;
      Last_Node  : in out Node_Id)
   is
      use Ocarina.ME_AADL.AADL_Instances.Nutils;

      Success         : Boolean;
      Local_List_Node : Node_Id;
   begin
      if not Is_Empty (Decl_List) then
         Local_List_Node := AIN.First_Node (Decl_List);

         while Present (Local_List_Node) loop
            Success := False;

            for K in Kinds'Range loop
               Success :=
                 Success or else (AIN.Kind (Local_List_Node) = Kinds (K));
            end loop;

            if Success then
               if No (First_Node) then
                  First_Node := Local_List_Node;
                  Last_Node  := Local_List_Node;
               else
                  AIN.Set_Next_Entity (Last_Node, Local_List_Node);
                  AIN.Set_Next_Entity (Local_List_Node, No_Node);
                  Last_Node := Local_List_Node;
               end if;
            end if;

            Local_List_Node := AIN.Next_Node (Local_List_Node);
         end loop;
      end if;
   end Select_Nodes;

   ------------------------
   -- Select_Single_Node --
   ------------------------

   procedure Select_Single_Node
     (Node       :        Node_Id;
      Kinds      :        Node_Kind_Array;
      First_Node : in out Node_Id;
      Last_Node  : in out Node_Id)
   is
      use Ocarina.ME_AADL.AADL_Instances.Nutils;

      Success : Boolean;
   begin
      if Present (Node) then
         Success := False;

         for K in Kinds'Range loop
            Success := Success or else (AIN.Kind (Node) = Kinds (K));
         end loop;

         if Success then
            if No (First_Node) then
               First_Node := Node;
               Last_Node  := Node;
            else
               AIN.Set_Next_Entity (Last_Node, Node);
               AIN.Set_Next_Entity (Node, No_Node);
               Last_Node := Node;
            end if;
         end if;
      end if;
   end Select_Single_Node;

end Ocarina.Instances.Finder;
