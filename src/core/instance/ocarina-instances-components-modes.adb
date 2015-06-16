------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . I N S T A N C E S . C O M P O N E N T S . M O D E S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

package body Ocarina.Instances.Components.Modes is

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AIE renames Ocarina.ME_AADL.AADL_Instances.Entities;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   function Fetch_Port_Reference_Instance
     (Ref : Node_Id;
      C   : Node_Id) return Node_Id;
   --  Return an reference to the instance corresponding to the port
   --  declaration referenced by Ref. C is the AADL component in which
   --  the search begins. Research can be continued inside the
   --  subcomponent of C incase Ref references a subcomponent port
   --  (C.Th_1.Out_Port for example).

   -----------------------------------
   -- Fetch_Port_Reference_Instance --
   -----------------------------------

   function Fetch_Port_Reference_Instance
     (Ref : Node_Id;
      C   : Node_Id) return Node_Id
   is
      Ref_Inst : Node_Id;

      procedure Recursive_Path_Search
        (Path_Element : Node_Id;
         Component    : Node_Id);
      --  Recusively update Ref_Inst, starting from Path_Element and
      --  continuing to its next element and to C component
      --  containment hierarchy.

      ---------------------------
      -- Recursive_Path_Search --
      ---------------------------

      procedure Recursive_Path_Search
        (Path_Element : Node_Id;
         Component    : Node_Id)
      is
         N : Node_Id;
      begin
         if No (Path_Element) then
            --  This cannot happen if the semantic analyzer did its
            --  work well.

            raise Program_Error
              with "FATAL: A feature path that does not end with a feature";
         end if;

         pragma Assert
           (AIN.Kind (Component) = K_Component_Instance
            and then ATN.Kind (Path_Element) = K_Node_Container
            and then ATN.Kind (ATN.Item (Path_Element)) = K_Identifier);

         case ATN.Kind (ATN.Corresponding_Entity (ATN.Item (Path_Element))) is
            when K_Port_Spec        |
              K_Subcomponent_Access |
              K_Feature_Group_Spec  |
              K_Parameter           |
              K_Subprogram_Spec     =>

               --  This is a local feature, we search it within the
               --  features of the current component, add it to the
               --  new path and stop the search.

               N :=
                 Get_First_Homonym_Instance
                   (AIN.Features (Component),
                    ATN.Name (ATN.Item (Path_Element)));

               Add_Path_Element_To_Entity_Reference (Ref_Inst, N);

            when K_Subcomponent =>

               --  This is a feature belonging to a subcomponent, we
               --  fetch the subcomponent instance, add it to the new
               --  path and then we continue the search with the
               --  component instance corresponding to the
               --  subcomponent instance.

               N :=
                 Get_First_Homonym
                   (ATN.Subcomponents (Component),
                    ATN.Name (ATN.Item (Path_Element)));

               Add_Path_Element_To_Entity_Reference (Ref_Inst, N);

               Recursive_Path_Search
                 (ATN.Next_Node (Path_Element),
                  AIN.Corresponding_Instance (N));

            when K_Subprogram_Call =>

               --  This is a subprogram call port, we act like for
               --  subomponents.

               declare
                  Call_Seq : Node_Id := ATN.First_Node (ATN.Calls (Component));
               begin
                  while Present (Call_Seq) loop
                     N :=
                       Get_First_Homonym
                         (ATN.Subprogram_Calls (Call_Seq),
                          ATN.Name (ATN.Item (Path_Element)));

                     if Present (N) then
                        Add_Path_Element_To_Entity_Reference (Ref_Inst, N);

                        Recursive_Path_Search
                          (ATN.Next_Node (Path_Element),
                           AIN.Corresponding_Instance (N));
                        exit;
                     end if;

                     Call_Seq := ATN.Next_Node (Call_Seq);
                  end loop;
               end;

            when others =>
               raise Program_Error;

         end case;
      end Recursive_Path_Search;

      Path_Element : Node_Id;
   begin
      Ref_Inst := New_Node (K_Entity_Reference_Instance, ATN.Loc (Ref));
      AIN.Set_Path (Ref_Inst, AINU.New_List (K_List_Id, ATN.Loc (Ref)));

      if ATN.Kind (Ref) = K_Mode_Transition_Trigger then
         Path_Element := ATN.First_Node (ATN.Path (ATN.Identifier (Ref)));
      else
         Path_Element := ATN.First_Node (ATN.Path (Ref));
      end if;

      Recursive_Path_Search (Path_Element, C);

      return Ref_Inst;
   end Fetch_Port_Reference_Instance;

   ----------------------
   -- Instantiate_Mode --
   ----------------------

   function Instantiate_Mode
     (Instance_Root      : Node_Id;
      Component_Instance : Node_Id;
      Mode               : Node_Id) return Node_Id
   is
      New_Instance : Node_Id;
   begin
      pragma Assert
        (Kind (Instance_Root) = K_Architecture_Instance
         and then Kind (Component_Instance) = K_Component_Instance
         and then Kind (Mode) = K_Mode);

      New_Instance := New_Node (K_Mode_Instance, ATN.Loc (Mode));

      AIN.Set_Corresponding_Declaration (New_Instance, Mode);
      AIN.Set_Identifier
        (New_Instance,
         AIE.Duplicate_Identifier (ATN.Identifier (Mode)));
      AIN.Set_Parent_Component (New_Instance, Component_Instance);
      AIN.Set_Is_Initial (New_Instance, ATN.Is_Initial (Mode));

      return New_Instance;
   end Instantiate_Mode;

   ---------------------------------
   -- Instantiate_Mode_Transition --
   ---------------------------------

   function Instantiate_Mode_Transition
     (Instance_Root      : Node_Id;
      Component_Instance : Node_Id;
      Mode_Transition    : Node_Id) return Node_Id
   is
      New_Instance : Node_Id;
      N            : Node_Id;
      M            : Node_Id;
   begin
      pragma Assert
        (Kind (Instance_Root) = K_Architecture_Instance
         and then Kind (Component_Instance) = K_Component_Instance
         and then Kind (Mode_Transition) = K_Mode_Transition);

      New_Instance :=
        New_Node (K_Mode_Transition_Instance, ATN.Loc (Mode_Transition));

      AIN.Set_Corresponding_Declaration (New_Instance, Mode_Transition);
      AIN.Set_Parent_Component (New_Instance, Component_Instance);
      AIN.Set_Source_Modes
        (New_Instance,
         AINU.New_List (K_List_Id, ATN.Loc (Mode_Transition)));
      AIN.Set_Triggers
        (New_Instance,
         AINU.New_List (K_List_Id, ATN.Loc (Mode_Transition)));

      --  Fetch the mode instances that corresponds to the transition
      --  source.

      N := ATN.First_Node (ATN.Source_Modes (Mode_Transition));

      while Present (N) loop
         M :=
           Get_First_Homonym_Instance
             (AIN.Modes (Component_Instance),
              ATN.Name (N));

         if No (M) then
            return No_Node;
         end if;

         pragma Assert (Kind (M) = K_Mode_Instance);

         AINU.Append_Node_To_List
           (Make_Node_Container (M),
            AIN.Source_Modes (New_Instance));

         N := ATN.Next_Node (N);
      end loop;

      --  Fetch the destination mode of the transition

      M :=
        Get_First_Homonym_Instance
          (AIN.Modes (Component_Instance),
           ATN.Name (ATN.Destination_Mode (Mode_Transition)));

      if No (M) then
         return No_Node;
      end if;

      pragma Assert (Kind (M) = K_Mode_Instance);

      AIN.Set_Destination_Mode (New_Instance, Make_Node_Container (M));

      --  Set the instances of the ports that trigger the transition

      N := ATN.First_Node (ATN.Triggers (Mode_Transition));

      while Present (N) loop
         M := Fetch_Port_Reference_Instance (N, Component_Instance);

         if No (M) then
            return No_Node;
         end if;

         pragma Assert (Kind (M) = K_Entity_Reference_Instance);

         AINU.Append_Node_To_List (M, AIN.Triggers (New_Instance));

         N := ATN.Next_Node (N);
      end loop;

      return New_Instance;
   end Instantiate_Mode_Transition;

   --------------------------
   -- Instantiate_In_Modes --
   --------------------------

   procedure Instantiate_In_Modes
     (Component_Instance : Node_Id;
      Subclause_Instance : Node_Id)
   is
      Subclause_In_Modes : Node_Id;
      MoT                : Node_Id; --  Mode or Transition
   begin
      pragma Assert
        (Kind (Component_Instance) = K_Component_Instance
         and then
         (Kind (Subclause_Instance) = K_Subcomponent_Instance
          or else Kind (Subclause_Instance) = K_Call_Sequence_Instance
          or else Kind (Subclause_Instance) = K_Connection_Instance
          or else
            Kind (Subclause_Instance) =
            K_Property_Association_Instance));

      if AIN.Kind (Subclause_Instance) = K_Property_Association_Instance then
         Subclause_In_Modes := AIN.In_Modes (Subclause_Instance);
      else
         Subclause_In_Modes :=
           ATN.In_Modes (Corresponding_Declaration (Subclause_Instance));
      end if;

      if Present (Subclause_In_Modes)
        and then not ATNU.Is_Empty (ATN.Modes (Subclause_In_Modes))
      then
         --  Create the subclause mode container

         AIN.Set_In_Modes
           (Subclause_Instance,
            New_Node (K_In_Modes, ATN.Loc (Subclause_In_Modes)));
         ATN.Set_Modes
           (AIN.In_Modes (Subclause_Instance),
            AINU.New_List (K_List_Id, ATN.Loc (Subclause_In_Modes)));

         MoT := ATN.First_Node (ATN.Modes (Subclause_In_Modes));

         declare
            Mode_List :
              List_Id renames
              ATN.Modes (AIN.In_Modes (Subclause_Instance));
         begin
            while Present (MoT) loop
               case ATN.Kind (MoT) is
                  when K_Entity_Reference =>
                     --  Fetch the mode instance and append it to the
                     --  mode instance list.

                     --  Note: in the case of long inheritance tree,
                     --  we have to defend against the case we are
                     --  instanciating a mode present in a child, but
                     --  not in one of its ancestor.

                     if Present
                         (Get_First_Homonym_Instance
                            (AIN.Modes (Component_Instance),
                             ATN.Name (ATN.Identifier (MoT))))
                     then
                        AINU.Append_Node_To_List
                          (Make_Node_Container
                             (Get_First_Homonym_Instance
                                (AIN.Modes (Component_Instance),
                                 ATN.Name (ATN.Identifier (MoT)))),
                           Mode_List);
                     end if;

                  when K_Pair_Of_Entity_References =>
                     --  Fetch the mode transition ends and append
                     --  them as a two-item node container to the mode
                     --  instance list.

                     ATNU.Append_Node_To_List
                       (Make_Node_Container
                          (Get_First_Homonym
                             (ATN.Modes (Component_Instance),
                              ATN.Name
                                (ATN.Identifier (First_Reference (MoT)))),
                           Get_First_Homonym
                             (ATN.Modes (Component_Instance),
                              ATN.Name
                                (ATN.Identifier (Second_Reference (MoT))))),
                        Mode_List);

                  when others =>
                     --  This cannot happen unless the parser went crazy

                     raise Program_Error
                       with "A non-mode-non-transition in an in_mode list";
               end case;

               MoT := ATN.Next_Node (MoT);
            end loop;
         end;
      end if;
   end Instantiate_In_Modes;

   --------------------------
   -- Instantiate_In_Modes --
   --------------------------

   procedure Instantiate_In_Modes
     (Component_Instance : Node_Id;
      Subclause_List     : List_Id)
   is
      Subclause_Node : Node_Id;
   begin
      pragma Assert (Kind (Component_Instance) = K_Component_Instance);

      if not AINU.Is_Empty (Subclause_List) then
         Subclause_Node := AIN.First_Node (Subclause_List);

         while Present (Subclause_Node) loop
            Instantiate_In_Modes (Component_Instance, Subclause_Node);

            Subclause_Node := AIN.Next_Node (Subclause_Node);
         end loop;
      end if;
   end Instantiate_In_Modes;

end Ocarina.Instances.Components.Modes;
