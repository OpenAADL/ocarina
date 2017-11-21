------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . A N A L Y Z E R . A A D L . N A M I N G _ R U L E S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

with Errors;
with Ocarina.Namet;

with Ocarina.Analyzer.Messages;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;

package body Ocarina.Analyzer.AADL.Naming_Rules is

   use Errors;
   use Ocarina.Namet;
   use Ocarina.Analyzer.Messages;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Scope_Stack;

   type Entity_Conflict_Status is (Replacement, Conflict, No_Conflict);
   --  Used by Resolve_Conflict

   function Is_A_Refinement (Node : Node_Id) return Boolean;

   function Merge_Packages (Pack1 : Node_Id; Pack2 : Node_Id) return Node_Id;
   --  Integrate Pack2 into Pack1 and return Pack1

   function Have_Common_Applies_To
     (Applies_To_1 : List_Id;
      Applies_To_2 : List_Id) return Boolean;
   --  Return True if the two property association declarations apply
   --  to the same thing. This means that the two lists contain the
   --  same identifiers.

   function Resolve_Conflict
     (Entity        : Node_Id;
      Former_Entity : Node_Id) return Entity_Conflict_Status;
   --  Resolve name conflicts if Entity and Former_Entity have the
   --  same name, Entity being added in a scope that already contains
   --  Former_Entity.

   procedure Display_Conflict (N : Node_Id; C : Node_Id);
   --  Output that N conflicts with C

   function Is_Inherited (E : Node_Id) return Boolean;
   pragma Unreferenced (Is_Inherited);
   --  To introduce an inherited entity in the scope of an interface,
   --  we introduce an identifier corresponding to this
   --  entity. However, the identifier of this entity is different
   --  from this new identifier. In particular, the original
   --  identifier refers to the original scope in which the entity was
   --  defined. To decide whether an entity is inherited or not, we
   --  check that the scope of the original identifier is not null
   --  (otherwise, it is a newly-added entity) and that this scope is
   --  different from the current scope.

   function Internal_Node_In_Scope (N : Name_Id; S : Node_Id) return Node_Id;
   procedure Set_In_Scope (N : Node_Id; S : Node_Id);
   procedure Unset_In_Scope (N : Node_Id; S : Node_Id);
   function Internal_Scope_Name (N : Name_Id; S : Node_Id) return Name_Id;
   --  Low level internal routines to enter/remove names in/from
   --  scopes. They are based on hash codes instead of building a
   --  chained list.

   ----------------------
   -- Display_Conflict --
   ----------------------

   procedure Display_Conflict (N : Node_Id; C : Node_Id) is
   begin
      Error_Loc (1)  := Loc (N);
      Error_Loc (2)  := Loc (C);
      Error_Name (1) := Display_Name (N);
      DE ("#conflicts with declaration!");
   end Display_Conflict;

   ------------------
   -- Is_Inherited --
   ------------------

   function Is_Inherited (E : Node_Id) return Boolean is
      S : constant Node_Id := Scope_Entity (Identifier (E));
   begin
      return Present (S) and then S /= Current_Scope;
   end Is_Inherited;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Node_Id is
   begin
      if Last = No_Scope_Depth then
         return No_Node;
      else
         return Table (Last).Node;
      end if;
   end Current_Scope;

   -------------------------
   -- Enter_Name_In_Scope --
   -------------------------

   function Enter_Name_In_Scope (Identifier : Node_Id) return Boolean is
   begin
      return Present (Enter_Name_In_Scope (Identifier));
   end Enter_Name_In_Scope;

   -------------------------
   -- Enter_Name_In_Scope --
   -------------------------

   function Enter_Name_In_Scope (Identifier : Node_Id) return Node_Id is
      pragma Assert (Kind (Identifier) = K_Identifier);

      Entity        : constant Node_Id := Corresponding_Entity (Identifier);
      Local_Scope   : constant Node_Id := Current_Scope;
      Former_Entity : constant Node_Id :=
        Node_In_Scope (Identifier, Local_Scope);
      Kind_Of_Former_Entity : Node_Kind;
      Kind_Of_Entity        : constant Node_Kind := Kind (Entity);
      Entity_Identifier     : Node_Id;
   begin
      if Present (Former_Entity) then
         Kind_Of_Former_Entity := Kind (Former_Entity);
         Entity_Identifier     :=
           Ocarina.ME_AADL.AADL_Tree.Nodes.Identifier (Former_Entity);

         --  This same entity is already in the scope The
         --  Node_In_Scope functions returns only one node. We must
         --  check all the homonyms of this node

         while Present (Entity_Identifier) loop
            if Corresponding_Entity (Entity_Identifier) = Entity then
               return Entity;
            end if;
            Entity_Identifier := Homonym (Entity_Identifier);
         end loop;

         --  This entity is a package. Reload the previous scope.

         if Kind_Of_Former_Entity = K_Package_Specification
           and then Kind_Of_Entity = K_Package_Specification
         then
            declare
               Global_Package : constant Node_Id :=
                 Merge_Packages (Former_Entity, Entity);
            begin
               --  We keep the previous scope entry. There is
               --  nothing to do.

               if No (Global_Package) then
                  Display_Conflict (Identifier, Former_Entity);
               end if;

               return Global_Package;
            end;

         else
            declare
               Conflict_Status : constant Entity_Conflict_Status :=
                 Resolve_Conflict
                   (Entity        => Entity,
                    Former_Entity => Former_Entity);
            begin
               if Conflict_Status = Replacement then
                  --  A connection refinement must inherit the source
                  --  and the destination of the connection it
                  --  refines.

                  if Kind_Of_Entity = K_Connection then
                     Set_Source (Entity, Source (Former_Entity));
                     Set_Destination (Entity, Destination (Former_Entity));
                  end if;

                  Remove_From_Scope
                    (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifier
                       (Former_Entity),
                     Current_Scope);

               elsif Conflict_Status = Conflict then
                  Display_Conflict (Identifier, Former_Entity);
                  return No_Node;
               end if;
            end;
         end if;

      elsif Is_A_Refinement (Entity) then
         DAE (Node1 => Identifier, Message1 => "does not refines anything");
         return No_Node;
      end if;

      Set_Scope_Entity (Identifier, Local_Scope);
      Set_Visible (Identifier, True);
      Set_In_Scope (Identifier, Local_Scope);
      return Entity;
   end Enter_Name_In_Scope;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Increment_Last;
   end Initialize;

   ---------------------
   -- Is_A_Refinement --
   ---------------------

   function Is_A_Refinement (Node : Node_Id) return Boolean is
      pragma Assert (Present (Node));
   begin
      case Kind (Node) is
         when K_Subcomponent     |
           K_Flow_Spec           |
           K_Port_Spec           |
           K_Feature_Group_Spec  |
           K_Subprogram_Spec     |
           K_Parameter           |
           K_Subcomponent_Access |
           K_Mode                |
           K_Mode_Transition     |
           K_Connection          =>
            return Is_Refinement (Node);

         when K_Flow_Implementation | K_End_To_End_Flow_Spec =>
            return False;

         when K_Flow_Implementation_Refinement =>
            case AADL_Version is
               when AADL_V1 =>
                  return True;
               when AADL_V2 =>
                  return False;
            end case;

         when K_End_To_End_Flow_Refinement =>
            return True;

         when others =>
            return False;
      end case;
   end Is_A_Refinement;

   ------------------------------
   -- Node_Explicitly_In_Scope --
   ------------------------------

   function Node_Explicitly_In_Scope
     (Identifier : Node_Id;
      Scope      : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Identifier) = K_Identifier);
   begin
      return Node_In_Scope (Name (Identifier), Scope);
   end Node_Explicitly_In_Scope;

   function Node_Explicitly_In_Scope
     (Name_Of_Identifier : Name_Id;
      Scope              : Node_Id) return Node_Id
   is
      pragma Assert (Name_Of_Identifier /= No_Name);
      pragma Assert (No (Scope) or else Kind (Scope) = K_Scope_Definition);

      Scoped_Identifier : Node_Id;
   begin
      if Present (Scope) then
         Scoped_Identifier :=
           Internal_Node_In_Scope (Name_Of_Identifier, Scope);
         if Present (Scoped_Identifier) then
            return Corresponding_Entity (Scoped_Identifier);
         end if;
      end if;

      return No_Node;
   end Node_Explicitly_In_Scope;

   -------------------
   -- Node_In_Scope --
   -------------------

   function Node_In_Scope
     (Identifier : Node_Id;
      Scope      : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Identifier) = K_Identifier);
   begin
      return Node_In_Scope (Name (Identifier), Scope);
   end Node_In_Scope;

   function Node_In_Scope
     (Name_Of_Identifier : Name_Id;
      Scope              : Node_Id) return Node_Id
   is
      pragma Assert (Name_Of_Identifier /= No_Name);
      pragma Assert
        (No (Scope)
         or else
         (Kind (Scope) = K_Scope_Definition
          and then Present (Corresponding_Entity (Scope))));

      First_Node            : Node_Id := No_Node;
      Homonym_Node          : Node_Id := No_Node;
      Previous_Homonym_Node : Node_Id := No_Node;

      procedure Recursive_Node_In_Scope (The_Scope : Node_Id);
      --  Fetch the Name_Of_Identifier in the given scope AND the
      --  scope of its parent (if any) AND the scope of its
      --  corresponding component type (if any).

      -----------------------------
      -- Recursive_Node_In_Scope --
      -----------------------------

      procedure Recursive_Node_In_Scope (The_Scope : Node_Id) is
         Scoped_Identifier : Node_Id;
         Entity            : Node_Id;
      begin
         if No (The_Scope) or else No (Corresponding_Entity (The_Scope)) then
            return;
         end if;

         --  Start with the given scope

         Scoped_Identifier :=
           Internal_Node_In_Scope (Name_Of_Identifier, The_Scope);

         if Present (Scoped_Identifier) then
            if No (First_Node) then
               First_Node := Corresponding_Entity (Scoped_Identifier);
            else
               Homonym_Node :=
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Identifier (First_Node);
               Previous_Homonym_Node := No_Node;

               while Present (Homonym_Node) loop
                  Previous_Homonym_Node := Homonym_Node;
                  Homonym_Node          := Homonym (Homonym_Node);
               end loop;

               Set_Homonym (Previous_Homonym_Node, Scoped_Identifier);
            end if;
            Set_Homonym (Scoped_Identifier, No_Node);
         end if;

         Entity := Corresponding_Entity (The_Scope);

         --  Fetch the scope of corresponding type and then the parent

         case Kind (Entity) is
            when K_Component_Implementation =>
               --  Fetch recursively the scope of the corresponding type

               if Present (Component_Type_Identifier (Entity)) then
                  Recursive_Node_In_Scope
                    (Entity_Scope
                       (Corresponding_Entity
                          (Component_Type_Identifier (Entity))));
               end if;

               --  Fetch recursively the scope of the parent

               if Present (Parent (Entity)) then
                  Recursive_Node_In_Scope
                    (Entity_Scope (Get_Referenced_Entity (Parent (Entity))));
               end if;

            when K_Component_Type | K_Feature_Group_Type =>
               --  Fetch recursively the scope of the parent

               if Present (Parent (Entity)) then
                  Recursive_Node_In_Scope
                    (Entity_Scope (Get_Referenced_Entity (Parent (Entity))));
               end if;

            when K_Package_Specification =>
               if not Present (First_Node)
                 and then not Is_Empty (Declarations (Entity))
               then
                  declare
                     List_Node, List_Node_2, List_Node_3 : Node_Id;
                     Test_Node                           : Node_Id;
                  begin
                     List_Node :=
                       Ocarina.ME_AADL.AADL_Tree.Nodes.First_Node
                         (Declarations (Entity));

                     while Present (List_Node) loop
                        case Kind (List_Node) is
                           when K_Component_Type        |
                             K_Component_Implementation =>

                              if Name (Identifier (List_Node)) =
                                Name_Of_Identifier
                              then
                                 First_Node := List_Node;
                              end if;

                           when K_Name_Visibility_Declaration =>
                              List_Node_2 :=
                                Ocarina.ME_AADL.AADL_Tree.Nodes.First_Node
                                  (List_Items (List_Node));

                              while Present (List_Node_2) loop
                                 if Kind (List_Node_2) =
                                   K_Alias_Declaration
                                 then
                                    if Is_All (List_Node_2) then
                                       List_Node_3 :=
                                         Ocarina.ME_AADL.AADL_Tree.Nodes
                                           .First_Node
                                           (Declarations
                                              (Parent (Parent (List_Node_2))));
                                       while Present (List_Node_3) loop
                                          if Name (Identifier (List_Node_3)) =
                                            Name
                                              (Build_Package_Identifier
                                                 (Package_Name (List_Node_2)))
                                          then
                                             Test_Node :=
                                               Node_In_Scope
                                                 (Name_Of_Identifier,
                                                  Entity_Scope (List_Node_3));

                                             if Present (Test_Node) then
                                                First_Node := Test_Node;
                                             end if;
                                          end if;
                                          List_Node_3 :=
                                            Next_Node (List_Node_3);
                                       end loop;
                                    else

                                       if Present (Identifier (List_Node_2))
                                         and then
                                           Name (Identifier (List_Node_2)) =
                                           Name_Of_Identifier
                                       then
                                          First_Node := List_Node_2;
                                       end if;
                                    end if;
                                 end if;
                                 List_Node_2 := Next_Node (List_Node_2);
                              end loop;

                           when others =>
                              null;
                        end case;
                        List_Node := Next_Node (List_Node);
                     end loop;
                  end;
               end if;

            when others =>
               null;

         end case;
      end Recursive_Node_In_Scope;

   begin
      Recursive_Node_In_Scope (Scope);
      return First_Node;
   end Node_In_Scope;

   ---------------
   -- Pop_Scope --
   ---------------

   procedure Pop_Scope is
   begin
      --  Pop scope

      Decrement_Last;
   end Pop_Scope;

   ----------------
   -- Push_Scope --
   ----------------

   procedure Push_Scope (Scope : Node_Id) is
      pragma Assert (Kind (Scope) = K_Scope_Definition);

   begin
      Increment_Last;
      Table (Last).Node := Scope;
   end Push_Scope;

   -----------------------
   -- Remove_From_Scope --
   -----------------------

   procedure Remove_From_Scope (Identifier : Node_Id; Scope : Node_Id) is
      pragma Assert (Kind (Identifier) = K_Identifier);
      pragma Assert (Kind (Scope) = K_Scope_Definition);

   begin
      Unset_In_Scope (Identifier, Scope);
      Set_Visible (Identifier, False);
   end Remove_From_Scope;

   --------------------
   -- Merge_Packages --
   --------------------

   function Merge_Packages (Pack1 : Node_Id; Pack2 : Node_Id) return Node_Id is
      pragma Assert (Kind (Pack1) = K_Package_Specification);
      pragma Assert (Kind (Pack2) = K_Package_Specification);
      pragma Assert (Name (Identifier (Pack1)) = Name (Identifier (Pack2)));

      Declaration_List : List_Id;
      Property_List    : List_Id;
   begin
      if Has_Public_Part (Pack1) = Has_Public_Part (Pack2)
        or else Has_Private_Part (Pack1) = Has_Private_Part (Pack2)
      then
         --  We can only merge a public part and a private part. Both
         --  packages cannot have a public or a private part.
         return No_Node;
      end if;

      Set_Has_Public_Part (Pack1, True);
      Set_Has_Private_Part (Pack1, True);
      Declaration_List := Declarations (Pack1);
      Property_List    := Properties (Pack1);
      Append_List_To_List (Declarations (Pack2), Declaration_List);
      Append_List_To_List (Properties (Pack2), Property_List);
      Set_Declarations (Pack1, Declaration_List);
      Set_Properties (Pack1, Property_List);
      return Pack1;
   end Merge_Packages;

   --------------------------
   -- Remove_From_Homonyms --
   --------------------------

   function Remove_From_Homonyms
     (First_Homonym     : Node_Id;
      Homonym_To_Remove : Node_Id) return Node_Id
   is
      pragma Assert (Kind (First_Homonym) = K_Identifier);
      pragma Assert (Kind (Homonym_To_Remove) = K_Identifier);

      List_Homonym          : Node_Id := First_Homonym;
      Previous_List_Homonym : Node_Id := First_Homonym;
   begin
      while Present (List_Homonym) and then List_Homonym /= Homonym_To_Remove
      loop
         Previous_List_Homonym := List_Homonym;
         List_Homonym          := Homonym (List_Homonym);
      end loop;

      if No (List_Homonym) then
         return First_Homonym;

      elsif List_Homonym = First_Homonym then
         Set_Homonym (First_Homonym, No_Node);
         return Homonym (List_Homonym);

      else
         Set_Homonym (Previous_List_Homonym, Homonym (List_Homonym));
         Set_Homonym (List_Homonym, No_Node);
         return Homonym (Previous_List_Homonym);
      end if;
   end Remove_From_Homonyms;

   ----------------------------
   -- Have_Common_Applies_To --
   ----------------------------

   function Have_Common_Applies_To
     (Applies_To_1 : List_Id;
      Applies_To_2 : List_Id) return Boolean
   is

      List_Item_1 : Node_Id;
      List_Item_2 : Node_Id;
   begin
      if Is_Empty (Applies_To_1) and then Is_Empty (Applies_To_2) then
         return True;

      elsif Safe_XOR (Is_Empty (Applies_To_1), Is_Empty (Applies_To_2)) then
         return False;

      else
         List_Item_1 := First_Node (List_Items (First_Node (Applies_To_1)));
         List_Item_2 := First_Node (List_Items (First_Node (Applies_To_2)));
         --  XXX Fix here we must verifiy if we have common applies to in all
         --  contained element path

         while Present (List_Item_1) loop
            pragma Assert (Kind (List_Item_1) = K_Identifier);
            pragma Assert
              (No (List_Item_2) or else Kind (List_Item_2) = K_Identifier);

            if No (List_Item_2) then
               return False;
            elsif Name (List_Item_1) /= Name (List_Item_2) then
               return False;
            end if;

            List_Item_1 := Next_Node (List_Item_1);
            List_Item_2 := Next_Node (List_Item_2);
         end loop;

         if Present (List_Item_2) then
            return False;
         end if;

         return True;
      end if;
   end Have_Common_Applies_To;

   ----------------------
   -- Resolve_Conflict --
   ----------------------

   function Resolve_Conflict
     (Entity        : Node_Id;
      Former_Entity : Node_Id) return Entity_Conflict_Status
   is
      pragma Assert (Present (Former_Entity));

      Category_Of_Entity : constant Ocarina.ME_AADL.Entity_Category :=
        Get_Entity_Category (Entity);
      Category_Of_Former_Entity : Ocarina.ME_AADL.Entity_Category;
      Homonym_Of_Former_Entity  : Node_Id := Identifier (Former_Entity);
      Current_Former_Entity     : Node_Id;
      Result                    : Entity_Conflict_Status := No_Conflict;
   begin
      case Kind (Entity) is
         when K_Property_Association =>
            --  Property associations are different from the other
            --  declarations: they have additional statements (applies
            --  to, in binding). In addition, property associations
            --  declared without 'in mode' statement are considered as
            --  default properties and thus do not conflict with
            --  property associations associated with an 'in modes'
            --  statement. In addition, private and public property
            --  associations are associated with the declarations of
            --  their section (public or private) and thus do not
            --  conflict.

            while Present (Homonym_Of_Former_Entity) loop
               Current_Former_Entity :=
                 Corresponding_Entity (Homonym_Of_Former_Entity);
               if Kind (Current_Former_Entity) = K_Property_Association
                 and then
                   Is_Private (Entity) =
                   Is_Private (Current_Former_Entity)
                 and then Have_Common_Statements
                   (In_Modes (Entity),
                    In_Modes (Current_Former_Entity))
                 and then Have_Common_Applies_To
                   (Applies_To_Prop (Entity),
                    Applies_To_Prop (Current_Former_Entity))
                 and then Have_Common_Statements
                   (In_Binding (Entity),
                    In_Binding (Current_Former_Entity))
               then
                  Result := Conflict;
                  exit;
               end if;

               Homonym_Of_Former_Entity := Homonym (Homonym_Of_Former_Entity);
            end loop;

         when K_Flow_Implementation         |
           K_Flow_Implementation_Refinement |
           K_End_To_End_Flow_Spec           |
           K_End_To_End_Flow_Refinement     =>
            --  Flow implementations and end to end flows are also
            --  allowed to have the same name within the same
            --  namespace. But they have to be active in different
            --  modes.

            while Present (Homonym_Of_Former_Entity) loop
               Current_Former_Entity :=
                 Corresponding_Entity (Homonym_Of_Former_Entity);
               Category_Of_Former_Entity :=
                 Get_Entity_Category
                   (Corresponding_Entity (Homonym_Of_Former_Entity));

               if Category_Of_Entity = Category_Of_Former_Entity
                 and then Have_Common_Statements
                   (In_Modes (Entity),
                    In_Modes (Current_Former_Entity))
               then
                  if Is_A_Refinement (Entity)
                    and then
                      Scope_Entity (Identifier (Current_Former_Entity)) /=
                      Current_Scope
                  then
                     Result := Replacement;
                  --  There is replacement only if the new entity
                  --  is a refinement of the same category as the
                  --  former entity, and the former entity is not
                  --  part of the current scope (we assume that the
                  --  new entity is to be inserted in the current
                  --  scope).

                  else
                     Result := Conflict;
                     exit;
                  end if;
               elsif Category_Of_Entity = Category_Of_Former_Entity
                 and then not Have_Common_Statements
                   (In_Modes (Entity),
                    In_Modes (Current_Former_Entity))
               then
                  if Is_A_Refinement (Entity)
                    and then
                      Scope_Entity (Identifier (Current_Former_Entity)) /=
                      Current_Scope
                  then
                     Result := Replacement;
                  --  There is replacement only if the new entity
                  --  is a refinement of the same category as the
                  --  former entity, and the former entity is not
                  --  part of the current scope (we assume that the
                  --  new entity is to be inserted in the current
                  --  scope).
                  elsif not Is_A_Refinement (Entity)
                    and then
                      Scope_Entity (Identifier (Current_Former_Entity)) =
                      Current_Scope
                  then
                     Result := No_Conflict;
                  --  There is no conflict if two flow
                  --  implementations having different modes are
                  --  declared within the same immediate scope.
                  else
                     Result := Conflict;
                     exit;
                  end if;

               elsif
                 (Kind (Entity) = K_Flow_Implementation
                  or else Kind (Entity) = K_Flow_Implementation_Refinement)
                 and then Kind (Current_Former_Entity) = K_Flow_Spec
               then
                  if Kind (Entity) = K_Flow_Implementation then
                     --  Mark the flow as having a corresponding flow
                     --  spec.

                     Set_Corresponding_Flow_Spec
                       (Entity,
                        Current_Former_Entity);
                  end if;

                  Result := No_Conflict;
               --  A flow implementation does not conflict with the
               --  corresponding flow spec.
               else
                  Result := Conflict;
                  exit;
               end if;

               Homonym_Of_Former_Entity := Homonym (Homonym_Of_Former_Entity);
            end loop;

         when others =>
            --  For the rest of categories, redefinition is forbidden
            --  except in refinements.

            while Present (Homonym_Of_Former_Entity) loop
               Current_Former_Entity :=
                 Corresponding_Entity (Homonym_Of_Former_Entity);
               Category_Of_Former_Entity :=
                 Get_Entity_Category
                   (Corresponding_Entity (Homonym_Of_Former_Entity));

               if Is_A_Refinement (Entity)
                 and then
                   Scope_Entity (Identifier (Current_Former_Entity)) /=
                   Current_Scope
               then
                  Result := Replacement;
               --  There is replacement only if the new entity
               --  is a refinement of the same category as the
               --  former entity, and the former entity is not
               --  part of the current scope (we assume that the
               --  new entity is to be inserted in the current
               --  scope).

               else
                  Result := Conflict;
                  exit;
               end if;

               Homonym_Of_Former_Entity := Homonym (Homonym_Of_Former_Entity);
            end loop;
      end case;

      return Result;
   end Resolve_Conflict;

   ----------------------------
   -- Internal_Node_In_Scope --
   ----------------------------

   function Internal_Node_In_Scope (N : Name_Id; S : Node_Id) return Node_Id is
      I_Name : constant Name_Id := Internal_Scope_Name (N, S);
   begin
      return Node_Id (Get_Name_Table_Info (I_Name));
   end Internal_Node_In_Scope;

   ------------------
   -- Set_In_Scope --
   ------------------

   procedure Set_In_Scope (N : Node_Id; S : Node_Id) is
      I_Name : constant Name_Id := Internal_Scope_Name (Name (N), S);
   begin
      Set_Name_Table_Info (I_Name, Nat (N));
   end Set_In_Scope;

   --------------------
   -- Unset_In_Scope --
   --------------------

   procedure Unset_In_Scope (N : Node_Id; S : Node_Id) is
      I_Name : constant Name_Id := Internal_Scope_Name (Name (N), S);
   begin
      Set_Name_Table_Info (I_Name, 0);
   end Unset_In_Scope;

   -------------------------
   -- Internal_Scope_Name --
   -------------------------

   function Internal_Scope_Name (N : Name_Id; S : Node_Id) return Name_Id is
   begin
      Get_Name_String (N);
      Add_Str_To_Name_Buffer ("%in_scope%");
      Add_Nat_To_Name_Buffer (Nat (S));
      return Name_Find;
   end Internal_Scope_Name;

end Ocarina.Analyzer.AADL.Naming_Rules;
