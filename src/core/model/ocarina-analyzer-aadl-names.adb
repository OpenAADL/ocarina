------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . A N A L Y Z E R . A A D L . N A M E S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

with Ocarina.Analyzer.Messages;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.Analyzer.AADL.Naming_Rules;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;

package body Ocarina.Analyzer.AADL.Names is

   use Ocarina.Analyzer.AADL.Finder;
   use Ocarina.Analyzer.Messages;
   use Ocarina.Analyzer.AADL.Naming_Rules;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Entities;

   function Check_Names_In_Package (Node : Node_Id) return Boolean;
   function Check_Names_In_Property_Set (Node : Node_Id) return Boolean;

   function Check_Declaration_Names_In_Component_Type
     (Node : Node_Id) return Boolean;
   --  Check the declarations inside a component type, including all
   --  its parents (in case of an extension).

   function Check_Declaration_Names_In_Component_Implementation
     (Node : Node_Id) return Boolean;
   --  Check the declarations inside a component implementation,
   --  including all its parents (in case of an extension) and
   --  corresponding type.

   function Check_Declaration_Names_In_Feature_Group_Type
     (Node : Node_Id) return Boolean;
   --  Check the declarations inside a port group type (AADL_V1) or
   --  inside a feature group type (AADL_V2), including all
   --  its parents (in case of an extension).

   function Check_Names_In_Component_Type (Node : Node_Id) return Boolean;
   --  Same as above but with a component type

   function Check_Names_In_Component_Implementation
     (Node : Node_Id) return Boolean;
   --  Same as above but with a component type

   function Check_Names_In_Feature_Group (Node : Node_Id) return Boolean;
   --  Same as above but with a component type

   function Check_Property_Association_Names (Node : Node_Id) return Boolean;

   function Check_Import_Declaration
     (Name_Visibility_Node : Node_Id;
      Package_Node         : Node_Id) return Boolean;
   --  Check if package or property in with sublcause exists in global scope

   ------------------------------
   -- Check_Import_Declaration --
   ------------------------------

   function Check_Import_Declaration
     (Name_Visibility_Node : Node_Id;
      Package_Node         : Node_Id) return Boolean
   is
      pragma Assert
        (Kind (Name_Visibility_Node) = K_Name_Visibility_Declaration);

      Identifier  : Node_Id;
      In_Node     : Node_Id;
      List_Node   : Node_Id;
      Import_Node : Node_Id;
      Success     : Boolean := True;
   begin
      Pop_Scope;

      if not Is_Empty (List_Items (Name_Visibility_Node)) then
         List_Node := First_Node (List_Items (Name_Visibility_Node));
      end if;

      while Present (List_Node) loop
         case Kind (List_Node) is
            when K_Import_Declaration =>
               if not Is_Empty (List_Items (List_Node)) then
                  Import_Node := First_Node (List_Items (List_Node));
               end if;

               while Present (Import_Node) loop
                  if Kind (Import_Node) = K_Package_Name then
                     Identifier := Build_Package_Identifier (Import_Node);

                     In_Node :=
                       Node_Explicitly_In_Scope (Identifier, Current_Scope);

                  else
                     In_Node :=
                       Node_Explicitly_In_Scope (Import_Node, Current_Scope);
                  end if;

                  if In_Node = No_Node then
                     Success := False;

                     Display_Analyzer_Error
                       (Import_Node,
                        "is not a package or a property set visible" &
                        " or existing");
                  end if;
                  Import_Node := Next_Node (Import_Node);
               end loop;

            when K_Alias_Declaration =>
               declare
                  Alias : Node_Id := No_Node;
               begin
                  if Present (Reference (List_Node)) then
                     --  Alias references an entity type
                     Alias := Full_Identifier (Reference (List_Node));

                  elsif Present (Package_Name (List_Node)) then

                     --  Alias references a package
                     Alias :=
                       Build_Package_Identifier (Package_Name (List_Node));
                  end if;
                  pragma Assert (Present (Alias));

                  if Name (Alias) =
                    Name
                      (Build_Package_Identifier (Package_Name (Package_Node)))
                    and then Is_All (List_Node)
                  then
                     Display_Analyzer_Error
                       (Alias,
                        "alias definition refers to self package",
                        Loc => Loc (Name_Visibility_Node));
                     Success := False;

                  elsif Present
                      (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifier (List_Node))
                    and then
                      Name
                        (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifier
                           (List_Node)) =
                      Name
                        (Build_Package_Identifier
                           (Package_Name (Package_Node)))
                  then
                     Success := False;
                     Display_Analyzer_Error
                       (List_Node,
                        "name conflicts with current package name",
                        Loc => Loc (List_Node));

                  else
                     In_Node := Node_In_Scope (Alias, Current_Scope);

                     if No (In_Node)
                       and then Present (Corresponding_Entity (Alias))
                     then
                        In_Node :=
                          Find_Component_Classifier
                            (Root               => 1,
                             Package_Identifier =>
                               Namespace_Identifier
                                 (Corresponding_Entity (Alias)),
                             Component_Identifier =>
                               Ocarina.ME_AADL.AADL_Tree.Nodes.Identifier
                                 (Corresponding_Entity (Alias)));
                     end if;

                     if Present
                         (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifier
                            (List_Node))
                     then
                        Success :=
                          Enter_Name_In_Scope
                            (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifier
                               (List_Node));
                     end if;

                     if No (In_Node) then
                        Success := False;
                        Display_Analyzer_Error (Alias, "is not visible");

                     else
                        Set_Renamed_Entity (List_Node, In_Node);

                     end if;
                  end if;
               end;
            when others =>
               null;
         end case;
         exit when not Success;

         List_Node := Next_Node (List_Node);
      end loop;

      Push_Scope (Entity_Scope (Package_Node));
      return Success;
   end Check_Import_Declaration;

   ---------------------------------------------------------
   -- Check_Declaration_Names_In_Component_Implementation --
   ---------------------------------------------------------

   function Check_Declaration_Names_In_Component_Implementation
     (Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Node) = K_Component_Implementation);

      List_Node      : Node_Id;
      Call_List_Node : Node_Id;
      Success        : Boolean := True;
   begin
      if Parent (Node) /= No_Node
        and then (Get_Referenced_Entity (Parent (Node))) /= No_Node
      then
         --  First check the declarations of the parent component
         Success :=
           Check_Declaration_Names_In_Component_Implementation
             (Get_Referenced_Entity (Parent (Node)));
      else
         --  But before all, we include the features of the component
         --  type into the scope

         Success :=
           Check_Declaration_Names_In_Component_Type
             (Corresponding_Entity (Component_Type_Identifier (Node)));
      end if;

      Push_Scope (Entity_Scope (Node));

      --  Type refinements

      if not Is_Empty (Refines_Type (Node)) then
         List_Node := First_Node (Refines_Type (Node));

         while Present (List_Node) loop
            Success :=
              Enter_Name_In_Scope (Identifier (List_Node))
              and then Check_Property_Association_Names (List_Node)
              and then Success;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Subcomponents

      if not Is_Empty (Subcomponents (Node)) then
         List_Node := First_Node (Subcomponents (Node));

         while Present (List_Node) loop
            Success :=
              Enter_Name_In_Scope (Identifier (List_Node))
              and then Check_Property_Association_Names (List_Node)
              and then Success;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Call sequences
      --  Some call sequences are anonymous

      if not Is_Empty (Calls (Node)) then
         List_Node := First_Node (Calls (Node));

         while Present (List_Node) loop
            if Identifier (List_Node) /= No_Node then
               Success :=
                 Enter_Name_In_Scope (Identifier (List_Node)) and then Success;
            end if;

            if not Is_Empty (Subprogram_Calls (List_Node)) then
               Call_List_Node := First_Node (Subprogram_Calls (List_Node));

               --  Subprogram call names are put in the same scope as
               --  all the other names. All the call names must be in
               --  the same scope, since connections directly refer to
               --  call names, without specifying sequence names.

               while Present (Call_List_Node) loop
                  Set_In_Modes (Call_List_Node, In_Modes (List_Node));
                  --  We set implicit in modes statement for the
                  --  subprogram call. It is the one the call
                  --  sequence.

                  Success :=
                    Enter_Name_In_Scope (Identifier (Call_List_Node))
                    and then Check_Property_Association_Names (Call_List_Node)
                    and then Success;
                  Call_List_Node := Next_Node (Call_List_Node);
               end loop;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Connections
      --  Some connections are anonymous

      if not Is_Empty (Connections (Node)) then
         List_Node := First_Node (Connections (Node));

         while Present (List_Node) loop
            if Identifier (List_Node) /= No_Node then
               Success :=
                 Enter_Name_In_Scope (Identifier (List_Node))
                 and then Check_Property_Association_Names (List_Node)
                 and then Success;
            else
               Success :=
                 Check_Property_Association_Names (List_Node) and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Flows

      if not Is_Empty (Flows (Node)) then
         List_Node := First_Node (Flows (Node));

         while Present (List_Node) loop
            Success :=
              Enter_Name_In_Scope (Identifier (List_Node))
              and then Check_Property_Association_Names (List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Modes

      if not Is_Empty (Modes (Node)) then
         List_Node := First_Node (Modes (Node));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Mode then
               Success :=
                 Enter_Name_In_Scope (Identifier (List_Node))
                 and then Check_Property_Association_Names (List_Node)
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Check_Declaration_Names_In_Component_Implementation;

   -----------------------------------------------
   -- Check_Declaration_Names_In_Component_Type --
   -----------------------------------------------

   function Check_Declaration_Names_In_Component_Type
     (Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Node) = K_Component_Type);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      --  First check the declarations of the parent component

      if Parent (Node) /= No_Node
        and then (Get_Referenced_Entity (Parent (Node))) /= No_Node
      then
         Success :=
           Check_Declaration_Names_In_Component_Type
             (Get_Referenced_Entity (Parent (Node)));
      end if;

      Push_Scope (Entity_Scope (Node));

      --  Prototypes

      if not Is_Empty (Prototypes (Node)) then
         List_Node := First_Node (Prototypes (Node));

         while Present (List_Node) loop
            Success :=
              Enter_Name_In_Scope (Identifier (List_Node))
            --  and then Check_Property_Association_Names (List_Node)
             and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Features

      if not Is_Empty (Features (Node)) then
         List_Node := First_Node (Features (Node));

         while Present (List_Node) loop
            Success :=
              Enter_Name_In_Scope (Identifier (List_Node))
              and then Check_Property_Association_Names (List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Flows

      if not Is_Empty (Flows (Node)) then
         List_Node := First_Node (Flows (Node));

         while Present (List_Node) loop
            Success :=
              Enter_Name_In_Scope (Identifier (List_Node))
              and then Check_Property_Association_Names (List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Check_Declaration_Names_In_Component_Type;

   ---------------------------------------------------
   -- Check_Declaration_Names_In_Feature_Group_Type --
   ---------------------------------------------------

   function Check_Declaration_Names_In_Feature_Group_Type
     (Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Node) = K_Feature_Group_Type);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      --  First check the declarations of the parent port group

      if Parent (Node) /= No_Node
        and then Get_Referenced_Entity (Parent (Node)) /= No_Node
      then
         Success :=
           Check_Declaration_Names_In_Feature_Group_Type
             (Get_Referenced_Entity (Parent (Node)));
      end if;

      Push_Scope (Entity_Scope (Node));

      if not Is_Empty (Features (Node)) then
         List_Node := First_Node (Features (Node));

         while Present (List_Node) loop
            if not Is_Implicit_Inverse (List_Node) then
               Success :=
                 Enter_Name_In_Scope (Identifier (List_Node))
                 and then Check_Property_Association_Names (List_Node)
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Check_Declaration_Names_In_Feature_Group_Type;

   --------------------------------------------------
   -- Check_Names_In_Components_And_Feature_Groups --
   --------------------------------------------------

   function Check_Names_In_Components_And_Feature_Groups
     (Root : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      List_Node         : Node_Id;
      Package_List_Node : Node_Id;
      Success           : Boolean := True;
   begin
      Push_Scope (Entity_Scope (Root));

      if not Is_Empty (Declarations (Root)) then
         List_Node := First_Node (Declarations (Root));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Component_Type =>
                  Success :=
                    Check_Names_In_Component_Type (List_Node) and then Success;

               when K_Component_Implementation =>
                  Success :=
                    Check_Names_In_Component_Implementation (List_Node)
                    and then Success;

               when K_Feature_Group_Type =>
                  Success :=
                    Check_Names_In_Feature_Group (List_Node) and then Success;

               when K_Package_Specification =>
                  Push_Scope (Entity_Scope (List_Node));

                  if not Is_Empty (Declarations (List_Node)) then
                     Package_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Present (Package_List_Node) loop
                        case Kind (Package_List_Node) is
                           when K_Component_Type =>
                              Success :=
                                Check_Names_In_Component_Type
                                  (Package_List_Node)
                                and then Success;

                           when K_Component_Implementation =>
                              Success :=
                                Check_Names_In_Component_Implementation
                                  (Package_List_Node)
                                and then Success;

                           when K_Feature_Group_Type =>
                              Success :=
                                Check_Names_In_Feature_Group
                                  (Package_List_Node)
                                and then Success;

                           when others =>
                              null;
                        end case;

                        Package_List_Node := Next_Node (Package_List_Node);
                     end loop;
                  end if;

                  Pop_Scope;

               when others =>
                  null;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Check_Names_In_Components_And_Feature_Groups;

   ---------------------------------------------
   -- Check_Names_In_Component_Implementation --
   ---------------------------------------------

   function Check_Names_In_Component_Implementation
     (Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Node) = K_Component_Implementation);

      Success : Boolean := True;
   begin
      Success := Check_Declaration_Names_In_Component_Implementation (Node);
      Success := Check_Property_Association_Names (Node) and then Success;

      return Success;
   end Check_Names_In_Component_Implementation;

   -----------------------------------
   -- Check_Names_In_Component_Type --
   -----------------------------------

   function Check_Names_In_Component_Type (Node : Node_Id) return Boolean is
      pragma Assert (Kind (Node) = K_Component_Type);

      Success : Boolean := True;
   begin
      Success := Check_Declaration_Names_In_Component_Type (Node);
      Success := Check_Property_Association_Names (Node) and then Success;

      return Success;
   end Check_Names_In_Component_Type;

   -------------------------------
   -- Check_Names_In_Namespaces --
   -------------------------------

   function Check_Names_In_Namespaces (Node : Node_Id) return Boolean is
      pragma Assert (Kind (Node) = K_AADL_Specification);

      Success               : Boolean := True;
      List_Node             : Node_Id;
      Node_Entered_In_Scope : Node_Id;
   begin
      if not Is_Empty (Declarations (Node)) then
         Push_Scope (Entity_Scope (Node));

         --  First, we check the names of the AADL specification

         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            Node_Entered_In_Scope :=
              Enter_Name_In_Scope (Identifier (List_Node));

            if Node_Entered_In_Scope = No_Node then
               Success := False;

            elsif Kind (List_Node) = K_Package_Specification
              and then Node_Entered_In_Scope /= List_Node
            then
               --  If the package was already declared elsewhere,
               --  Enter_Name_In_Scope merges the two package and
               --  returns the former Node_Id. Hence we have to remove
               --  the node from the list.

               Remove_Node_From_List (List_Node, Declarations (Node));
            end if;

            List_Node := Next_Node (List_Node);
         end loop;

         --  Then, we check the names inside packages.

         --  Note: we could not do it in the same time as for the
         --  names of the AADL specification. Indeed, packages are
         --  declared in two times, we would check the names inside
         --  the first part of the package, and when we find the
         --  second part of the package we would have to clear the
         --  scopes of the resulting merged package and then check the
         --  names again in the whole package (thus checking some
         --  declarations twice).

         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Package_Specification then
               Success := Check_Names_In_Package (List_Node) and then Success;

            elsif Kind (List_Node) = K_Property_Set then
               Success :=
                 Check_Names_In_Property_Set (List_Node) and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;

         Pop_Scope;
      end if;

      return Success;
   end Check_Names_In_Namespaces;

   ----------------------------
   -- Check_Names_In_Package --
   ----------------------------

   function Check_Names_In_Package (Node : Node_Id) return Boolean is
      pragma Assert (Kind (Node) = K_Package_Specification);

      Success   : Boolean := True;
      List_Node : Node_Id;
   begin
      if not Is_Empty (Declarations (Node)) then
         Push_Scope (Entity_Scope (Node));
         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Name_Visibility_Declaration then
               Success := Check_Import_Declaration (List_Node, Node);
            else
               if not Enter_Name_In_Scope (Identifier (List_Node)) then
                  Success := False;
               end if;

            end if;
            exit when not Success;

            List_Node := Next_Node (List_Node);
         end loop;

         Pop_Scope;
      end if;

      if not Is_Empty (Properties (Node)) then
         Push_Scope (Property_Scope (Node));
         List_Node := First_Node (Properties (Node));

         while Present (List_Node) loop
            if not Enter_Name_In_Scope (Identifier (List_Node)) then
               Success := False;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;

         Pop_Scope;
      end if;

      return Success;
   end Check_Names_In_Package;

   ----------------------------------
   -- Check_Names_In_Feature_Group --
   ----------------------------------

   function Check_Names_In_Feature_Group (Node : Node_Id) return Boolean is

      pragma Assert (Kind (Node) = K_Feature_Group_Type);

      Success : Boolean := True;
   begin
      Success := Check_Declaration_Names_In_Feature_Group_Type (Node);

      Success := Check_Property_Association_Names (Node) and then Success;

      return Success;
   end Check_Names_In_Feature_Group;

   ---------------------------------
   -- Check_Names_In_Property_Set --
   ---------------------------------

   function Check_Names_In_Property_Set (Node : Node_Id) return Boolean is

      pragma Assert (Kind (Node) = K_Property_Set);

      Success   : Boolean := True;
      List_Node : Node_Id;
   begin
      if not Is_Empty (Declarations (Node)) then
         Push_Scope (Entity_Scope (Node));
         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            if not Enter_Name_In_Scope (Identifier (List_Node)) then
               Success := False;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;

         Pop_Scope;
      end if;

      return Success;
   end Check_Names_In_Property_Set;

   --------------------------------------
   -- Check_Property_Association_Names --
   --------------------------------------

   function Check_Property_Association_Names (Node : Node_Id) return Boolean is
      pragma Assert
        (Kind (Node) = K_Component_Implementation
         or else Kind (Node) = K_Component_Type
         or else Kind (Node) = K_Port_Spec
         or else Kind (Node) = K_Feature_Group_Spec
         or else Kind (Node) = K_Feature_Group_Type
         or else Kind (Node) = K_Subprogram_Spec
         or else Kind (Node) = K_Parameter
         or else Kind (Node) = K_Subcomponent
         or else Kind (Node) = K_Subcomponent_Access
         or else Kind (Node) = K_Flow_Spec
         or else Kind (Node) = K_Mode
         or else Kind (Node) = K_Flow_Implementation
         or else Kind (Node) = K_End_To_End_Flow_Spec
         or else Kind (Node) = K_Flow_Implementation_Refinement
         or else Kind (Node) = K_End_To_End_Flow_Refinement
         or else Kind (Node) = K_Subprogram_Call
         or else Kind (Node) = K_Connection);

      Success   : Boolean := True;
      List_Node : Node_Id;
   begin
      if not Is_Empty (Properties (Node)) then
         Push_Scope (Property_Scope (Node));
         List_Node := First_Node (Properties (Node));

         while Present (List_Node) loop
            Success :=
              Enter_Name_In_Scope (Identifier (List_Node)) and then Success;
            List_Node := Next_Node (List_Node);
         end loop;

         Pop_Scope;
      end if;

      return Success;
   end Check_Property_Association_Names;

end Ocarina.Analyzer.AADL.Names;
