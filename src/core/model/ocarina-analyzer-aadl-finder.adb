------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . A N A L Y Z E R . A A D L . F I N D E R          --
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

with Ocarina.Namet;

with Ocarina.Analyzer.AADL.Naming_Rules;
with Ocarina.Analyzer.Messages;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

with Ocarina.Property_Sets;

package body Ocarina.Analyzer.AADL.Finder is

   use Ocarina.Namet;
   use Ocarina.Analyzer.AADL.Naming_Rules;
   use Ocarina.Analyzer.Messages;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
   use Ocarina.Property_Sets;

   function Find_AADL_Declaration_Classifier
     (Root                   : Node_Id;
      Package_Identifier     : Node_Id;
      Declaration_Identifier : Node_Id;
      Declaration_Kinds      : Node_Kind_Array) return Node_Id;

   function Find_Subclause_Declaration_Classifier
     (Component              : Node_Id;
      Declaration_Identifier : Node_Id;
      Subclause_Kinds        : Node_Kind_Array) return Node_Id;

   function Filter_Declarations_According_To_Modes
     (Declaration_Node : Node_Id;
      In_Modes         : Node_Id) return Node_Id;
   --  Given a chained list of homonyms 'Declaration_Node',
   --
   --  * if In_Modes is not null, return the node coprresponding to
   --  the declaration that matches these modes or else the
   --  declaration that has no "in modes" clause or else No_Node.
   --  * if In_Modes is nul, return the node coprresponding to the
   --  declaration with no "in modes" clause or else No_Node.

   --------------------------------------------
   -- Filter_Declarations_According_To_Modes --
   --------------------------------------------

   function Filter_Declarations_According_To_Modes
     (Declaration_Node : Node_Id;
      In_Modes         : Node_Id) return Node_Id
   is
      Pointed_Node       : Node_Id := Declaration_Node;
      Homonym_Node       : Node_Id;
      Homonym_Identifier : Node_Id;
      Success            : Boolean;
      Was_First_Homonym  : Boolean;
      Required_Mode      : Node_Id;
      Present_Mode       : Node_Id;
      Name_Id_1          : Name_Id;
      Name_Id_2          : Name_Id;
      Name_Id_1b         : Name_Id;
      Name_Id_2b         : Name_Id;
   begin
      --  If In_Modes is (none), return No_Node

      if Present (In_Modes) and then not Have_Modes (In_Modes) then
         return No_Node;
      end if;

      Homonym_Node := Pointed_Node;

      while Present (Homonym_Node) loop
         Success           := True;
         Was_First_Homonym := (Homonym_Node = Pointed_Node);

         if Have_Modes (In_Modes) then
            if Present
                (Ocarina.ME_AADL.AADL_Tree.Nodes.In_Modes (Homonym_Node))
              and then not Have_Modes
                (Ocarina.ME_AADL.AADL_Tree.Nodes.In_Modes (Homonym_Node))
            then
               --  This means that the declarator has an 'in modes
               --  (none)' clause. This is not good for us.

               Success := False;
            elsif Have_Modes
                (Ocarina.ME_AADL.AADL_Tree.Nodes.In_Modes (Homonym_Node))
            then
               --  All the modes of

               Required_Mode := First_Node (Modes (In_Modes));
               Success       := False;

               --  For each required mode, we look for it in the
               --  in_modes statement.

               while Present (Required_Mode) loop
                  Present_Mode :=
                    First_Node
                      (Modes
                         (Ocarina.ME_AADL.AADL_Tree.Nodes.In_Modes
                            (Homonym_Node)));

                  while Present (Present_Mode) loop
                     if Kind (Present_Mode) = Kind (Required_Mode) then
                        if Kind (Required_Mode) = K_Entity_Reference then
                           Name_Id_1 :=
                             Get_Name_Of_Entity_Reference (Present_Mode);
                           Name_Id_2 :=
                             Get_Name_Of_Entity_Reference (Required_Mode);
                           Success := (Name_Id_1 = Name_Id_2) or else Success;

                        elsif Kind (Required_Mode) =
                          K_Pair_Of_Entity_References
                          and then
                            (Second_Reference (Required_Mode) /= No_Node) =
                            (Second_Reference (Present_Mode) /= No_Node)
                        then
                           if Second_Reference (Required_Mode) = No_Node then
                              Name_Id_1 :=
                                Get_Name_Of_Entity_Reference
                                  (First_Reference (Present_Mode));
                              Name_Id_2 :=
                                Get_Name_Of_Entity_Reference
                                  (First_Reference (Required_Mode));
                              Success :=
                                (Name_Id_1 = Name_Id_2) or else Success;

                           else
                              Name_Id_1 :=
                                Get_Name_Of_Entity_Reference
                                  (First_Reference (Present_Mode));
                              Name_Id_2 :=
                                Get_Name_Of_Entity_Reference
                                  (First_Reference (Required_Mode));
                              Name_Id_1b :=
                                Get_Name_Of_Entity_Reference
                                  (Second_Reference (Present_Mode));
                              Name_Id_2b :=
                                Get_Name_Of_Entity_Reference
                                  (Second_Reference (Required_Mode));

                              Success :=
                                ((Name_Id_1 = Name_Id_2)
                                 and then (Name_Id_1b = Name_Id_2b))
                                or else Success;
                           end if;
                        end if;
                     end if;

                     exit when Success;
                     Present_Mode := Next_Node (Present_Mode);
                  end loop;

                  exit when not Success;
                  Required_Mode := Next_Node (Required_Mode);
               end loop;
            end if;
         end if;

         Homonym_Identifier := Homonym (Identifier (Homonym_Node));

         if Present (Homonym_Identifier) then
            Homonym_Node := Corresponding_Entity (Homonym_Identifier);
         else
            Homonym_Node := No_Node;
         end if;

         if Was_First_Homonym and then not Success then
            Pointed_Node := Homonym_Node;
         end if;
      end loop;

      return Pointed_Node;
   end Filter_Declarations_According_To_Modes;

   --------------------------------------
   -- Find_AADL_Declaration_Classifier --
   --------------------------------------

   function Find_AADL_Declaration_Classifier
     (Root                   : Node_Id;
      Package_Identifier     : Node_Id;
      Declaration_Identifier : Node_Id;
      Declaration_Kinds      : Node_Kind_Array) return Node_Id
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert
        (No (Package_Identifier)
         or else Kind (Package_Identifier) = K_Identifier);
      pragma Assert (Kind (Declaration_Identifier) = K_Identifier);
      pragma Assert (Declaration_Kinds'Length > 0);

      Pack               : Node_Id;
      Pointed_Node       : Node_Id := No_Node;
      Homonym_Node       : Node_Id;
      Homonym_Identifier : Node_Id;
      Success            : Boolean;
      Was_First_Homonym  : Boolean;
   begin
      if Present (Package_Identifier) then
         Pack := Node_In_Scope (Package_Identifier, Entity_Scope (Root));
         Pointed_Node := No_Node;

         --  Node_In_Scope returns a node with all its homonyms. We
         --  have to look for a package in this list. Naming rules
         --  ensure there is at most one package in the list.

         while Present (Pack) and then Kind (Pack) /= K_Package_Specification
         loop
            Homonym_Identifier := Homonym (Identifier (Pack));

            if Present (Homonym_Identifier) then
               Pack := Corresponding_Entity (Homonym_Identifier);
            else
               Pack := No_Node;
            end if;
         end loop;

         --  If the package has been found, we look for the declaration

         if Present (Pack) then
            Pointed_Node :=
              Node_In_Scope (Declaration_Identifier, Entity_Scope (Pack));

            if Current_Scope /= Entity_Scope (Pack) then
               --  If the search is not done from the local package,
               --  then we must ignore the private declarations

               Homonym_Node := Pointed_Node;

               while Present (Homonym_Node) loop
                  Was_First_Homonym := (Homonym_Node = Pointed_Node);
                  Success           := not Is_Private (Homonym_Node);

                  if not Success then
                     Homonym_Identifier :=
                       Remove_From_Homonyms
                         (Identifier (Pointed_Node),
                          Identifier (Homonym_Node));
                  --  Beware: Remove_From_Homonyms only handles
                  --  identifiers.
                  else
                     Homonym_Identifier := Homonym (Identifier (Homonym_Node));
                  end if;

                  if Present (Homonym_Identifier) then
                     Homonym_Node := Corresponding_Entity (Homonym_Identifier);
                  else
                     Homonym_Node := No_Node;
                  end if;

                  if Was_First_Homonym and then not Success then
                     Pointed_Node := Homonym_Node;
                  end if;
               end loop;
            end if;
         end if;
      else
         Pointed_Node := Node_In_Scope (Declaration_Identifier, Current_Scope);
         --  Current_Scope is supposed to be the one of the package
      end if;

      --  We then filter out the node kinds we do not seek

      Homonym_Node := Pointed_Node;

      while Present (Homonym_Node) loop
         Success           := False;
         Was_First_Homonym := (Homonym_Node = Pointed_Node);

         for K in Declaration_Kinds'Range loop
            Success :=
              (Kind (Pointed_Node) = Declaration_Kinds (K)) or else Success;
         end loop;

         if not Success then
            Homonym_Identifier :=
              Remove_From_Homonyms
                (Identifier (Pointed_Node),
                 Identifier (Homonym_Node));
         --  Beware: Remove_From_Homonyms only handles identifiers.
         else
            Homonym_Identifier := Homonym (Identifier (Homonym_Node));
         end if;

         if Present (Homonym_Identifier) then
            Homonym_Node := Corresponding_Entity (Homonym_Identifier);
         else
            Homonym_Node := No_Node;
         end if;

         if Was_First_Homonym and then not Success then
            Pointed_Node := Homonym_Node;
         end if;
      end loop;

      return Pointed_Node;
   end Find_AADL_Declaration_Classifier;

   ------------------------------
   -- Find_All_Component_Types --
   ------------------------------

   function Find_All_Component_Types
     (Root      : Node_Id;
      Namespace : Node_Id := No_Node) return Node_List
   is
   begin
      return Find_All_Declarations (Root, (1 => K_Component_Type), Namespace);
   end Find_All_Component_Types;

   ---------------------------
   -- Find_All_Declarations --
   ---------------------------

   function Find_All_Declarations
     (Root      : Node_Id;
      Kinds     : Node_Kind_Array;
      Namespace : Node_Id := No_Node) return Node_List
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert
        (No (Namespace)
         or else Kind (Namespace) = K_AADL_Specification
         or else Kind (Namespace) = K_Package_Specification);

      EL        : Node_List;
      List_Node : Node_Id;
   begin
      if No (Namespace) then
         Select_Nodes
           (Ocarina.ME_AADL.AADL_Tree.Nodes.Declarations (Root),
            Kinds,
            EL.First,
            EL.Last);

         --  We first get the declarations of the unnamed namespace

         if not Is_Empty
             (Ocarina.ME_AADL.AADL_Tree.Nodes.Declarations (Root))
         then
            List_Node :=
              Ocarina.ME_AADL.AADL_Tree.Nodes.First_Node
                (Ocarina.ME_AADL.AADL_Tree.Nodes.Declarations (Root));

            while Present (List_Node) loop
               if Kind (List_Node) = K_Package_Specification then
                  --  Then those of the packages

                  Select_Nodes
                    (Ocarina.ME_AADL.AADL_Tree.Nodes.Declarations (List_Node),
                     Kinds,
                     EL.First,
                     EL.Last);
               end if;

               List_Node := Next_Node (List_Node);
            end loop;
         end if;
      else
         Select_Nodes
           (Ocarina.ME_AADL.AADL_Tree.Nodes.Declarations (Namespace),
            Kinds,
            EL.First,
            EL.Last);
      end if;

      return EL;
   end Find_All_Declarations;

   -----------------------
   -- Find_All_Features --
   -----------------------

   function Find_All_Features (AADL_Declaration : Node_Id) return Node_List is
   begin
      return Find_All_Subclauses
          (AADL_Declaration,
           (K_Port_Spec,
            K_Parameter,
            K_Feature_Group_Spec,
            K_Subcomponent_Access));
   end Find_All_Features;

   ------------------------------------
   -- Find_All_Property_Associations --
   ------------------------------------

   function Find_All_Property_Associations
     (AADL_Declaration : Node_Id) return Node_List
   is
   begin
      return Find_All_Subclauses
          (AADL_Declaration,
           (1 => K_Property_Association));
   end Find_All_Property_Associations;

   -------------------------------
   -- Find_Property_Enumeration --
   -------------------------------

   function Find_Property_Enumeration
     (Root               : Node_Id;
      Container          : Node_Id;
      Property_Container : Node_Id;
      Default_Value      : Node_Id;
      Designator         : Node_Id) return Node_Id
   is
      pragma Assert (Present (Root));
      pragma Assert (Present (Container));
      pragma Assert
        (Kind (Property_Container) = K_Property_Association
         or else Kind (Property_Container) = K_Constant_Property_Declaration
         or else Kind (Property_Container) = K_Property_Type
         or else Kind (Property_Container) = K_Property_Definition_Declaration
         or else Kind (Property_Container) = K_Property_Type_Declaration
         or else Kind (Property_Container) = K_Record_Term_Element);

      List_Node     : Node_Id := No_Node;
      Property_Type : Node_Id;
      Pointed_Node  : Node_Id;

   begin
      case Kind (Designator) is
         when K_Unique_Property_Type_Identifier =>
            Pointed_Node :=
              Find_Property_Entity
                (Root,
                 Identifier
                   (Corresponding_Entity
                      (Scope_Entity (Identifier (Entity (Designator))))),
                 Identifier (Designator));

            if Present (Pointed_Node) then
               if Kind (Pointed_Node) = K_Property_Type_Declaration then
                  Property_Type := Property_Type_Designator (Pointed_Node);
               elsif Kind (Pointed_Node) =
                 K_Property_Definition_Declaration
               then
                  Property_Type := Property_Name_Type (Pointed_Node);
               end if;

               if Kind (Property_Type) /= K_Enumeration_Type
                 and then Kind (Property_Type) /= K_Record_Type
               then
                  return No_Node;
               end if;

               if Kind (Property_Type) = K_Enumeration_Type
                 and then not Is_Empty (Identifiers (Property_Type))
               then
                  List_Node := First_Node (Identifiers (Property_Type));

                  while Present (List_Node) loop
                     if Ocarina.ME_AADL.AADL_Tree.Nodes.Name (List_Node) =
                       Name (Identifier (Default_Value))
                     then
                        Resolve_Term_In_Property
                          (Property_Container,
                           Default_Value,
                           K_Enumeration_Term);
                        return Pointed_Node;
                     end if;

                     List_Node := Next_Node (List_Node);
                  end loop;

               elsif Kind (Property_Type) = K_Record_Type then

                  --  When processing a Record_Type, we iterate over
                  --  the property container that holds the record
                  --  term element (i.e. foo => bar) and check that
                  --

                  List_Node := First_Node (List_Items (Property_Type));
                  while Present (List_Node) loop
                     --  A property type is a list of record_type element
                     --  XXX should use case insensitive match ?
                     if Ocarina.ME_AADL.AADL_Tree.Nodes.Display_Name
                       (Identifier (List_Node)) =
                       Display_Name (Identifier (Property_Container))
                     then
                        Resolve_Term_In_Property
                          (List_Node, --  Property_Container,
                           Default_Value,
                           K_Enumeration_Term);

                        return Pointed_Node;
                     end if;

                     List_Node := Next_Node (List_Node);
                  end loop;

               end if;
            end if;

         when K_Enumeration_Type =>
            if not Is_Empty (Identifiers (Designator)) then
               List_Node := First_Node (Identifiers (Designator));

               while Present (List_Node) loop
                  if Ocarina.ME_AADL.AADL_Tree.Nodes.Name (List_Node) =
                    Name (Identifier (Default_Value))
                  then
                     Resolve_Term_In_Property
                       (Property_Container,
                        Default_Value,
                        K_Enumeration_Term);

                     return Property_Container;
                  end if;

                  List_Node := Next_Node (List_Node);
               end loop;
            end if;

         when others =>
            return No_Node;
      end case;

      return No_Node;
   end Find_Property_Enumeration;

   -------------------------------------------------------
   -- Find_All_Subclause_Declarations_Except_Properties --
   -------------------------------------------------------

   function Find_All_Subclause_Declarations_Except_Properties
     (AADL_Declaration : Node_Id) return Node_List
   is
   begin
      return Find_All_Subclauses
          (AADL_Declaration,
           (K_Annex_Subclause,
            K_Port_Spec,
            K_Parameter,
            K_Feature_Group_Spec,
            K_Subcomponent_Access,
            K_Flow_Spec,
            K_Flow_Implementation,
            K_End_To_End_Flow_Spec,
            K_Flow_Implementation_Refinement,
            K_End_To_End_Flow_Refinement,
            K_Mode,
            K_Connection,
            K_Subprogram_Call,
            K_Subprogram_Call_Sequence));
   end Find_All_Subclause_Declarations_Except_Properties;

   -------------------------
   -- Find_All_Subclauses --
   -------------------------

   function Find_All_Subclauses
     (AADL_Declaration : Node_Id;
      Kinds            : Node_Kind_Array) return Node_List
   is
      pragma Assert
        (Kind (AADL_Declaration) = K_Component_Implementation
         or else Kind (AADL_Declaration) = K_Component_Type
         or else Kind (AADL_Declaration) = K_Feature_Group_Type);

      EL               : Node_List;
      List_Node        : Node_Id;
      Declaration_Node : Node_Id;
   begin
      case Kind (AADL_Declaration) is
         when K_Component_Type =>
            Declaration_Node := AADL_Declaration;

            while Present (Declaration_Node)
              and then Kind (Declaration_Node) = K_Component_Type
            loop
               Select_Nodes
                 (Annexes (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Features (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Flows (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
                    (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);

               if Present (Parent (Declaration_Node)) then
                  Declaration_Node :=
                    Corresponding_Entity
                      (Identifier (Parent (Declaration_Node)));
               else
                  Declaration_Node := No_Node;
               end if;
            end loop;

         when K_Component_Implementation =>

            --  We first scan the corresponding component type and its
            --  parents

            if Component_Type_Identifier (AADL_Declaration) /= No_Node then
               Declaration_Node :=
                 Corresponding_Entity
                   (Component_Type_Identifier (AADL_Declaration));

               while Present (Declaration_Node)
                 and then Kind (Declaration_Node) = K_Component_Type
               loop
                  Select_Nodes
                    (Annexes (Declaration_Node),
                     Kinds,
                     EL.First,
                     EL.Last);
                  Select_Nodes
                    (Features (Declaration_Node),
                     Kinds,
                     EL.First,
                     EL.Last);
                  Select_Nodes
                    (Flows (Declaration_Node),
                     Kinds,
                     EL.First,
                     EL.Last);
                  Select_Nodes
                    (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
                       (Declaration_Node),
                     Kinds,
                     EL.First,
                     EL.Last);

                  if Present (Parent (Declaration_Node)) then
                     Declaration_Node :=
                       Corresponding_Entity
                         (Identifier (Parent (Declaration_Node)));
                  else
                     Declaration_Node := No_Node;
                  end if;
               end loop;
            end if;

            --  Then we scan the component implementation and its
            --  parents

            Declaration_Node := AADL_Declaration;

            while Present (Declaration_Node)
              and then Kind (Declaration_Node) = K_Component_Implementation
            loop
               Select_Nodes
                 (Refines_Type (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Subcomponents (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Calls (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);

               if not Is_Empty (Calls (Declaration_Node)) then
                  List_Node :=
                    Ocarina.ME_AADL.AADL_Tree.Nodes.First_Node
                      (Calls (Declaration_Node));

                  while Present (List_Node) loop
                     Select_Nodes
                       (Subprogram_Calls (List_Node),
                        Kinds,
                        EL.First,
                        EL.Last);
                     List_Node := Next_Node (List_Node);
                  end loop;
               end if;

               Select_Nodes
                 (Annexes (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Connections (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Flows (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Modes (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
                    (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);

               if Present (Parent (Declaration_Node)) then
                  Declaration_Node :=
                    Corresponding_Entity
                      (Identifier (Parent (Declaration_Node)));
               else
                  Declaration_Node := No_Node;
               end if;
            end loop;

         when K_Feature_Group_Type =>
            Declaration_Node := AADL_Declaration;

            while Present (Declaration_Node)
              and then Kind (Declaration_Node) = K_Feature_Group_Type
            loop
               Select_Nodes
                 (Features (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);
               Select_Nodes
                 (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
                    (Declaration_Node),
                  Kinds,
                  EL.First,
                  EL.Last);

               if Present (Parent (Declaration_Node)) then
                  Declaration_Node :=
                    Corresponding_Entity
                      (Identifier (Parent (Declaration_Node)));
               else
                  Declaration_Node := No_Node;
               end if;
            end loop;

         when others =>
            DAE
              (Node1    => AADL_Declaration,
               Message1 => " is not an adequate AADL declaration");
            return (No_Node, No_Node);
      end case;

      return EL;
   end Find_All_Subclauses;

   ---------------------------
   -- Find_All_Root_Systems --
   ---------------------------

   function Find_All_Root_Systems (Root : Node_Id) return Node_List is

      pragma Assert (Kind (Root) = K_AADL_Specification);

      System_List       : Node_List;
      Top_Level_Systems : Node_List;
      List_Node         : Node_Id;
      Kept_Node         : Node_Id;
   begin
      System_List :=
        Find_All_Declarations
          (Root,
           (1 => K_Component_Implementation),
           No_Node);

      --  First, we only retrieve the component implementations

      List_Node := System_List.First;

      while Present (List_Node) loop
         if Component_Category'Val (Category (List_Node)) = CC_System
           and then Is_Empty
             (Features
                (Corresponding_Entity (Component_Type_Identifier (List_Node))))
         then
            --  If the system implementation corresponds to a type
            --  that does not have any feature, we keep it.

            Kept_Node := List_Node;
         else
            Kept_Node := No_Node;
         end if;

         List_Node := Next_Entity (List_Node);

         if Present (Kept_Node) then
            Set_Next_Entity (Kept_Node, Top_Level_Systems.First);

            if Top_Level_Systems.Last = No_Node then
               Top_Level_Systems.Last := Kept_Node;
            end if;

            Top_Level_Systems.First := Kept_Node;
         end if;
      end loop;

      return Top_Level_Systems;
   end Find_All_Root_Systems;

   -------------------------------
   -- Find_Component_Classifier --
   -------------------------------

   function Find_Component_Classifier
     (Root                 : Node_Id;
      Package_Identifier   : Node_Id;
      Component_Identifier : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert
        (No (Package_Identifier)
         or else Kind (Package_Identifier) = K_Identifier);
      pragma Assert (Kind (Component_Identifier) = K_Identifier);

      Pointed_Node : Node_Id;

   begin
      Pointed_Node :=
        Find_AADL_Declaration_Classifier
          (Root,
           Package_Identifier,
           Component_Identifier,
           (K_Component_Type,
            K_Component_Implementation,
            K_Alias_Declaration));

      --  In case the classifier is an alias, return the renamed entity

      if Present (Pointed_Node)
        and then Kind (Pointed_Node) = K_Alias_Declaration
      then
         Pointed_Node := Renamed_Entity (Pointed_Node);
      end if;

      return Pointed_Node;
   end Find_Component_Classifier;

   ---------------------
   -- Find_Connection --
   ---------------------

   function Find_Connection
     (Component             : Node_Id;
      Connection_Identifier : Node_Id;
      In_Modes              : Node_Id := No_Node) return Node_Id
   is
      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Kind (Connection_Identifier) = K_Identifier);

      Pointed_Node : Node_Id;
   begin
      Pointed_Node :=
        Find_Subclause_Declaration_Classifier
          (Component,
           Connection_Identifier,
           (1 => K_Connection));

      Pointed_Node :=
        Filter_Declarations_According_To_Modes (Pointed_Node, In_Modes);

      return Pointed_Node;
   end Find_Connection;

   ------------------
   -- Find_Feature --
   ------------------

   function Find_Feature
     (Component          : Node_Id;
      Feature_Identifier : Node_Id) return Node_Id
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type
         or else Kind (Component) = K_Subcomponent_Access
         or else Kind (Component) = K_Feature_Group_Type);
      pragma Assert (Kind (Feature_Identifier) = K_Identifier);

      Pointed_Node : Node_Id;
   begin
      Pointed_Node :=
        Find_Subclause_Declaration_Classifier
          (Component,
           Feature_Identifier,
           (K_Port_Spec,
            K_Parameter,
            K_Feature_Group_Spec,
            K_Subcomponent_Access,
            K_Subprogram_Spec));

      if No (Pointed_Node)
        and then Kind (Component) = K_Feature_Group_Type
        and then Present (Inverse_Of (Component))
      then
         Pointed_Node :=
           Find_Feature
             (Get_Referenced_Entity (Inverse_Of (Component)),
              Feature_Identifier);

         Pointed_Node := Inversed_Entity (Pointed_Node);
      end if;

      return Pointed_Node;
   end Find_Feature;

   --------------------
   -- Find_Flow_Spec --
   --------------------

   function Find_Flow_Spec
     (Component       : Node_Id;
      Flow_Identifier : Node_Id) return Node_Id
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type);
      pragma Assert (Kind (Flow_Identifier) = K_Identifier);
   begin
      return Find_Subclause_Declaration_Classifier
          (Component,
           Flow_Identifier,
           (1 => K_Flow_Spec));
   end Find_Flow_Spec;

   ---------------
   -- Find_Mode --
   ---------------

   function Find_Mode
     (Component       : Node_Id;
      Mode_Identifier : Node_Id) return Node_Id
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type
         or else Kind (Component) = K_Feature_Group_Type);
      pragma Assert (Kind (Mode_Identifier) = K_Identifier);
   begin
      return Find_Subclause_Declaration_Classifier
          (Component,
           Mode_Identifier,
           (1 => K_Mode));
   end Find_Mode;

   --------------------------------
   -- Find_Port_Group_Classifier --
   --------------------------------

   function Find_Port_Group_Classifier
     (Root                  : Node_Id;
      Package_Identifier    : Node_Id;
      Port_Group_Identifier : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert
        (No (Package_Identifier)
         or else Kind (Package_Identifier) = K_Identifier);
      pragma Assert (Kind (Port_Group_Identifier) = K_Identifier);
   begin
      return Find_AADL_Declaration_Classifier
          (Root,
           Package_Identifier,
           Port_Group_Identifier,
           (1 => K_Feature_Group_Type));
   end Find_Port_Group_Classifier;

   -------------------------------
   -- Find_Property_Association --
   -------------------------------

   function Find_Property_Association
     (AADL_Declaration          : Node_Id;
      Property_Association_Name : Name_Id) return Node_Id
   is
      pragma Assert (Present (AADL_Declaration));

      All_Properties : constant Node_List :=
        Find_All_Property_Associations (AADL_Declaration);
      List_Node : Node_Id;
   begin
      if All_Properties.First /= No_Node then
         List_Node := All_Properties.First;

         while Present (List_Node) loop
            if Name (Identifier (List_Node)) = Property_Association_Name then
               return List_Node;
            end if;

            List_Node := Next_Entity (List_Node);
         end loop;
      end if;

      return No_Node;
   end Find_Property_Association;

   --------------------------
   -- Find_Property_Entity --
   --------------------------

   function Find_Property_Entity
     (Root                    : Node_Id;
      Property_Set_Identifier : Node_Id;
      Property_Identifier     : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert
        (No (Property_Set_Identifier)
         or else Kind (Property_Set_Identifier) = K_Identifier);
      pragma Assert (Kind (Property_Identifier) = K_Identifier);

      Property_Set               : Node_Id;
      Found_Property_Declaration : Node_Id := No_Node;
   begin
      --  Per strict conformance to AADL legality rules, all property
      --  entities should be either fully qualified, or part of
      --  pre-declared property sets.

      if Present (Property_Set_Identifier) then
         Property_Set :=
           Node_In_Scope (Property_Set_Identifier, Entity_Scope (Root));

         --  If we found the corresponding property set, then we look
         --  for the property in it.

         if Present (Property_Set) then
            Found_Property_Declaration :=
              Node_In_Scope (Property_Identifier, Entity_Scope (Property_Set));
         else
            Found_Property_Declaration := No_Node;
         end if;

      else
         --  If we did not find anything so far, we try the implicit
         --  property sets.

         for S in Standard_Property_Set_Type'Range loop
            Set_Str_To_Name_Buffer (Image (S));
            Property_Set := Node_In_Scope (Name_Find, Entity_Scope (Root));

            if Present (Property_Set) then
               Found_Property_Declaration :=
                 Node_In_Scope
                   (Property_Identifier,
                    Entity_Scope (Property_Set));
            end if;

            exit when Present (Found_Property_Declaration);
         end loop;
      end if;

      return Found_Property_Declaration;
   end Find_Property_Entity;

   --------------------
   -- Find_Subclause --
   --------------------

   function Find_Subclause
     (Component  : Node_Id;
      Identifier : Node_Id) return Node_Id
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type
         or else Kind (Component) = K_Port_Spec);
      pragma Assert (Kind (Identifier) = K_Identifier);
   begin
      return Find_Subclause_Declaration_Classifier
          (Component,
           Identifier,
           (K_Flow_Spec,
            K_Flow_Implementation,
            K_Flow_Implementation_Refinement,
            K_End_To_End_Flow_Spec,
            K_End_To_End_Flow_Refinement,
            K_Connection,
            K_Subcomponent,
            K_Port_Spec,
            K_Parameter,
            K_Feature_Group_Spec,
            K_Subcomponent_Access,
            K_Subprogram_Spec,
            K_Mode,
            K_Subprogram_Call));
   end Find_Subclause;

   -------------------------------------------
   -- Find_Subclause_Declaration_Classifier --
   -------------------------------------------

   function Find_Subclause_Declaration_Classifier
     (Component              : Node_Id;
      Declaration_Identifier : Node_Id;
      Subclause_Kinds        : Node_Kind_Array) return Node_Id
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type
         or else Kind (Component) = K_Feature_Group_Type
         or else Kind (Component) = K_Port_spec
         or else Kind (Component) = K_Subcomponent_Access);
      pragma Assert (Kind (Declaration_Identifier) = K_Identifier);
      pragma Assert (Subclause_Kinds'Length > 0);

      Pointed_Node : Node_Id := No_Node;
   begin
      if Kind (Component) = K_Subcomponent_Access
        and then
          Component_Category'Val (Subcomponent_Category (Component)) =
          CC_Subprogram
      then
         Pointed_Node :=
           Node_In_Scope
             (Declaration_Identifier,
              Entity_Scope (Get_Referenced_Entity (Entity_Ref (Component))));
      else
         Pointed_Node :=
           Node_In_Scope (Declaration_Identifier, Entity_Scope (Component));
      end if;

      return Pointed_Node;
   end Find_Subclause_Declaration_Classifier;

   -----------------------
   -- Find_Subcomponent --
   -----------------------

   function Find_Subcomponent
     (Component               : Node_Id;
      Subcomponent_Identifier : Node_Id;
      In_Modes                : Node_Id := No_Node) return Node_Id
   is
      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Kind (Subcomponent_Identifier) = K_Identifier);

      Pointed_Node : Node_Id;
   begin
      Pointed_Node :=
        Find_Subclause_Declaration_Classifier
          (Component,
           Subcomponent_Identifier,
           (1 => K_Subcomponent));

      Pointed_Node :=
        Filter_Declarations_According_To_Modes (Pointed_Node, In_Modes);

      return Pointed_Node;
   end Find_Subcomponent;

   --------------------
   -- Find_Prototype --
   --------------------

   function Find_Prototype
     (Component            : Node_Id;
      Prototype_Identifier : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Component) = K_Component_Type);
      pragma Assert (Kind (Prototype_Identifier) = K_Identifier);

      Pointed_Node : Node_Id;
   begin
      Pointed_Node :=
        Find_Subclause_Declaration_Classifier
          (Component,
           Prototype_Identifier,
           (1 => K_Prototype));

      return Pointed_Node;
   end Find_Prototype;

   --------------------------
   -- Find_Subprogram_Call --
   --------------------------

   function Find_Subprogram_Call
     (Component       : Node_Id;
      Call_Identifier : Node_Id;
      In_Modes        : Node_Id := No_Node) return Node_Id
   is
      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Kind (Call_Identifier) = K_Identifier);

      Pointed_Node : Node_Id;
   begin
      Pointed_Node :=
        Find_Subclause_Declaration_Classifier
          (Component,
           Call_Identifier,
           (1 => K_Subprogram_Call));

      Pointed_Node :=
        Filter_Declarations_According_To_Modes (Pointed_Node, In_Modes);

      return Pointed_Node;
   end Find_Subprogram_Call;

   --------------------------------
   -- Find_In_Import_Declaration --
   --------------------------------

   function Find_In_Import_Declaration
     (Package_Container : Node_Id;
      Node              : Node_Id) return Boolean
   is
      Identifier           : Node_Id := No_Node;
      Pack_Identifier      : Node_Id;
      Import_Node          : Node_Id;
      List_Node            : Node_Id;
      Name_Visibility_Node : Node_Id;
      Success              : Boolean := False;
   begin
      if Kind (Node) = K_Identifier then
         Identifier := Node;
      elsif Kind (Node) = K_Entity_Reference then
         Identifier := Namespace_Identifier (Node);
      end if;

      if Present (Package_Container)
        and then not Is_Empty (Declarations (Package_Container))
        and then
          (Kind (First_Node (Declarations (Package_Container)))) =
          K_Name_Visibility_Declaration
      then
         Name_Visibility_Node := First_Node (Declarations (Package_Container));

         if not Is_Empty (List_Items (Name_Visibility_Node)) then
            List_Node := First_Node (List_Items (Name_Visibility_Node));
         end if;

         --  XXX fixme : when pack2 'with' pack1, pack1 identifier must
         --  be add to the scope of pack2 ?

         while Present (List_Node) loop
            if Kind (List_Node) = K_Import_Declaration
              and then not Is_Empty (List_Items (List_Node))
            then
               Import_Node := First_Node (List_Items (List_Node));
            end if;

            while Present (Import_Node) loop
               if Kind (Import_Node) = K_Package_Name then
                  Pack_Identifier := Build_Package_Identifier (Import_Node);

                  if Name (Pack_Identifier) = Name (Identifier) then
                     Success := True;
                  end if;
               else
                  if Name (Import_Node) = Name (Identifier) then
                     Success := True;
                  end if;
               end if;

               Import_Node := Next_Node (Import_Node);
            end loop;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Find_In_Import_Declaration;

   ------------------
   -- Select_Nodes --
   ------------------

   procedure Select_Nodes
     (Decl_List  :        List_Id;
      Kinds      :        Node_Kind_Array;
      First_Node : in out Node_Id;
      Last_Node  : in out Node_Id)
   is
      Success         : Boolean;
      Local_List_Node : Node_Id;
   begin
      if not Is_Empty (Decl_List) then
         Local_List_Node :=
           Ocarina.ME_AADL.AADL_Tree.Nodes.First_Node (Decl_List);

         while Present (Local_List_Node) loop
            Success := False;

            for K in Kinds'Range loop
               Success := Success or else (Kind (Local_List_Node) = Kinds (K));
            end loop;

            if Success then
               if No (First_Node) then
                  First_Node := Local_List_Node;
                  Last_Node  := Local_List_Node;
               else
                  Set_Next_Entity (Last_Node, Local_List_Node);
                  Set_Next_Entity (Local_List_Node, No_Node);
                  Last_Node := Local_List_Node;
               end if;
            end if;

            Local_List_Node := Next_Node (Local_List_Node);
         end loop;
      end if;
   end Select_Nodes;

end Ocarina.Analyzer.AADL.Finder;
