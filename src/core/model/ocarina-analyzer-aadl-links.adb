------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . A N A L Y Z E R . A A D L . L I N K S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2020 ESA & ISAE.        --
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

with Utils;

with Ocarina.Analyzer.Messages;
with Ocarina.Analyzer.AADL.Semantics;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.Analyzer.AADL.Naming_Rules;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

package body Ocarina.Analyzer.AADL.Links is

   use Utils;
   use Ocarina.Analyzer.Messages;
   use Ocarina.Analyzer.AADL.Finder;
   use Ocarina.Analyzer.AADL.Naming_Rules;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

   function Link_Declarations_Of_Package
     (Root : Node_Id;
      Node : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the
   --  declarations of an AADL package.

   function Link_Declarations_Of_Property_Set
     (Root : Node_Id;
      Node : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the
   --  declarations of an AADL property set.

   function Link_Component_Implementation_Subclauses
     (Root : Node_Id;
      Node : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the
   --  subclauses (call sequences, subcomponents...) of a component
   --  implementation.

   function Link_Component_Type_Subclauses
     (Root : Node_Id;
      Node : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the
   --  subclauses (features...) of a component type.

   function Link_Feature_Group_Type_Subclauses
     (Root : Node_Id;
      Node : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the
   --  subclauses (features...) of a port group (AADL_V1) or
   --  a feature group (AADL_V2)

   function Link_Properties_Of_Component_Type
     (Root : Node_Id;
      Node : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the property
   --  associations of a component type.

   function Link_Properties_Of_Component_Implementation
     (Root : Node_Id;
      Node : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the property
   --  associations of a component implementation.

   function Link_Properties_Of_Feature_Group_Type
     (Root : Node_Id;
      Node : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the property
   --  associations of a port group (AADL_V1) or a feature group (AADL_V2)

   function Link_Property_Value
     (Root               : Node_Id;
      Container          : Node_Id;
      Property_Container : Node_Id;
      Node               : Node_Id;
      Property_Type      : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the property
   --  value 'Node'.

   function Link_Type_Designator
     (Root       : Node_Id;
      Designator : Node_Id) return Boolean;
   --  Perform the designator and identifier link of a property type.

   function Link_Properties_Of_Package
     (Root : Node_Id;
      Node : Node_Id) return Boolean;
   --  Perform all the designator and identifier links in the property
   --  associations of an AADL package.

   procedure Retrieve_Connection_End
     (Component          :     Node_Id;
      Connection_End     :     Node_Id;
      Corresponding_Node : out Node_Id;
      Is_Local           : out Boolean);
   --  Find the node corresponding to the end of a connection

   function Link_Flow_Feature
     (Feature_Identifier : Node_Id;
      Component          : Node_Id) return Node_Id;
   --  Return the feature instance having the identifier
   --  'Feature_Identifier' in the component 'Component'. Perform all
   --  the necessary links between the reference and the found
   --  entities.

   function Link_Flow_Of_Subcomponent
     (Flow_Identifier : Node_Id;
      Component       : Node_Id;
      In_Modes        : Node_Id := No_Node) return Node_Id;
   --  Return the flow instance having the identifier
   --  'Flow_Identifier' in the component 'Component' in the modes
   --  'In_Modes'. This function performs all the necessary links
   --  between the reference and the found entities.

   function Link_Flow_Connections (Flow : Node_Id) return Boolean;
   --  Performs links and checks on the flow connection list.

   function Equals (Unit_Id_1 : Node_Id; Unit_Id_2 : Node_Id) return Boolean;
   --  Return True when the two identifiers have the same name. This
   --  function is *not* case sensitive.

   function Unwind_Units_Type
     (Root          : Node_Id;
      Property_Type : Node_Id) return Node_Id;
   --  Return the units type declaration corresponding to the given
   --  property type. If the type definition does not contain any unit
   --  definition, then return No_Bode.

   ------------
   -- Equals --
   ------------

   function Equals (Unit_Id_1 : Node_Id; Unit_Id_2 : Node_Id) return Boolean is
   begin
      return To_Lower (Name (Unit_Id_1)) = To_Lower (Name (Unit_Id_2));
   end Equals;

   -------------------------------
   -- Link_Flow_Of_Subcomponent --
   -------------------------------

   function Link_Flow_Of_Subcomponent
     (Flow_Identifier : Node_Id;
      Component       : Node_Id;
      In_Modes        : Node_Id := No_Node) return Node_Id
   is
      pragma Assert (Kind (Flow_Identifier) = K_Entity_Reference);
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type);

      Pointed_Node      : Node_Id;
      Pointed_Component : Node_Id;
   begin
      --  The entity reference must be in the form "a.b", otherwise it
      --  cannot be a path of a subcomponent.

      if Length (Path (Flow_Identifier)) /= 2 then
         return No_Node;
      end if;

      --  Fetch "a" and link it

      Pointed_Node :=
        Find_Subcomponent
          (Component               => Component,
           Subcomponent_Identifier =>
             Item (First_Node (Path (Flow_Identifier))),
           In_Modes => In_Modes);

      Set_Corresponding_Entity
        (Item (First_Node (Path (Flow_Identifier))),
         Pointed_Node);

      Display_Node_Link
        (Item (First_Node (Path (Flow_Identifier))),
         Pointed_Node);

      if Present (Pointed_Node) then
         --  Fetch "b" and link it

         Pointed_Component :=
           Get_Referenced_Entity (Entity_Ref (Pointed_Node));

         Pointed_Node :=
           Find_Flow_Spec
             (Pointed_Component,
              Item (Next_Node (First_Node (Path (Flow_Identifier)))));

         Set_Corresponding_Entity
           (Item (Next_Node (First_Node (Path (Flow_Identifier)))),
            Pointed_Node);

         Set_Referenced_Entity (Flow_Identifier, Pointed_Node);

         Display_Node_Link
           (Item (Next_Node (First_Node (Path (Flow_Identifier)))),
            Pointed_Node);
      end if;

      return Pointed_Node;
   end Link_Flow_Of_Subcomponent;

   -----------------------
   -- Link_Flow_Feature --
   -----------------------

   function Link_Flow_Feature
     (Feature_Identifier : Node_Id;
      Component          : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Feature_Identifier) = K_Entity_Reference);
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type);

      Pointed_Node       : Node_Id;
      Pointed_Port_Group : Node_Id;
   begin
      Pointed_Node :=
        Find_Feature
          (Component          => Component,
           Feature_Identifier =>
             Item (First_Node (Path (Feature_Identifier))));

      Set_Corresponding_Entity
        (Item (First_Node (Path (Feature_Identifier))),
         Pointed_Node);

      Display_Node_Link
        (Item (First_Node (Path (Feature_Identifier))),
         Pointed_Node);

      if Present (Next_Node (First_Node (Path (Feature_Identifier)))) then
         Pointed_Port_Group :=
           Get_Referenced_Entity (Entity_Ref (Pointed_Node));
         Pointed_Node :=
           Find_Feature
             (Component          => Pointed_Port_Group,
              Feature_Identifier =>
                Item (Next_Node (First_Node (Path (Feature_Identifier)))));

         Set_Corresponding_Entity
           (Item (Next_Node (First_Node (Path (Feature_Identifier)))),
            Pointed_Node);

         Set_Referenced_Entity (Feature_Identifier, Pointed_Node);

         Display_Node_Link
           (Next_Node (First_Node (Path (Feature_Identifier))),
            Pointed_Node);
      end if;

      if Present (Pointed_Node)
        and then Kind (Pointed_Node) /= K_Port_Spec
        and then Kind (Pointed_Node) /= K_Feature_Group_Spec
        and then Kind (Pointed_Node) /= K_Parameter
      then
         return No_Node;
      else
         return Pointed_Node;
      end if;
   end Link_Flow_Feature;

   ---------------
   -- Link_Call --
   ---------------

   function Link_Call (Root : Node_Id; Node : Node_Id) return Boolean is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Subprogram_Call);
      pragma Assert (Kind (Entity_Ref (Node)) = K_Entity_Reference);

      Success            : Boolean := True;
      Pointed_Node       : Node_Id := No_Node;
      Other_Pointed_Node : Node_Id := No_Node;

      Subprogram_Ref  : constant Node_Id := Entity_Ref (Node);
      Pack_Identifier : constant Node_Id :=
        Namespace_Identifier (Subprogram_Ref);

      Pointed_Node_Is_Ok       : Boolean;
      Other_Pointed_Node_Is_Ok : Boolean;
   begin
      --  Either look in available components

      Pointed_Node :=
        Find_Component_Classifier
          (Root                 => Root,
           Package_Identifier   => Pack_Identifier,
           Component_Identifier => Identifier (Subprogram_Ref));

      --  or in local subclauses

      if No (Pointed_Node) then
         Pointed_Node :=
           Find_Subclause
             (Container_Component (Parent_Sequence (Node)),
              Identifier (Subprogram_Ref));
      end if;

      if Present (Next_Node (First_Node (Path (Subprogram_Ref)))) then
         Other_Pointed_Node :=
           Find_Component_Classifier
             (Root                 => Root,
              Package_Identifier   => Pack_Identifier,
              Component_Identifier =>
                Item (First_Node (Path (Subprogram_Ref))));

         if Present (Other_Pointed_Node)
           and then Kind (Other_Pointed_Node) = K_Component_Type
           and then
           (Component_Category'Val (Category (Other_Pointed_Node)) = CC_Thread
            or else
              Component_Category'Val (Category (Other_Pointed_Node)) =
              CC_Data)
         then
            --  Link the Identifier to its corresponding component

            Set_Corresponding_Entity
              (Item (First_Node (Path (Subprogram_Ref))),
               Other_Pointed_Node);

            Other_Pointed_Node :=
              Find_Feature
                (Component          => Other_Pointed_Node,
                 Feature_Identifier =>
                   Item (Next_Node (First_Node (Path (Subprogram_Ref)))));
         else
            Other_Pointed_Node := No_Node;
         end if;
      end if;

      Pointed_Node_Is_Ok :=
        Present (Pointed_Node)
        and then
        ((Kind (Pointed_Node) = K_Component_Type
          or else Kind (Pointed_Node) = K_Component_Implementation
          or else Kind (Pointed_Node) = K_Subcomponent)

         and then
           Component_Category'Val (Category (Pointed_Node)) =
           CC_Subprogram);

      case AADL_Version is
         when AADL_V1 =>
            Other_Pointed_Node_Is_Ok :=
              Present (Other_Pointed_Node)
              and then Kind (Other_Pointed_Node) = K_Subprogram_Spec;

         when AADL_V2 =>
            Other_Pointed_Node_Is_Ok :=
              Present (Other_Pointed_Node)
              and then Kind (Other_Pointed_Node) = K_Subcomponent_Access;
      end case;

      if Pointed_Node_Is_Ok and then Other_Pointed_Node_Is_Ok then
         DAE (Node1 => Node, Message1 => " points to ", Node2 => Pointed_Node);
         DAE
           (Node1    => Node,
            Message1 => " also points to ",
            Node2    => Other_Pointed_Node);
         Success := False;

      elsif Pointed_Node_Is_Ok then
         Set_Referenced_Entity (Entity_Ref (Node), Pointed_Node);

      elsif Other_Pointed_Node_Is_Ok then
         --  In this case, the Other_Pointed_Node is a subprogram
         --  spec, we must link it now because the data component the
         --  subprogram spec may be declared at the end of the AADL
         --  specification.

         Success :=
           Link_Feature (Root, Other_Pointed_Node, No_Node) and then Success;

         Set_Referenced_Entity (Entity_Ref (Node), Other_Pointed_Node);

      else
         DLTWN (Node, Pointed_Node);
         Success := False;
      end if;

      return Success;
   end Link_Call;

   ----------------------------------------------
   -- Link_Component_Implementation_Subclauses --
   ----------------------------------------------

   function Link_Component_Implementation_Subclauses
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Component_Implementation);

      List_Node         : Node_Id;
      Call_List_Node    : Node_Id;
      Success           : Boolean := True;
      Subclause_Success : Boolean := True;
   begin
      --  First, we resolve link for the parent node, as we may extend
      --  its features, or refine its subcomponents.

      if Present (Parent (Node)) then
         Success := Link_Component_Implementation_Subclauses
           (Root, Get_Referenced_Entity
         (Corresponding_Entity (Identifier (Parent (Node)))));
      end if;

      --  Modes, connections and flows are linked only if
      --  subcomponents and features were correctly linked. Indeed,
      --  those subclauses may access elements pointed by
      --  subcomponents or features.

      if not Is_Empty
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Refines_Type (Node))
      then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Refines_Type (Node));

         while Present (List_Node) loop
            Success :=
              Link_Feature (Root, List_Node, No_Node) and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Subcomponents (Node))
      then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Subcomponents (Node));

         while Present (List_Node) loop
            Success :=
              Link_Subcomponent (Root, List_Node)
              and then Link_In_Modes_Statement (Node, In_Modes (List_Node))
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Calls (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Calls (Node));

         while Present (List_Node) loop
            Success :=
              Link_In_Modes_Statement (Node, In_Modes (List_Node))
              and then Success;

            if not Is_Empty (Subprogram_Calls (List_Node)) then
               Call_List_Node := First_Node (Subprogram_Calls (List_Node));

               while Present (Call_List_Node) loop
                  Success := Link_Call (Root, Call_List_Node) and then Success;
                  Call_List_Node := Next_Node (Call_List_Node);
               end loop;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Subclause_Success := Success;

      if Subclause_Success
        and then not Is_Empty
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node))
      then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node));

         while Present (List_Node) loop
            Success     :=
              Link_Connection (Node, List_Node)
              and then Link_In_Modes_Statement (Node, In_Modes (List_Node))
              and then Success;
            List_Node   := Next_Node (List_Node);
         end loop;
      end if;

      Subclause_Success := Success;

      if Subclause_Success
        and then not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Flows (Node))
      then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Flows (Node));

         while Present (List_Node) loop
            if Kind (List_Node) = K_End_To_End_Flow_Refinement
              or else Kind (List_Node) = K_End_To_End_Flow_Spec
            then
               Success :=
                 Link_End_To_End_Flow_Spec (Node, List_Node)
                 and then Link_In_Modes_Statement (Node, In_Modes (List_Node))
                 and then Success;
            elsif Kind (List_Node) = K_Flow_Implementation_Refinement
              or else Kind (List_Node) = K_Flow_Implementation
            then
               Success :=
                 Link_Flow_Implementation (Node, List_Node)
                 and then Link_In_Modes_Statement (Node, In_Modes (List_Node))
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if Subclause_Success
        and then not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Modes (Node))
      then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Modes (Node));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Mode_Transition then
               Success :=
                 Link_Mode_Transition (Node, List_Node) and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Link_Component_Implementation_Subclauses;

   -----------------------------------------------------
   -- Link_Component_Implementation_To_Component_Type --
   -----------------------------------------------------

   function Link_Component_Implementation_To_Component_Type
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Component_Implementation);

      Success      : Boolean := True;
      Pointed_Node : Node_Id;
   begin
      Pointed_Node :=
        Find_Component_Classifier
          (Root                 => Root,
           Package_Identifier   => No_Node,
           Component_Identifier => Component_Type_Identifier (Node));
      --  According to the AADL syntax, the component type must be in
      --  the same namespace as the implementations.

      if No (Pointed_Node) then
         DAE
           (Node1    => Node,
            Message1 => " implements a component type that does not exist");
         Success := False;

      elsif Kind (Pointed_Node) /= K_Component_Type then
         DAE
           (Node1    => Node,
            Message1 => " implements ",
            Node2    => Pointed_Node,
            Message2 => ", which is not a component type");
         Success := False;

      elsif Category (Pointed_Node) /= Category (Node) then
         DAE
           (Node1    => Node,
            Message1 => " implements ",
            Node2    => Pointed_Node,
            Message2 => ", which is of different kind");
         Success := False;
      else
         Set_Corresponding_Entity
           (Component_Type_Identifier (Node),
            Pointed_Node);
         Success := True;
      end if;

      return Success;
   end Link_Component_Implementation_To_Component_Type;

   -----------------------------------------------
   -- Link_Component_Or_Feature_Group_Extension --
   -----------------------------------------------

   function Link_Component_Or_Feature_Group_Extension
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert
        (Kind (Node) = K_Component_Implementation
         or else Kind (Node) = K_Component_Type
         or else Kind (Node) = K_Feature_Group_Type);

      Success      : Boolean := True;
      Pointed_Node : Node_Id;
   begin
      if Present (Parent (Node)) then
         declare
            Component_Ref   : constant Node_Id := Parent (Node);
            Pack_Identifier : Node_Id;
         begin
            Pack_Identifier := Namespace_Identifier (Component_Ref);

            if Kind (Node) = K_Feature_Group_Type then
               Pointed_Node :=
                 Find_Port_Group_Classifier
                   (Root                  => Root,
                    Package_Identifier    => Pack_Identifier,
                    Port_Group_Identifier => Identifier (Component_Ref));
            else
               Pointed_Node :=
                 Find_Component_Classifier
                   (Root                 => Root,
                    Package_Identifier   => Pack_Identifier,
                    Component_Identifier => Identifier (Component_Ref));

            end if;
         end;

         if No (Pointed_Node) then
            DAE
              (Node1    => Node,
               Message1 => " extends something that does not exist");
            Success := False;
         elsif Kind (Pointed_Node) /= Kind (Node) then
            DAE
              (Node1    => Node,
               Message1 => " extends ",
               Node2    => Pointed_Node,
               Message2 => ", which is not of the same kind");
            Success := False;
         elsif Kind (Node) /= K_Feature_Group_Type
           and then
           (Category (Pointed_Node) /= Component_Category'Pos (CC_Abstract)
            and then Category (Pointed_Node) /= Category (Node))
         then
            DAE
              (Node1    => Node,
               Message1 => " extends ",
               Node2    => Pointed_Node,
               Message2 => ", which is of different type");
            Success := False;
         else
            Set_Referenced_Entity (Parent (Node), Pointed_Node);
            Success := True;
         end if;
      end if;

      return Success;
   end Link_Component_Or_Feature_Group_Extension;

   ------------------------------------
   -- Link_Component_Type_Subclauses --
   ------------------------------------

   function Link_Component_Type_Subclauses
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Component_Type);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      if not Is_Empty (Features (Node)) then
         List_Node := First_Node (Features (Node));

         while Present (List_Node) loop
            Success   := Link_Feature (Root, List_Node, Node) and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Flows (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Flows (Node));

         while Present (List_Node) loop
            Success   := Link_Flow_Spec (Node, List_Node) and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Link_Component_Type_Subclauses;

   ---------------------
   -- Link_Connection --
   ---------------------

   function Link_Connection
     (Component : Node_Id;
      Node      : Node_Id) return Boolean
   is
      Success              : Boolean := True;
      Source_Node          : Node_Id;
      Destination_Node     : Node_Id;
      Source_Is_Local      : Boolean;
      Destination_Is_Local : Boolean;
   begin
      if Is_Refinement (Node) then
         return True;
      end if;

      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Kind (Node) = K_Connection);
      pragma Assert (Kind (Source (Node)) = K_Entity_Reference);
      pragma Assert (Kind (Destination (Node)) = K_Entity_Reference);

      --  Connection source

      Retrieve_Connection_End
        (Component          => Component,
         Connection_End     => Source (Node),
         Corresponding_Node => Source_Node,
         Is_Local           => Source_Is_Local);

      if No (Source_Node) then
         DAE
           (Node1    => Source (Node),
            Message1 => "does not point to anything");
         Success := False;
      end if;

      --  Connection destination

      Retrieve_Connection_End
        (Component          => Component,
         Connection_End     => Destination (Node),
         Corresponding_Node => Destination_Node,
         Is_Local           => Destination_Is_Local);

      if No (Destination_Node) then
         DAE
           (Node1    => Destination (Node),
            Message1 => "does not point to anything");
         Success := False;
      end if;

      if Success then
         Set_Referenced_Entity (Source (Node), Source_Node);
         Display_Node_Link (Identifier (Source (Node)), Source_Node);
         Set_Referenced_Entity (Destination (Node), Destination_Node);
         Display_Node_Link (Identifier (Destination (Node)), Destination_Node);
      end if;

      return Success;
   end Link_Connection;

   -------------------------------------
   -- Link_Declarations_Of_Namespaces --
   -------------------------------------

   function Link_Declarations_Of_Namespaces (Root : Node_Id) return Boolean is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      Push_Scope (Entity_Scope (Root));

      if not Is_Empty (Declarations (Root)) then
         List_Node := First_Node (Declarations (Root));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Package_Specification then
               Success :=
                 Link_Declarations_Of_Package (Root => Root, Node => List_Node)
                 and then Success;

            elsif Kind (List_Node) = K_Property_Set then
               Success :=
                 Link_Declarations_Of_Property_Set
                   (Root => Root,
                    Node => List_Node)
                 and then Success;

            elsif Kind (List_Node) = K_Component_Type
              or else Kind (List_Node) = K_Component_Implementation
              or else Kind (List_Node) = K_Feature_Group_Type
            then
               Success :=
                 Link_Component_Or_Feature_Group_Extension
                   (Root => Root,
                    Node => List_Node)
                 and then Success;
            end if;

            if Kind (List_Node) = K_Component_Implementation then
               Success :=
                 Link_Component_Implementation_To_Component_Type
                   (Root,
                    List_Node)
                 and then Success;
            end if;

            if Kind (List_Node) = K_Feature_Group_Type then
               Success :=
                 Link_Inverse_Of_Feature_Group_Type (Root, List_Node)
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Link_Declarations_Of_Namespaces;

   ----------------------------------
   -- Link_Declarations_Of_Package --
   ----------------------------------

   function Link_Declarations_Of_Package
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Package_Specification);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      Push_Scope (Entity_Scope (Node));

      if not Is_Empty (Declarations (Node)) then
         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Component_Type
              or else Kind (List_Node) = K_Component_Implementation
              or else Kind (List_Node) = K_Feature_Group_Type
            then
               Success :=
                 Link_Component_Or_Feature_Group_Extension (Root, List_Node)
                 and then Success;
            end if;

            if Kind (List_Node) = K_Component_Implementation then
               Success :=
                 Link_Component_Implementation_To_Component_Type
                   (Root,
                    List_Node)
                 and then Success;
            end if;

            if Kind (List_Node) = K_Feature_Group_Type then
               Success :=
                 Link_Inverse_Of_Feature_Group_Type (Root, List_Node)
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Link_Declarations_Of_Package;

   ---------------------------------------
   -- Link_Declarations_Of_Property_Set --
   ---------------------------------------

   function Link_Declarations_Of_Property_Set
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Property_Set);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      Push_Scope (Entity_Scope (Node));

      if not Is_Empty (Declarations (Node)) then
         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Property_Definition_Declaration =>
                  Success :=
                    Link_Property_Name (Root, List_Node) and then Success;

               when K_Property_Type_Declaration =>
                  Success :=
                    Link_Property_Type (Root, List_Node) and then Success;

               when K_Constant_Property_Declaration =>
                  Success :=
                    Link_Property_Constant (Root, List_Node) and then Success;

               when others =>
                  raise Program_Error;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Link_Declarations_Of_Property_Set;

   -------------------------------
   -- Link_End_To_End_Flow_Spec --
   -------------------------------

   function Link_End_To_End_Flow_Spec
     (Component : Node_Id;
      Flow      : Node_Id) return Boolean
   is
      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert
        (Kind (Flow) = K_End_To_End_Flow_Refinement
         or else Kind (Flow) = K_End_To_End_Flow_Spec);

      Success      : Boolean := True;
      Pointed_Node : Node_Id;
   begin
      case Kind (Flow) is
         when K_End_To_End_Flow_Spec =>
            --  The Source_Flow field must point to a subcomponent
            --  flow source or path.

            Pointed_Node :=
              Link_Flow_Of_Subcomponent
                (Component       => Component,
                 Flow_Identifier => Source_Flow (Flow),
                 In_Modes        => In_Modes (Flow));

            if No (Pointed_Node) then
               DLTWN (Source_Flow (Flow), Pointed_Node);
               Success := False;
            elsif Flow_Category'Val (Category (Pointed_Node)) /= FC_Source
              and then Flow_Category'Val (Category (Pointed_Node)) /= FC_Path
            then
               DAE
                 (Node1    => Source_Flow (Flow),
                  Message1 => " points to ",
                  Node2    => Pointed_Node,
                  Message2 => " which should be a flow source or flow path");
               Success := False;
            end if;

            --  The Sink_Flow field must point to a subcomponent flow
            --  sink or path.

            Pointed_Node :=
              Link_Flow_Of_Subcomponent
                (Component       => Component,
                 Flow_Identifier => Sink_Flow (Flow),
                 In_Modes        => In_Modes (Flow));

            if No (Pointed_Node) then
               DLTWN (Sink_Flow (Flow), Pointed_Node);
               Success := False;
            elsif Flow_Category'Val (Category (Pointed_Node)) /= FC_Sink
              and then Flow_Category'Val (Category (Pointed_Node)) /= FC_Path
            then
               DAE
                 (Node1    => Sink_Flow (Flow),
                  Message1 => " points to ",
                  Node2    => Pointed_Node,
                  Message2 => " which should be a flow sink or flow path");
               Success := False;
            end if;

            Success := Link_Flow_Connections (Flow) and then Success;

         when others =>
            null;
      end case;

      return Success;
   end Link_End_To_End_Flow_Spec;

   ------------------
   -- Link_Feature --
   ------------------

   function Link_Feature
     (Root           : Node_Id;
      Node           : Node_Id;
      Component_Type : Node_Id) return Boolean
   is
      use Ocarina.Analyzer.AADL.Semantics;

      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert
        (Kind (Node) = K_Port_Spec
         or else Kind (Node) = K_Parameter
         or else Kind (Node) = K_Feature_Group_Spec
         or else Kind (Node) = K_Subprogram_Spec
         or else Kind (Node) = K_Subcomponent_Access);

      Success            : Boolean := True;
      Pointed_Node       : Node_Id := No_Node;
      Other_Pointed_Node : Node_Id := No_Node;
      No_Ref_Given       : Boolean := False;
   --  Some features may not refer to components (e.g. data ports)
   begin
      if AADL_Version = AADL_V2
        and then Present (Entity_Ref (Node))
        and then Present (Namespace_Identifier (Entity_Ref (Node)))
      then
         Success :=
           Check_Qualified_References
             (Namespace (Container_Component (Node)),
              Entity_Ref (Node));
      end if;

      if Success then
         case Kind (Node) is
            when K_Port_Spec | K_Parameter =>
               if Kind (Node) = K_Parameter or else Is_Data (Node) then
                  declare
                     Component_Ref : constant Node_Id := Entity_Ref (Node);
                  begin
                     if Present (Component_Ref) then
                        Pointed_Node :=
                          Find_Component_Classifier
                            (Root               => Root,
                             Package_Identifier =>
                               Namespace_Identifier (Component_Ref),
                             Component_Identifier =>
                               Identifier (Component_Ref));

                        if No (Pointed_Node)
                          and then Present (Component_Type)
                        then
                           Pointed_Node :=
                             Find_Prototype
                               (Component_Type,
                                Identifier (Component_Ref));
                        end if;

                        if No (Pointed_Node) then
                           DLTWN (Node, Component_Ref);
                           Success := False;
                        end if;

                     else
                        Pointed_Node := No_Node;
                        No_Ref_Given := True;
                     end if;

                     if No (Pointed_Node) then
                        if No_Ref_Given then
                           Success := True;
                        else
                           DLTWN (Node, Pointed_Node);
                           Success := False;
                        end if;

                     elsif not
                       ((Kind (Pointed_Node) = K_Component_Type
                         or else
                           Kind (Pointed_Node) =
                           K_Component_Implementation)
                        and then
                          Component_Category'Val (Category (Pointed_Node)) =
                          CC_Data)
                       and then not (Kind (Pointed_Node) = K_Prototype)
                     then
                        DLTWN (Node, Pointed_Node);
                        Success := False;

                     else
                        Set_Referenced_Entity (Component_Ref, Pointed_Node);
                        Success := True;
                     end if;
                  end;
               else
                  --  If we are dealing with an event port

                  No_Ref_Given := True;
                  Success      := True;
               end if;

            when K_Feature_Group_Spec =>
               declare
                  Port_Group_Ref : Node_Id := Entity_Ref (Node);
               begin
                  if No (Port_Group_Ref) then
                     --  If Entity_Ref is null, look for inverse
                     Port_Group_Ref := Inverse_Of (Node);
                  end if;

                  if Present (Port_Group_Ref) then
                     Pointed_Node :=
                       Find_Port_Group_Classifier
                         (Root               => Root,
                          Package_Identifier =>
                            Namespace_Identifier (Port_Group_Ref),
                          Port_Group_Identifier =>
                            Identifier (Port_Group_Ref));
                  else
                     Pointed_Node := No_Node;
                     No_Ref_Given := True;
                  end if;

                  if No (Pointed_Node) then
                     if No_Ref_Given then
                        Success := True;
                     else
                        DLTWN (Node, Pointed_Node);
                        Success := False;
                     end if;

                  elsif Kind (Pointed_Node) /= K_Feature_Group_Type then
                     DLTWN (Node, Pointed_Node);
                     Success := False;
                  else
                     Set_Referenced_Entity (Port_Group_Ref, Pointed_Node);
                     Success := True;
                  end if;
               end;

            when K_Subprogram_Spec =>
               declare
                  Subprog_Ref : constant Node_Id := Entity_Ref (Node);
               begin
                  if Present (Subprog_Ref) then
                     Pointed_Node :=
                       Find_Component_Classifier
                         (Root               => Root,
                          Package_Identifier =>
                            Namespace_Identifier (Subprog_Ref),
                          Component_Identifier => Identifier (Subprog_Ref));

                     Other_Pointed_Node :=
                       Find_Component_Classifier
                         (Root               => Root,
                          Package_Identifier =>
                            Namespace_Identifier (Subprog_Ref),
                          Component_Identifier =>
                            Item (First_Node (Path (Subprog_Ref))));

                     if Present (Other_Pointed_Node)
                       and then Kind (Other_Pointed_Node) = K_Component_Type
                       and then
                         Next_Node (First_Node (Path (Subprog_Ref))) /=
                         No_Node
                     then
                        Other_Pointed_Node :=
                          Find_Feature
                            (Component          => Other_Pointed_Node,
                             Feature_Identifier =>
                               Item
                                 (Next_Node
                                    (First_Node (Path (Subprog_Ref)))));
                     else
                        Other_Pointed_Node := No_Node;
                     end if;

                  else
                     Pointed_Node := No_Node;
                     No_Ref_Given := True;
                  end if;

                  if Present (Pointed_Node)
                    and then Present (Other_Pointed_Node)
                  then
                     DAE
                       (Node1    => Node,
                        Message1 => " points to ",
                        Node2    => Pointed_Node);
                     DAE
                       (Node1    => Node,
                        Message1 => " also points to ",
                        Node2    => Other_Pointed_Node);
                     Success := False;
                  else
                     if No (Pointed_Node) then
                        Pointed_Node := Other_Pointed_Node;
                     end if;

                     if No (Pointed_Node) then
                        if No_Ref_Given then
                           Success := True;
                        --  Nothing was to be found. It is OK

                        else
                           DLTWN (Node, Pointed_Node);
                           Success := False;
                        end if;

                     elsif not
                       ((
                         (Kind (Pointed_Node) = K_Component_Type
                          or else
                            Kind (Pointed_Node) =
                            K_Component_Implementation)
                         and then
                           Component_Category'Val (Category (Pointed_Node)) =
                           CC_Subprogram)
                        or else
                        (Kind (Pointed_Node) = K_Subprogram_Spec
                         and then not Is_Server (Pointed_Node)))
                     then
                        DLTWN (Node, Pointed_Node);
                        Success := False;
                     else
                        Set_Referenced_Entity (Subprog_Ref, Pointed_Node);
                        Success := True;
                     end if;
                  end if;
               end;

            when K_Subcomponent_Access =>
               declare
                  Subcomp_Ref : constant Node_Id := Entity_Ref (Node);
               begin
                  if Present (Subcomp_Ref) then
                     Pointed_Node :=
                       Find_Component_Classifier
                         (Root               => Root,
                          Package_Identifier =>
                            Namespace_Identifier (Subcomp_Ref),
                          Component_Identifier => Identifier (Subcomp_Ref));
                  else
                     Pointed_Node := No_Node;
                     No_Ref_Given := True;
                  end if;

                  if No (Pointed_Node) then
                     if No_Ref_Given then
                        Success := True;
                     else
                        DLTWN (Node, Pointed_Node);
                        Success := False;
                     end if;

                  elsif not
                    ((Kind (Pointed_Node) = K_Component_Type
                      or else Kind (Pointed_Node) = K_Component_Implementation)
                     and then
                       Category (Pointed_Node) =
                       Subcomponent_Category (Node))
                  then
                     DLTWN (Node, Pointed_Node);
                     Success := False;
                  else
                     Set_Referenced_Entity (Subcomp_Ref, Pointed_Node);
                     Success := True;
                  end if;
               end;

            when others =>
               raise Program_Error;
         end case;
      end if;

      if not No_Ref_Given then
         Display_Node_Link (Node, Pointed_Node);
      end if;

      return Success;
   end Link_Feature;

   ------------------------------
   -- Link_Flow_Implementation --
   ------------------------------

   function Link_Flow_Implementation
     (Component : Node_Id;
      Flow      : Node_Id) return Boolean
   is
      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert
        (Kind (Flow) = K_Flow_Implementation_Refinement
         or else Kind (Flow) = K_Flow_Implementation);
      Success      : Boolean                := True;
      C : constant Flow_Category := Flow_Category'Val (Category (Flow));
      Pointed_Node : Node_Id;
   begin
      case Kind (Flow) is
         when K_Flow_Implementation =>
            --  Check for existing flow spec

            if No (Corresponding_Flow_Spec (Flow)) then
               DAE
                 (Node1    => Flow,
                  Message1 => " does not have a corresponding flow spec");
               Success := False;
            else
               --  In case of a flow source or a flow path, the
               --  Source_Flow field is a feature.

               if C = FC_Source or else C = FC_Path then
                  Pointed_Node :=
                    Link_Flow_Feature
                      (Component          => Component,
                       Feature_Identifier => Source_Flow (Flow));

                  Set_Corresponding_Entity
                    (Item (First_Node (Path (Source_Flow (Flow)))),
                     Pointed_Node);
                  Set_Referenced_Entity (Source_Flow (Flow), Pointed_Node);

                  Display_Node_Link (Source_Flow (Flow), Pointed_Node);

                  if No (Pointed_Node) then
                     DAE
                       (Node1    => Source_Flow (Flow),
                        Message1 => " does not point to a feature");
                     Success := False;
                  else
                     --  Check that the source is the same as in the
                     --  flow spec.

                     if Get_Referenced_Entity (Source_Flow (Flow)) /=
                       Get_Referenced_Entity
                         (Source_Flow (Corresponding_Flow_Spec (Flow)))
                     then
                        DAE
                          (Node1    => Flow,
                           Message1 =>
                             " and its corresponding flow spec" &
                             " have different sources");
                        Success := False;
                     end if;
                  end if;
               end if;

               --  In case of a flow sink or a flow path, the
               --  Sink_Flow field is a feature.

               if C = FC_Sink or else C = FC_Path then
                  Pointed_Node :=
                    Link_Flow_Feature
                      (Component          => Component,
                       Feature_Identifier => Sink_Flow (Flow));

                  Set_Corresponding_Entity
                    (Item (First_Node (Path (Sink_Flow (Flow)))),
                     Pointed_Node);
                  Set_Referenced_Entity (Sink_Flow (Flow), Pointed_Node);

                  Display_Node_Link (Sink_Flow (Flow), Pointed_Node);

                  if No (Pointed_Node) then
                     DAE
                       (Node1    => Sink_Flow (Flow),
                        Message1 => " does not point to a feature");
                     Success := False;
                  else
                     --  Check that the sink is the same as in the
                     --  flow spec.

                     if Present (Get_Referenced_Entity
                                   (Sink_Flow
                                      (Corresponding_Flow_Spec (Flow))))
                       and then
                       Get_Referenced_Entity (Sink_Flow (Flow)) /=
                       Get_Referenced_Entity
                         (Sink_Flow (Corresponding_Flow_Spec (Flow)))
                     then
                        DAE
                          (Node1    => Flow,
                           Message1 =>
                             " and its corresponding flow spec" &
                             " have different sinks");
                        Success := False;
                     end if;
                  end if;
               end if;

               --  We do not try to link connection if the flow
               --  extremities are erronous because this can cause an
               --  error cascade.

               Success := Success and then Link_Flow_Connections (Flow);
            end if;

         when others =>
            null;
      end case;

      return Success;
   end Link_Flow_Implementation;

   --------------------
   -- Link_Flow_Spec --
   --------------------

   function Link_Flow_Spec
     (Component : Node_Id;
      Flow      : Node_Id) return Boolean
   is
      pragma Assert (Kind (Component) = K_Component_Type);
      pragma Assert (Kind (Flow) = K_Flow_Spec);

      Success      : Boolean := True;
      Pointed_Node : Node_Id := No_Node;
   begin
      --  Flow refinements do not contain elements to link. The
      --  semantic part of the analyzer should check if a flow
      --  refinement corresponds to an existing flow
      --  implementation. It should also check that the different
      --  elements of the flow are compatible (e.g. connections
      --  actually connect the flows and ports specified).

      if Is_Refinement (Flow) then
         return True;
      end if;

      if Flow_Category'Val (Category (Flow)) = FC_Source
        or else Flow_Category'Val (Category (Flow)) = FC_Path
      then
         Pointed_Node :=
           Link_Flow_Feature
             (Feature_Identifier => Source_Flow (Flow),
              Component          => Component);

         if No (Pointed_Node) then
            DLTWN (Source_Flow (Flow), Pointed_Node);
            Success := False;
         else
            if Next_Node (First_Node (Path (Source_Flow (Flow)))) =
              No_Node
            then
               Display_Node_Link
                 (Item (First_Node (Path (Source_Flow (Flow)))),
                  Pointed_Node);
               Set_Corresponding_Entity
                 (Item (First_Node (Path (Source_Flow (Flow)))),
                  Pointed_Node);
            else
               Display_Node_Link
                 (Item (Next_Node (First_Node (Path (Source_Flow (Flow))))),
                  Pointed_Node);
               Set_Corresponding_Entity
                 (Item (Next_Node (First_Node (Path (Source_Flow (Flow))))),
                  Pointed_Node);
            end if;

            Set_Referenced_Entity (Source_Flow (Flow), Pointed_Node);
         end if;
      end if;

      if Flow_Category'Val (Category (Flow)) = FC_Sink
        or else Flow_Category'Val (Category (Flow)) = FC_Path
      then
         Pointed_Node :=
           Link_Flow_Feature
             (Feature_Identifier => Sink_Flow (Flow),
              Component          => Component);

         if No (Pointed_Node) then
            DLTWN (Sink_Flow (Flow), Pointed_Node);
            Success := False;
         else
            if Next_Node (First_Node (Path (Sink_Flow (Flow)))) = No_Node then
               Display_Node_Link
                 (Item (First_Node (Path (Sink_Flow (Flow)))),
                  Pointed_Node);
               Set_Corresponding_Entity
                 (Item (First_Node (Path (Sink_Flow (Flow)))),
                  Pointed_Node);
            else
               Display_Node_Link
                 (Item (Next_Node (First_Node (Path (Sink_Flow (Flow))))),
                  Pointed_Node);
               Set_Corresponding_Entity
                 (Item (Next_Node (First_Node (Path (Sink_Flow (Flow))))),
                  Pointed_Node);
            end if;

            Set_Referenced_Entity (Sink_Flow (Flow), Pointed_Node);
         end if;
      end if;

      return Success;
   end Link_Flow_Spec;

   -----------------------------
   -- Link_In_Modes_Statement --
   -----------------------------

   function Link_In_Modes_Statement
     (Component : Node_Id;
      In_Modes  : Node_Id) return Boolean
   is
      pragma Assert (Kind (Component) = K_Component_Implementation);

      function Set_Corresponding_Mode
        (Component      : Node_Id;
         Mode_Reference : Node_Id) return Boolean;

      ----------------------------
      -- Set_Corresponding_Mode --
      ----------------------------

      function Set_Corresponding_Mode
        (Component      : Node_Id;
         Mode_Reference : Node_Id) return Boolean
      is
         Pointed_Node : Node_Id;
         Success      : Boolean := True;
      begin
         Pointed_Node := Find_Mode (Component, Identifier (Mode_Reference));

         if No (Pointed_Node) or else Kind (Pointed_Node) /= K_Mode then
            DLTWN (Mode_Reference, Pointed_Node);
            Success := False;

         else
            Set_Referenced_Entity (Mode_Reference, Pointed_Node);
         end if;

         return Success;
      end Set_Corresponding_Mode;

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      if Present (In_Modes) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Modes (In_Modes));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Entity_Reference then
               Success :=
                 Set_Corresponding_Mode (Component, List_Node)
                 and then Success;

            elsif Kind (List_Node) = K_Pair_Of_Entity_References then
               Success :=
                 Set_Corresponding_Mode
                   (Component,
                    First_Reference (List_Node))
                 and then Set_Corresponding_Mode
                   (Component,
                    Second_Reference (List_Node))
                 and then Success;
            else
               raise Program_Error;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Link_In_Modes_Statement;

   ----------------------------------------
   -- Link_Inverse_Of_Feature_Group_Type --
   ----------------------------------------

   function Link_Inverse_Of_Feature_Group_Type
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      Success      : Boolean := True;
      Pointed_Node : Node_Id := No_Node;
   begin
      if Present (Inverse_Of (Node)) then
         Pointed_Node :=
           Find_Port_Group_Classifier
             (Root                  => Root,
              Package_Identifier => Namespace_Identifier (Inverse_Of (Node)),
              Port_Group_Identifier => Identifier (Inverse_Of (Node)));

         if No (Pointed_Node)
           or else Kind (Pointed_Node) /= K_Feature_Group_Type
         then
            DLTWN (Node, Pointed_Node);
            Success := False;

         else
            Set_Referenced_Entity (Inverse_Of (Node), Pointed_Node);
         end if;
      end if;

      return Success;
   end Link_Inverse_Of_Feature_Group_Type;

   --------------------------
   -- Link_Mode_Transition --
   --------------------------

   function Link_Mode_Transition
     (Component : Node_Id;
      Node      : Node_Id) return Boolean
   is
      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Kind (Node) = K_Mode_Transition);

      Source_Mode_List : constant List_Id := Source_Modes (Node);
      Port_List        : constant List_Id := Triggers (Node);
      Destination_Mode : constant Node_Id :=
        Ocarina.ME_AADL.AADL_Tree.Nodes.Destination_Mode (Node);
      List_Node    : Node_Id;
      Entity_Node  : Node_Id;
      Pointed_Node : Node_Id;
      SC_Owned     : Boolean;
      Success      : Boolean := True;
   begin
      --  We first link with the in event ports

      if not Is_Empty (Port_List) then
         List_Node := First_Node (Port_List);

         while Present (List_Node) loop
            if Kind (List_Node) = K_Mode_Transition_Trigger then
               Entity_Node := Identifier (List_Node);
            else
               Entity_Node := List_Node;
            end if;

            if No (Next_Node (First_Node (Path (Entity_Node)))) then
               --  We look for a feature of the component

               Pointed_Node :=
                 Find_Feature
                   (Component          => Component,
                    Feature_Identifier =>
                      Item (First_Node (Path (Entity_Node))));

               SC_Owned := False;
            else
               --  We look for a feature of a subcomponent

               Pointed_Node :=
                 Find_Subcomponent
                   (Component               => Component,
                    Subcomponent_Identifier =>
                      Item (First_Node (Path (Entity_Node))));
               if Present (Pointed_Node) then
                  if Present (Entity_Ref (Pointed_Node))
                    and then Present
                      (Get_Referenced_Entity (Entity_Ref (Pointed_Node)))
                  then
                     Pointed_Node :=
                       Find_Feature
                         (Component =>
                            Get_Referenced_Entity (Entity_Ref (Pointed_Node)),
                          Feature_Identifier =>
                            Item
                              (Next_Node (First_Node (Path (Entity_Node)))));

                     SC_Owned := True;
                  elsif Present (Inverse_Of (Pointed_Node))
                    and then Present
                      (Get_Referenced_Entity (Inverse_Of (Pointed_Node)))
                  then
                     Pointed_Node :=
                       Find_Feature
                         (Component =>
                            Get_Referenced_Entity (Inverse_Of (Pointed_Node)),
                          Feature_Identifier =>
                            Item
                              (Next_Node (First_Node (Path (Entity_Node)))));

                     SC_Owned := True;
                  end if;
               end if;
            end if;

            if Present (Pointed_Node) then
               if Kind (Pointed_Node) /= K_Port_Spec
                 or else not Is_Event (Pointed_Node)
                 or else Is_Data (Pointed_Node)
               then
                  --  Mode triggers must be pure event ports

                  DAE
                    (Node1    => Entity_Node,
                     Message1 => " points to ",
                     Node2    => Pointed_Node,
                     Message2 => ", which is not an event port");
                  Success := False;
               elsif SC_Owned and then not Is_Out (Pointed_Node) then
                  --  Mode triggers belonging to a subcomponent must
                  --  be OUT or IN OUT.

                  DAE
                    (Node1    => Entity_Node,
                     Message1 => " points to subcomponent port ",
                     Node2    => Pointed_Node,
                     Message2 => ", which is not an OUT nor INOUT event port");
                  Success := False;
               elsif not SC_Owned and then not Is_In (Pointed_Node) then
                  --  Mode triggers belonging to the current component
                  --  must be IN or IN OUT.

                  DAE
                    (Node1    => Entity_Node,
                     Message1 => " points to port ",
                     Node2    => Pointed_Node,
                     Message2 => ", which is not an IN nor INOUT event port");
                  Success := False;
               else
                  --  Everything is fine

                  Set_Corresponding_Entity
                    (Item (First_Node (Path (Entity_Node))),
                     Pointed_Node);
               end if;
            else
               DLTWN (Entity_Node, Pointed_Node);
               Success := False;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      else
         DAE
           (Message0 => "Mode transition ",
            Node1    => Node,
            Message1 => " depends on no in event port");
         Success := False;
      end if;

      --  Then we link the source modes with the modes declared within
      --  the component implementation.

      if not Is_Empty (Source_Mode_List) then
         List_Node := First_Node (Source_Mode_List);

         while Present (List_Node) loop
            Pointed_Node :=
              Find_Mode (Component => Component, Mode_Identifier => List_Node);

            if Present (Pointed_Node) then
               Set_Corresponding_Entity (List_Node, Pointed_Node);
            else
               DLTWN (List_Node, Pointed_Node);
               Success := False;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      else
         DAE
           (Message0 => "warning: ",
            Node1    => Node,
            Message1 => " has no source mode");
      end if;

      --  Finally we link the destination mode

      Pointed_Node :=
        Find_Mode
          (Component       => Component,
           Mode_Identifier => Destination_Mode);

      if Present (Pointed_Node) then
         Set_Corresponding_Entity (Destination_Mode, Pointed_Node);
      else
         DLTWN (Destination_Mode, Pointed_Node);
         Success := False;
      end if;

      return Success;
   end Link_Mode_Transition;

   ----------------------------------------
   -- Link_Feature_Group_Type_Subclauses --
   ----------------------------------------

   function Link_Feature_Group_Type_Subclauses
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Feature_Group_Type);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Features (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Features (Node));

         while Present (List_Node) loop
            Success :=
              Link_Feature (Root, List_Node, No_Node) and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Link_Feature_Group_Type_Subclauses;

   -------------------------------
   -- Link_Property_Association --
   -------------------------------

   function Link_Property_Association
     (Root      : Node_Id;
      Container : Node_Id;
      Node      : Node_Id) return Boolean
   is
      use Ocarina.Analyzer.AADL.Semantics;

      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Property_Association);
      pragma Assert
        (Kind (Container) = K_Component_Implementation
         or else Kind (Container) = K_Component_Type
         or else Kind (Container) = K_Feature_Group_Type
         or else Kind (Container) = K_Package_Specification
         or else Kind (Container) = K_Flow_Spec
         or else Kind (Container) = K_Flow_Implementation
         or else Kind (Container) = K_Flow_Implementation_Refinement
         or else Kind (Container) = K_End_To_End_Flow_Spec
         or else Kind (Container) = K_End_To_End_Flow_Refinement
         or else Kind (Container) = K_Connection
         or else Kind (Container) = K_Subcomponent
         or else Kind (Container) = K_Port_Spec
         or else Kind (Container) = K_Parameter
         or else Kind (Container) = K_Feature_Group_Spec
         or else Kind (Container) = K_Subcomponent_Access
         or else Kind (Container) = K_Subprogram_Spec
         or else Kind (Container) = K_Mode
         or else Kind (Container) = K_Subprogram_Call);

      Success                 : Boolean := True;
      Pointed_Node            : Node_Id;
      Corresponding_Container : Node_Id;
      List_Node               : Node_Id;
      Tmp_Node                : Node_Id;
      Property_Type           : Node_Id := No_Node;
   begin
      if AADL_Version = AADL_V2
        and then Present (Namespace_Identifier (Property_Name (Node)))
        and then Present (Value_Container (Property_Association_Value (Node)))
      then
         Success :=
           Check_Qualified_References
             (Value_Container (Property_Association_Value (Node)),
              Namespace_Identifier (Property_Name (Node)));
      end if;

      if Success then
         Pointed_Node :=
           Find_Property_Entity
             (Root                    => Root,
              Property_Set_Identifier =>
                Namespace_Identifier (Property_Name (Node)),
              Property_Identifier => Identifier (Property_Name (Node)));

         if No (Pointed_Node) then
            DAE (Node1 => Node, Message1 => "does not point to anything");
            Success := False;

         elsif Kind (Pointed_Node) /= K_Property_Definition_Declaration then
            DAE
              (Node1    => Node,
               Message1 => " points to ",
               Node2    => Pointed_Node,
               Message2 => ", which is not a property name");
            Success := False;
         else
            Set_Referenced_Entity (Property_Name (Node), Pointed_Node);

            --  Get the type of the property association

            Property_Type := Property_Name_Type (Pointed_Node);
         end if;

         --  Link to the referenced entity if it is relevant

         if Present (Property_Association_Value (Node)) then
            if Present (Single_Value (Property_Association_Value (Node))) then
               Success :=
                 Link_Property_Value
                   (Root,
                    Container,
                    Node,
                    Single_Value (Property_Association_Value (Node)),
                    Property_Type)
                 and then Success;
            else
               List_Node :=
                 First_Node (Multi_Value (Property_Association_Value (Node)));

               while Present (List_Node) loop
                  Success :=
                    Link_Property_Value
                      (Root,
                       Container,
                       Node,
                       List_Node,
                       Property_Type)
                    and then Success;
                  List_Node := Next_Node (List_Node);
               end loop;
            end if;
         end if;

         --  Link 'applies to' statement

         if not Is_Empty (Applies_To_Prop (Node)) then
            List_Node    := First_Node (Applies_To_Prop (Node));

            while Present (List_Node) loop
               Pointed_Node := Container;

               case Kind (Pointed_Node) is
                  when K_Subcomponent     |
                    K_Port_Spec           |
                    K_Feature_Group_Spec  |
                    K_Feature_Group_Type  |
                    K_Parameter           |
                    K_Subcomponent_Access |
                    K_Subprogram_Spec     |
                    K_Subprogram_Call     =>
                     Corresponding_Container :=
                       Get_Referenced_Entity (Entity_Ref (Pointed_Node));
                  --  For subclauses that can refer to a component, we
                  --  retrieve the corresponding entity.

                  when K_Component_Type | K_Component_Implementation =>
                     Corresponding_Container := Pointed_Node;

                  when K_Connection =>
                     Corresponding_Container :=
                       Container_Component (Pointed_Node);

                  when others =>
                     --  These entities cannot have 'applies to'
                     --  clause in their property associations.

                     Corresponding_Container := No_Node;
               end case;

               if Present (Corresponding_Container) then
                  Tmp_Node := First_Node (List_Items (List_Node));

                  if Kind (Tmp_Node) = K_Array_Selection then
                     Pointed_Node :=
                       Find_Subclause
                         (Corresponding_Container,
                          Identifier (Tmp_Node));
                     Tmp_Node := Identifier (Tmp_Node);
                  else
                     while Tmp_Node /= No_Node loop
                        Pointed_Node :=
                          Find_Subclause (Corresponding_Container, Tmp_Node);

                        if Present (Pointed_Node) then
                           Set_Corresponding_Entity (Tmp_Node, Pointed_Node);
                           Set_Corresponding_Entity
                             (First_Node (List_Items (List_Node)),
                              Pointed_Node);

                        else
                           DAE
                             (Node1    => Node,
                              Message1 => "points to ",
                              Node2    => Tmp_Node,
                              Message2 => "that is not a valid subcomponent");
                           Success := False;
                           exit;
                        end if;

                        Tmp_Node := Next_Node (Tmp_Node);
                        if No (Tmp_Node) then
                           exit;
                        end if;
                        Corresponding_Container :=
                          Get_Referenced_Entity (Entity_Ref (Pointed_Node));
                     end loop;
                  end if;

               else
                  DAE
                    (Node1    => Node,
                     Message1 => "applies to something that cannot be found");
                  --  XXX is this the correct error message?
                  Pointed_Node := No_Node;
               end if;
               List_Node := Next_Node (List_Node);
            end loop;

            if No (Pointed_Node) then
               Success := False;
            end if;
         end if;

         --  link 'in modes' statement

         if Present (In_Modes (Node))
           and then not Is_Empty (Modes (In_Modes (Node)))
         then
            --  If the container or the pointed node of the property
            --  association is a mode, raise an error.

            if Kind (Container) = K_Mode
              or else
              (Present (Pointed_Node) and then Kind (Pointed_Node) = K_Mode)
            then
               DAE
                 (Node1    => Node,
                  Message1 =>
                    " belongs to a mode. It cannot have " &
                    "'in modes'  statement");
               Success := False;
            elsif Present (Current_Scope)
              and then
                Kind (Corresponding_Entity (Current_Scope)) =
                K_Component_Implementation
            then
               Success :=
                 Link_In_Modes_Statement
                   (Component => Corresponding_Entity (Current_Scope),
                    In_Modes  => In_Modes (Node))
                 and then Success;
            else
               DAE
                 (Node1    => Node,
                  Message1 =>
                    " have 'in modes' statement but is not in" &
                    " a component implementation");
               Success := False;
            end if;
         end if;
      end if;

      return Success;
   end Link_Property_Association;

   ---------------------
   -- Link_Properties --
   ---------------------

   function Link_Properties
     (Root      : Node_Id;
      Container : Node_Id;
      List      : List_Id) return Boolean
   is
      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      if not Is_Empty (List) then
         List_Node := First_Node (List);

         while Present (List_Node) loop
            pragma Assert (Kind (List_Node) = K_Property_Association);

            Success :=
              Link_Property_Association (Root, Container, List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Link_Properties;

   -----------------------------------------
   -- Link_Properties_Of_AADL_Description --
   -----------------------------------------

   function Link_Properties_Of_AADL_Description
     (Root : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      Push_Scope (Entity_Scope (Root));

      if not Is_Empty (Declarations (Root)) then
         List_Node := First_Node (Declarations (Root));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Package_Specification then
               Success :=
                 Link_Properties_Of_Package (Root => Root, Node => List_Node)
                 and then Success;

            elsif Kind (List_Node) = K_Component_Type
              or else Kind (List_Node) = K_Component_Implementation
              or else Kind (List_Node) = K_Feature_Group_Type
            then
               Success :=
                 Link_Properties_Of_Component (Root => Root, Node => List_Node)
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Link_Properties_Of_AADL_Description;

   ----------------------------------
   -- Link_Properties_Of_Component --
   ----------------------------------

   function Link_Properties_Of_Component
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      pragma Assert
        (Kind (Node) = K_Component_Implementation
         or else Kind (Node) = K_Component_Type
         or else Kind (Node) = K_Feature_Group_Type);

      Success : Boolean := False;
   begin
      case Kind (Node) is
         when K_Component_Type =>
            Success := Link_Properties_Of_Component_Type (Root, Node);

         when K_Component_Implementation =>
            Success :=
              Link_Properties_Of_Component_Implementation (Root, Node);

         when K_Feature_Group_Type =>
            Success := Link_Properties_Of_Feature_Group_Type (Root, Node);

         when others =>
            raise Program_Error;
      end case;

      return Success;
   end Link_Properties_Of_Component;

   -------------------------------------------------
   -- Link_Properties_Of_Component_Implementation --
   -------------------------------------------------

   function Link_Properties_Of_Component_Implementation
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Component_Implementation);

      List_Node      : Node_Id;
      Call_List_Node : Node_Id;
      Success        : Boolean := True;
   begin
      Push_Scope (Property_Scope (Node));

      if not Is_Empty (Refines_Type (Node)) then
         List_Node := First_Node (Refines_Type (Node));

         while Present (List_Node) loop
            Success :=
              Link_Properties
                (Root,
                 List_Node,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node))
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Subcomponents (Node)) then
         List_Node := First_Node (Subcomponents (Node));

         while Present (List_Node) loop
            Success :=
              Link_Properties
                (Root,
                 List_Node,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node))
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Calls (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Calls (Node));

         while Present (List_Node) loop
            if not Is_Empty (Subprogram_Calls (List_Node)) then
               Call_List_Node := First_Node (Subprogram_Calls (List_Node));

               while Present (Call_List_Node) loop
                  Success :=
                    Link_Properties
                      (Root,
                       Node,
                       Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
                         (Call_List_Node))
                    and then Success;
                  Call_List_Node := Next_Node (Call_List_Node);
               end loop;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node));

         while Present (List_Node) loop
            Success :=
              Link_Properties
                (Root,
                 List_Node,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node))
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Flows (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Flows (Node));

         while Present (List_Node) loop
            Success :=
              Link_Properties
                (Root,
                 List_Node,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node))
              and then Success;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Modes (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Modes (Node));

         while Present (List_Node) loop
            --  Mode transitions have no property associations

            if Kind (List_Node) = K_Mode then
               Success :=
                 Link_Properties
                   (Root,
                    List_Node,
                    Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node))
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

         while Present (List_Node) loop
            Success :=
              Link_Property_Association (Root, Node, List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Link_Properties_Of_Component_Implementation;

   ---------------------------------------
   -- Link_Properties_Of_Component_Type --
   ---------------------------------------

   function Link_Properties_Of_Component_Type
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Component_Type);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      Push_Scope (Property_Scope (Node));

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Features (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Features (Node));

         while Present (List_Node) loop
            Success :=
              Link_Properties
                (Root,
                 List_Node,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node))
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Flows (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Flows (Node));

         while Present (List_Node) loop
            Success :=
              Link_Properties
                (Root,
                 List_Node,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node))
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

         while Present (List_Node) loop
            Success :=
              Link_Property_Association (Root, Node, List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Link_Properties_Of_Component_Type;

   --------------------------------
   -- Link_Properties_Of_Package --
   --------------------------------

   function Link_Properties_Of_Package
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Package_Specification);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      Push_Scope (Property_Scope (Node));

      if not Is_Empty (Declarations (Node)) then
         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Component_Type
              or else Kind (List_Node) = K_Component_Implementation
              or else Kind (List_Node) = K_Feature_Group_Type
            then
               Success :=
                 Link_Properties_Of_Component (Root, List_Node)
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

         while Present (List_Node) loop
            Success :=
              Link_Property_Association (Root, Node, List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Link_Properties_Of_Package;

   -------------------------------------------
   -- Link_Properties_Of_Feature_Group_Type --
   -------------------------------------------

   function Link_Properties_Of_Feature_Group_Type
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Feature_Group_Type);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      Push_Scope (Property_Scope (Node));

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Features (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Features (Node));

         while Present (List_Node) loop
            Success :=
              Link_Properties
                (Root,
                 List_Node,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node))
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

         while Present (List_Node) loop
            Success :=
              Link_Property_Association (Root, Node, List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Pop_Scope;
      return Success;
   end Link_Properties_Of_Feature_Group_Type;

   ----------------------------
   -- Link_Property_Constant --
   ----------------------------

   function Link_Property_Constant
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Constant_Property_Declaration);
      pragma Assert (Present (Constant_Type (Node)));

      Success      : Boolean := True;
      Pointed_Node : Node_Id;
   begin
      --  Link unit identifier

      if Unique_Unit_Identifier (Node) /= No_Node then
         Pointed_Node :=
           Find_Property_Entity
             (Root,
              Property_Set_Identifier (Unique_Unit_Identifier (Node)),
              Identifier (Unique_Unit_Identifier (Node)));

         if Present (Pointed_Node) then
            Set_Referenced_Entity
              (Unique_Unit_Identifier (Node),
               Pointed_Node);
         else
            DLTWN (Unique_Unit_Identifier (Node), Pointed_Node);
            Success := False;
         end if;
      end if;

      --  Link constant type if it is a reference to a declared
      --  property type.

      if Kind (Constant_Type (Node)) = K_Unique_Property_Type_Identifier then
         declare
            Pointed_Node : Node_Id;
         begin
            Pointed_Node :=
              Find_Property_Entity
                (Root                    => Root,
                 Property_Set_Identifier =>
                   Property_Set_Identifier (Constant_Type (Node)),
                 Property_Identifier => Identifier (Constant_Type (Node)));

            if No (Pointed_Node)
              or else Kind (Pointed_Node) /= K_Property_Type_Declaration
            then
               DLTWN (Node, Pointed_Node);
               Success := False;

            else
               Set_Corresponding_Entity
                 (Identifier (Constant_Type (Node)),
                  Pointed_Node);
               Set_Referenced_Entity (Constant_Type (Node), Pointed_Node);
            end if;
         end;
      end if;

      --  Link constant value(s)

      if Single_Value (Constant_Value (Node)) /= No_Node then
         Success :=
           Link_Property_Value
             (Root,
              Corresponding_Entity (Current_Scope),
              Node,
              Single_Value (Constant_Value (Node)),
              Node)
           and then Success;

      else
         pragma Assert (Multi_Value (Constant_Value (Node)) /= No_List);
         declare
            List_Node : Node_Id;
         begin
            List_Node := First_Node (Multi_Value (Constant_Value (Node)));

            while Present (List_Node) loop
               Success :=
                 Link_Property_Value
                   (Root,
                    Corresponding_Entity (Current_Scope),
                    Node,
                    List_Node,
                    Node)
                 and then Success;
               List_Node := Next_Node (List_Node);
            end loop;
         end;
      end if;

      return Success;
   end Link_Property_Constant;

   ------------------------
   -- Link_Property_Name --
   ------------------------

   function Link_Property_Type_Name
     (Root : Node_Id;
      Node : Node_Id;
      Type_Designator : Node_Id)
     return Boolean
   is
      Success : Boolean := True;
   begin
      case Kind (Type_Designator) is
         when K_Unique_Property_Type_Identifier =>
            declare
               Pointed_Node : Node_Id;
            begin
               Pointed_Node :=
                 Find_Property_Entity
                 (Root                    => Root,
                  Property_Set_Identifier =>
                    Property_Set_Identifier (Type_Designator),
                  Property_Identifier => Identifier (Type_Designator));

               if No (Pointed_Node) then
                  DAE
                    (Node1    => Node,
                     Message1 => "does not point to anything");
                  Success := False;

               elsif (Kind (Pointed_Node) /= K_Property_Type_Declaration
                        and then Kind (Pointed_Node)
                        /= K_Property_Definition_Declaration)
               then
                  DAE
                    (Node1    => Node,
                     Message1 => " points to ",
                     Node2    => Pointed_Node,
                     Message2 => ", which is not a property type");
                  Success := False;

               else
                  Set_Referenced_Entity (Type_Designator, Pointed_Node);
               end if;
            end;

         when K_Integer_Type | K_Real_Type =>
            --  If the property names is of type integer or real, then we
            --  must link the optional unit

            Success := Link_Type_Designator (Root, Type_Designator);

         when K_Range_Type =>
            Success := Link_Type_Designator (Root, Type_Designator);

         when K_Classifier_Type | K_Reference_Type | K_Enumeration_Type
           | K_Boolean_Type | K_String_Type =>
            null; -- Nothing to do here

         when K_Record_Type =>
            declare
               J : Node_Id;
            begin
               J := First_Node (List_Items (Type_Designator));
               while Present (J) loop
                  Success := Success and then
                    Link_Property_Type_Name
                    (Root, J, Property_Type_Designator (J));
                  J := Next_Node (J);
               end loop;
            end;

         when others =>
            raise Program_Error;
            --  Null;
      end case;

      return Success;
   end Link_Property_Type_Name;

   function Link_Property_Name
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Property_Definition_Declaration
                    or else Kind (Node) = K_Record_Type_Element);
      pragma Assert (Present (Property_Name_Type (Node)));

      Success         : Boolean          := True;
      List_Node       : Node_Id;
      Type_Designator : constant Node_Id :=
        Property_Type_Designator (Property_Name_Type (Node));
      Property_Type : constant Node_Id := Property_Name_Type (Node);

   begin
      Success := Link_Property_Type_Name (Root, Node, Type_Designator);

      --  Link optional default value

      if Default_Value (Node) /= No_Node then
         if Single_Value (Default_Value (Node)) /= No_Node then
            Success :=
              Link_Property_Value
                (Root,
                 Corresponding_Entity (Current_Scope),
                 Node,
                 Single_Value (Default_Value (Node)),
                 Property_Type)
              and then Success;

         elsif Multi_Value (Default_Value (Node)) /= No_List then
            declare
               List_Node : Node_Id;
            begin
               List_Node := First_Node (Multi_Value (Default_Value (Node)));

               while Present (List_Node) loop
                  Success :=
                    Link_Property_Value
                      (Root,
                       Corresponding_Entity (Current_Scope),
                       Node,
                       List_Node,
                       Property_Type)
                    and then Success;
                  List_Node := Next_Node (List_Node);
               end loop;
            end;
         end if;
      end if;

      --  Link optional classifier reference of the owner
      --  category

      if Owner_Categories (Applies_To (Node)) /= No_List then
         List_Node := First_Node (Owner_Categories (Applies_To (Node)));

         while Present (List_Node) loop
            if Classifier_Ref (List_Node) /= No_Node then

               Push_Scope (Entity_Scope (Root));

               --  We search declarations from the root namespace, not
               --  from the property set namespace

               case Named_Element'Val (Category (List_Node)) is
                  when PO_Component_Category =>
                     Set_Referenced_Entity
                       (Classifier_Ref (List_Node),
                        Find_Component_Classifier
                          (Root               => Root,
                           Package_Identifier =>
                             Namespace_Identifier (Classifier_Ref (List_Node)),
                           Component_Identifier =>
                             Identifier (Classifier_Ref (List_Node))));
                  when PO_Port_Group | PO_Feature_Group =>
                     Set_Referenced_Entity
                       (Classifier_Ref (List_Node),
                        Find_Port_Group_Classifier
                          (Root               => Root,
                           Package_Identifier =>
                             Namespace_Identifier (Classifier_Ref (List_Node)),
                           Port_Group_Identifier =>
                             Identifier (Classifier_Ref (List_Node))));

                  when others =>
                     Set_Referenced_Entity
                       (Classifier_Ref (List_Node),
                        No_Node);
                     --  XXX We do not link subclauses. Is it relevant
                     --  to do so?
               end case;

               Pop_Scope;

               if Get_Referenced_Entity (Classifier_Ref (List_Node)) =
                 No_Node
               then
                  Display_Link_To_Wrong_Node
                    (Classifier_Ref (List_Node),
                     Get_Referenced_Entity (Classifier_Ref (List_Node)),
                     Warning => True);
                  --  Not finding the corresponding entity is not a
                  --  problem if the property is not to be used.
               end if;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Link_Property_Name;

   ------------------------
   -- Link_Property_Type --
   ------------------------

   function Link_Property_Type
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Property_Type_Declaration);
   begin
      return Link_Type_Designator (Root, Property_Type_Designator (Node));
   end Link_Property_Type;

   -------------------------
   -- Link_Property_Value --
   -------------------------

   function Link_Property_Value
     (Root               : Node_Id;
      Container          : Node_Id;
      Property_Container : Node_Id;
      Node               : Node_Id;
      Property_Type      : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Present (Container));
      pragma Assert (Present (Node));
      pragma Assert
        (No (Property_Type)
         or else Kind (Property_Type) = K_Real_Type
         or else Kind (Property_Type) = K_Integer_Type
         or else Kind (Property_Type) = K_Property_Type
         or else Kind (Property_Type) = K_Constant_Property_Declaration
         or else Kind (Property_Type) = K_Record_Type_Element
         or else Kind (Property_Type) = K_Property_Term
         or else Kind (Property_Type) = K_Unique_Property_Type_Identifier);

      Success                 : Boolean          := True;
      Pointed_Node            : Node_Id;
      Local_Scope             : constant Node_Id := Current_Scope;
      List_Node               : Node_Id;
      Corresponding_Container : Node_Id;
      Type_Designator         : Node_Id;

   begin
      case Kind (Node) is
         when K_Component_Classifier_Term =>
            Pop_Scope;

            --  The current scope is supposed to be the one of the
            --  component we are working in. We have to retrieve the
            --  above scope, which is the one of the current
            --  namespace.

            Pointed_Node :=
              Find_Component_Classifier
                (Root,
                 Namespace_Identifier (Node),
                 Identifier (Node));

            if Present (Pointed_Node) then
               case AADL_Version is
                  when AADL_V1 =>
                     if Category (Pointed_Node) = Component_Cat (Node) then
                        Set_Referenced_Entity (Node, Pointed_Node);
                        Success := True;
                     else
                        DLTWN (Node, Pointed_Node);
                        Success := False;
                     end if;

                  when AADL_V2 =>
                     Set_Referenced_Entity (Node, Pointed_Node);
                     Success := True;
               end case;
            else
               DLTWN (Node, Pointed_Node);
               Success := False;
            end if;

            Push_Scope (Local_Scope);

         when K_Reference_Term =>
            case Kind (Reference_Term (Node)) is
               when K_Entity_Reference =>
                  --  AADL_V1

                  List_Node := First_Node (Path (Reference_Term (Node)));

               when K_Contained_Element_Path =>
                  --  AADL_V2

                  List_Node := First_Node (List_Items (Reference_Term (Node)));

               when others =>
                  raise Program_Error;
            end case;

            Pointed_Node := Container;

            while Present (List_Node) loop
               case Kind (Pointed_Node) is
                  when K_Subcomponent     |
                    K_Port_Spec           |
                    K_Feature_Group_Spec  |
                    K_Parameter           |
                    K_Subcomponent_Access |
                    K_Subprogram_Spec     |
                    K_Subprogram_Call     =>

                     --  For subclauses that can refer to a component,
                     --  we retrieve the corresponding entity.

                     Corresponding_Container :=
                       Get_Referenced_Entity (Entity_Ref (Pointed_Node));

                  when K_Mode                        |
                    K_Connection                     |
                    K_Flow_Spec                      |
                    K_Flow_Implementation            |
                    K_Flow_Implementation_Refinement |
                    K_End_To_End_Flow_Spec           |
                    K_End_To_End_Flow_Refinement     =>

                     --  For the subclauses, the container is the
                     --  component they have been declared in.

                     Corresponding_Container :=
                       Container_Component (Pointed_Node);

                  when K_Component_Type | K_Component_Implementation =>

                     Corresponding_Container := Pointed_Node;

                  when others =>
                     Corresponding_Container := No_Node;
               end case;

               if Present (Corresponding_Container) then
                  case AADL_Version is
                     when AADL_V1 =>
                        Pointed_Node :=
                          Find_Subclause
                            (Corresponding_Container,
                             Item (List_Node));
                     when AADL_V2 =>
                        --  Search in subclause

                        Pointed_Node :=
                          Find_Subclause (Corresponding_Container, List_Node);

                        --  then in features

                        if No (Pointed_Node) then
                           Pointed_Node :=
                             Find_Feature (Corresponding_Container, List_Node);
                        end if;

                        --  in the case of property applied directly
                        --  to a subcomponent, then we need to check
                        --  whether the property refers to a colocated
                        --  subcomponent.

                        if No (Pointed_Node)
                          and then Present
                            (Scope_Entity (Identifier (Container)))
                        then
                           Pointed_Node :=
                             Find_Subcomponent
                               (Corresponding_Entity
                                  (Scope_Entity (Identifier (Container))),
                                List_Node);
                        end if;
                  end case;

               else
                  Pointed_Node := No_Node;
               end if;

               if Present (Pointed_Node) then
                  case AADL_Version is
                     when AADL_V1 =>
                        Set_Corresponding_Entity
                          (Item (List_Node),
                           Pointed_Node);
                     when AADL_V2 =>
                        Set_Corresponding_Entity (List_Node, Pointed_Node);
                  end case;
                  List_Node := Next_Node (List_Node);
               else
                  DLTWN (Node, Pointed_Node);
                  Success := False;
                  exit;
               end if;
            end loop;

            if No (Pointed_Node) then
               DLTWN (Node, Pointed_Node);
               Success := False;
            else
               Set_Referenced_Entity (Reference_Term (Node), Pointed_Node);
               Success := True;
            end if;

         when K_Unique_Property_Const_Identifier | K_Property_Term =>
            if Kind (Container) = K_Component_Type
              or else Kind (Container) = K_Component_Implementation
              or else Kind (Container) = K_Feature_Group_Type
            then
               Pointed_Node :=
                 Find_Property_Association
                   (Container,
                    Name (Identifier (Node)));

            --  We first look for a property association if we are
            --  in an AADL declaration.

            else
               Pointed_Node := No_Node;
            end if;

            if No (Pointed_Node) then
               --  If we did not find anything, we look for a property
               --  name.

               Pointed_Node :=
                 Find_Property_Entity
                   (Root,
                    Property_Set_Identifier (Node),
                    Identifier (Node));
            end if;

            --  If we did not find anything, we look for an enumeration in
            --  properties

            if Present (Property_Type)
              and then (Kind (Property_Type) = K_Property_Type
                          or else Kind (Property_Type)
                          = K_Unique_Property_Type_Identifier
                          or else Kind (Property_Type)
                          = K_Constant_Property_Declaration)
            then
               if Kind (Property_Type) = K_Property_Type
                 and then Present (Property_Type_Designator (Property_Type))
               then
                  Type_Designator := Property_Type_Designator (Property_Type);

               elsif Kind (Property_Type)
                 = K_Constant_Property_Declaration
               then
                  Type_Designator :=
                    Constant_Type (Property_Type);

               else
                  Type_Designator := Property_Type_Designator
                    (Entity (Property_Type));
               end if;

               if No (Pointed_Node) then
                  if (Kind (Type_Designator)
                        = K_Unique_Property_Type_Identifier
                        or else Kind (Type_Designator) = K_Enumeration_Type)
                  then
                     Pointed_Node :=
                       Find_Property_Enumeration
                       (Root,
                        Container,
                        Property_Container,
                        Node,
                        Type_Designator);

                  elsif Kind (Type_Designator) = K_Record_Type_Element then
                     raise Program_Error;

                  else
                     raise Program_Error;
                  end if;
               end if;
            end if;

            if Present (Property_Type)
              and then Kind (Property_Type) = K_Record_Type_Element
            then
               declare
                  Unit_Node : Node_Id := Node;
               begin
                  if No (Entity (Property_Type_Designator (Property_Type)))
                  then
                     Success := Success
                       and then Link_Property_Type_Name
                       (Root, Property_Type,
                        Property_Type_Designator (Property_Type));
                  end if;
                  --  When processing a record_term_element, we
                  --  iterate over the property designator until we
                  --  find the corresponding entity to operate on.

                  if Kind (Property_Type_Designator
                             (Entity (Property_Type_Designator
                                        (Property_Type)))) = K_Record_Type
                  then
                     List_Node := First_Node
                       (List_Items
                          (Property_Type_Designator
                             (Entity
                                (Property_Type_Designator (Property_Type)))));
                     while Present (List_Node) loop
                        --  A property type is a list of record_type element
                        --  XXX should use case insensitive match ?

                        if Ocarina.ME_AADL.AADL_Tree.Nodes.Display_Name
                          (Identifier (List_Node)) =
                          Display_Name (Identifier (Property_Container))
                        then
                           Unit_Node := Property_Type_Designator (List_Node);

                           declare
                              Toto : Node_Id;
                           begin
                              Toto := Find_Property_Entity
                                (Root                    => Root,
                                 Property_Set_Identifier =>
                                   Property_Set_Identifier (Unit_Node),
                                 Property_Identifier =>
                                   Identifier (Unit_Node));

                              if No (Entity (Unit_Node)) then
                                 Set_Entity (Unit_Node, Toto);
                              end if;
                           end;
                        end if;

                        List_Node := Next_Node (List_Node);
                     end loop;
                  end if;

                  Success := Success and then Link_Property_Value
                    (Root,
                     Container,
                     Property_Container,
                     Node,
                     Unit_Node);
               end;
            end if;

            if Present (Pointed_Node)
              and then
              (Kind (Pointed_Node) = K_Property_Association
               or else Kind (Pointed_Node) = K_Constant_Property_Declaration
               or else Kind (Pointed_Node) = K_Property_Type
               or else Kind (Pointed_Node) = K_Property_Definition_Declaration
               or else Kind (Pointed_Node) = K_Property_Type_Declaration)
            then
               Set_Referenced_Entity (Node, Pointed_Node);

            --  IMPORTANT: we do not perform any verification
            --  regarding the validity of this reference: a single
            --  value property association may refer to a
            --  multi-valued constant. This is checked in the
            --  semantics packages.

            elsif Present (Pointed_Node)
              and then Kind (Pointed_Node) = K_Record_Term_Element
            then
               Set_Entity (Node, Pointed_Node);

            elsif Present (Property_Type)
              and then Kind (Property_Type) = K_Record_Type_Element
            then
               null;

            else

               DLTWN (Node, Pointed_Node);
               Success := False;
            end if;

         when K_Number_Range_Term =>
            Success :=
              Link_Property_Value
                (Root,
                 Container,
                 Property_Container,
                 Lower_Bound (Node),
                 Property_Type);
            Success :=
              Link_Property_Value
                (Root,
                 Container,
                 Property_Container,
                 Upper_Bound (Node),
                 Property_Type)
              and then Success;

            if Delta_Term (Node) /= No_Node then
               Success :=
                 Link_Property_Value
                   (Root,
                    Container,
                    Property_Container,
                    Delta_Term (Node),
                    Property_Type)
                 and then Success;
            end if;

         when K_Minus_Numeric_Term =>
            Success :=
              Link_Property_Value
                (Root,
                 Container,
                 Property_Container,
                 Numeric_Term (Node),
                 Property_Type);

         when K_Literal =>
            Success := True;

         --  Boolean terms must be followed to look for boolean
         --  property terms.

         when K_And_Boolean_Term | K_Or_Boolean_Term =>
            Success :=
              Link_Property_Value
                (Root,
                 Container,
                 Property_Container,
                 First_Term (Node),
                 Property_Type);
            Success :=
              Link_Property_Value
                (Root,
                 Container,
                 Property_Container,
                 Second_Term (Node),
                 Property_Type)
              and then Success;

         when K_Not_Boolean_Term | K_Parenthesis_Boolean_Term =>
            Success :=
              Link_Property_Value
                (Root,
                 Container,
                 Property_Container,
                 Boolean_Term (Node),
                 Property_Type);

         when K_Signed_AADLNumber =>
            --  If the number has a unit identifier, link it to the
            --  corresponding identifier of the unit type. We do not
            --  perform this test if the property type was not found
            --  to avoid cascading errors.

            if Present (Property_Type) then
               declare
                  Unit_Type : Node_Id := No_Node;

                  V_Unit_Id : constant Node_Id := Unit_Identifier (Node);
                  Unit_Id               : Node_Id;
                  Compatible_Unit_Types : Boolean          := False;
               begin
                  if Kind (Property_Container) = K_Record_Term_Element then
                     --  When processing a record_term_element, we
                     --  iterate over the property designator until we
                     --  find the corresponding entity to operate on.

                     if Present (V_Unit_Id) and then
                       Kind (Property_Type_Designator
                               (Entity
                                  (Property_Type_Designator
                                     (Property_Type)))) = K_Record_Type
                     then
                        List_Node := First_Node
                          (List_Items
                             (Property_Type_Designator
                                (Entity
                                   (Property_Type_Designator
                                      (Property_Type)))));

                        while Present (List_Node) loop
                           --  A property type is a list of record_type element
                           if Ocarina.ME_AADL.AADL_Tree.Nodes.Name
                             (Identifier (List_Node)) =
                             Name (Identifier (Property_Container))
                           then
                              Unit_Type := Unwind_Units_Type (Root, List_Node);
                              exit;
                           end if;
                           List_Node := Next_Node (List_Node);
                        end loop;
                     else
                        if Present (V_Unit_Id) then
                           Unit_Type := Unwind_Units_Type
                             (Root, Property_Type_Designator
                                (Entity
                                   (Property_Type_Designator
                                      (Property_Type))));
                        end if;
                     end if;
                  else
                     Unit_Type := Unwind_Units_Type (Root, Property_Type);
                  end if;

                  if Present (Unit_Identifier (Node))
                    and then Present (Property_Type)
                  then
                     if No (Unit_Type) then
                        DAE
                          (Node1    => Node,
                           Message1 => " is a unit literal but ",
                           Node2    => Property_Type,
                           Message2 => " is not a unit type");
                        Success := False;
                     end if;

                     if Success then
                        --  Link the units identifier of the value to
                        --  its corresponding units identifier.

                        Unit_Id := Base_Identifier (Unit_Type);

                        if Equals (V_Unit_Id, Unit_Id) then
                           Set_Corresponding_Entity (V_Unit_Id, Unit_Id);
                           Compatible_Unit_Types := True;
                        else
                           Unit_Id :=
                             First_Node (Unit_Definitions (Unit_Type));

                           while Present (Unit_Id) loop
                              if Equals (V_Unit_Id, Identifier (Unit_Id)) then
                                 Set_Corresponding_Entity
                                   (V_Unit_Id,
                                    Identifier (Unit_Id));
                                 Compatible_Unit_Types := True;
                              end if;

                              Unit_Id := Next_Node (Unit_Id);
                           end loop;
                        end if;

                        if not Compatible_Unit_Types then
                           DAE
                             (Node1    => Node,
                              Message1 => " and ",
                              Node2    => Property_Type,
                              Message2 => " have incompatible unit types");
                           Success := False;
                        end if;
                     end if;

                  elsif Present (Unit_Type) then
                     --  We accept that the user does not give the
                     --  unit for a literal only in the case where the
                     --  units type contains only one unit identifier.

                     if not Is_Empty (Unit_Definitions (Unit_Type)) then
                        DAE
                          (Node1    => Node,
                           Message1 => " is not a unit literal but ",
                           Node2    => Property_Type,
                           Message2 => " is a unit type");
                        Success := False;
                     end if;
                  end if;
               end;
            end if;

         when K_Record_Term =>
            declare
               J : Node_Id := First_Node (List_Items (Node));
               Actual_Property_Type : Node_Id := Property_Type;
            begin
               if Kind (Property_Type_Designator (Property_Type))
                 = K_Record_Type
               then
                  Actual_Property_Type :=
                    First_Node
                    (List_Items (Property_Type_Designator (Property_Type)));
               end if;

               while Present (J) loop
                  Success := Success and then Link_Property_Value
                    (Root,
                     Container,
                     J, --  Use current record_term_element as property
                        --  container
                     Property_Expression (J),
                     Actual_Property_Type);

                  J := Next_Node (J);
                  if Kind (Property_Type_Designator (Property_Type))
                    = K_Record_Type
                  then
                     Actual_Property_Type := Next_Node (Actual_Property_Type);
                  end if;
               end loop;
            end;

         when others =>
            null;
      end case;

      return Success;
   end Link_Property_Value;

   -------------------------------------------------------
   -- Link_Subclauses_In_Components_And_Features_Groups --
   -------------------------------------------------------

   function Link_Subclauses_In_Components_And_Feature_Groups
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
                    Link_Component_Type_Subclauses (Root, List_Node)
                    and then Success;

               when K_Component_Implementation =>
                  Success :=
                    Link_Component_Implementation_Subclauses (Root, List_Node)
                    and then Success;

               when K_Feature_Group_Type =>
                  Success :=
                    Link_Feature_Group_Type_Subclauses (Root, List_Node)
                    and then Success;

               when K_Package_Specification =>
                  Push_Scope (Entity_Scope (List_Node));

                  if not Is_Empty (Declarations (List_Node)) then
                     Package_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Present (Package_List_Node) loop
                        case Kind (Package_List_Node) is
                           when K_Component_Type =>
                              Success :=
                                Link_Component_Type_Subclauses
                                  (Root,
                                   Package_List_Node)
                                and then Success;

                           when K_Component_Implementation =>
                              Success :=
                                Link_Component_Implementation_Subclauses
                                  (Root,
                                   Package_List_Node)
                                and then Success;

                           when K_Feature_Group_Type =>
                              Success :=
                                Link_Feature_Group_Type_Subclauses
                                  (Root,
                                   Package_List_Node)
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
   end Link_Subclauses_In_Components_And_Feature_Groups;

   -----------------------
   -- Link_Subcomponent --
   -----------------------

   function Link_Subcomponent
     (Root : Node_Id;
      Node : Node_Id) return Boolean
   is
      use Ocarina.Analyzer.AADL.Semantics;
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Kind (Node) = K_Subcomponent);

      Success      : Boolean := True;
      Pointed_Node : Node_Id;
      No_Ref_Given : Boolean := False;
      --  The reference is optional

      Component_Ref        : constant Node_Id := Entity_Ref (Node);
      Component_Identifier : Node_Id;
      Pack_Identifier      : Node_Id;
   begin
      if Present (Component_Ref) then
         Component_Identifier := Identifier (Component_Ref);

         --  XXX We only link with the component type, no matter if an
         --  implementation is specified. The tree structure should be
         --  changed so that a Classifier_Ref has a single identifier
         --  instead of two.

         Pack_Identifier := Namespace_Identifier (Component_Ref);

         if AADL_Version = AADL_V2
           and then Present (Namespace_Identifier (Component_Ref))
         then
            Success :=
              Check_Qualified_References
                (Namespace (Container_Component (Node)),
                 Component_Ref);
         end if;

         Pointed_Node :=
           Find_Component_Classifier
             (Root                 => Root,
              Package_Identifier   => Pack_Identifier,
              Component_Identifier => Component_Identifier);

         --  If no package name is given, we try again in the current
         --  package forcing explictly the name of the package.

         if No (Pointed_Node)
           and then No (Pack_Identifier)
           and then Present (Namespace (Container_Component (Node)))
           and then Kind (Namespace (Container_Component (Node)))
           /= K_AADL_Specification
         then
            Pack_Identifier := Identifier
              (Namespace (Container_Component (Node)));

            Pointed_Node :=
              Find_Component_Classifier
                (Root                 => Root,
                 Package_Identifier   => Pack_Identifier,
                 Component_Identifier => Component_Identifier);
         end if;

         if No (Pointed_Node) then
            DLTWN (Node, Component_Ref, Non_Existent => True);
            Success := False;
         end if;

      else
         Pointed_Node := No_Node;
         No_Ref_Given := True;
      end if;

      if Success then
         if No (Pointed_Node) then
            if No_Ref_Given then
               Success := True;
            end if;

         elsif not
           ((Kind (Pointed_Node) = K_Component_Type
               or else Kind (Pointed_Node) = K_Component_Implementation))
         then
            DAE
              (Node1    => Node,
               Message1 => " points to ",
               Node2    => Pointed_Node,
               Message2 => ", which is not a component ");
            Success := False;

         elsif Is_Refinement (Node)
           and then Category (Node) = Component_Category'Pos (CC_Abstract)
           and then Category (Pointed_Node) /= Category (Node)
         then
            DAE
              (Node1    => Node,
               Message1 => " cannot be refined into an abstract");
            Success := False;

         elsif Category (Pointed_Node) /= Category (Node)
           and then
           Category (Pointed_Node) /=
           Component_Category'Pos (CC_Abstract)
         then
            DAE
              (Node1    => Node,
               Message1 => " points to ",
               Node2    => Pointed_Node,
               Message2 => ", which is not of the same kind ");
            Success := False;

         else
            Set_Referenced_Entity (Entity_Ref (Node), Pointed_Node);
            Success := True;
         end if;
      end if;

      return Success;
   end Link_Subcomponent;

   --------------------------
   -- Link_Type_Designator --
   --------------------------

   function Link_Type_Designator
     (Root       : Node_Id;
      Designator : Node_Id) return Boolean
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert
        (Kind (Designator) = K_Boolean_Type
         or else Kind (Designator) = K_String_Type
         or else Kind (Designator) = K_Enumeration_Type
         or else Kind (Designator) = K_Units_Type
         or else Kind (Designator) = K_Real_Type
         or else Kind (Designator) = K_Integer_Type
         or else Kind (Designator) = K_Classifier_Type
         or else Kind (Designator) = K_Reference_Type
         or else Kind (Designator) = K_Range_Type
         or else Kind (Designator) = K_Record_Type
         or else Kind (Designator) = K_Record_Type_Element);

      Success      : Boolean := True;
      Pointed_Node : Node_Id;
   begin
      case Kind (Designator) is
         when K_Integer_Type | K_Real_Type =>
            if Present (Unit_Designator (Designator))
              and then
                Kind (Unit_Designator (Designator)) =
                K_Unique_Property_Type_Identifier
            then
               Pointed_Node :=
                 Find_Property_Entity
                   (Root                    => Root,
                    Property_Set_Identifier =>
                      Property_Set_Identifier (Unit_Designator (Designator)),
                    Property_Identifier =>
                      Identifier (Unit_Designator (Designator)));

               if No (Pointed_Node) then
                  DAE
                    (Node1    => Unit_Designator (Designator),
                     Message1 => "does not point to anything");
                  Success := False;

               elsif Kind (Pointed_Node) /= K_Property_Type_Declaration then
                  DAE
                    (Node1    => Unit_Designator (Designator),
                     Message1 => " points to ",
                     Node2    => Pointed_Node,
                     Message2 => ", which is not a unit declaration");
                  Success := False;

               else
                  Set_Referenced_Entity
                    (Unit_Designator (Designator),
                     Pointed_Node);
               end if;
            end if;

            if Present (Type_Range (Designator)) then
               Success :=
                 Link_Property_Value
                   (Root,
                    Corresponding_Entity (Current_Scope),
                    Designator,
                    Lower_Bound (Type_Range (Designator)),
                    Designator)
                 and then Success;

               Success :=
                 Link_Property_Value
                   (Root,
                    Corresponding_Entity (Current_Scope),
                    Designator,
                    Upper_Bound (Type_Range (Designator)),
                    Designator)
                 and then Success;
            end if;

         when K_Range_Type =>
            if Present (Number_Type (Designator)) then
               if Kind (Number_Type (Designator)) =
                 K_Unique_Property_Type_Identifier
               then
                  Pointed_Node :=
                    Find_Property_Entity
                      (Root                    => Root,
                       Property_Set_Identifier =>
                         Property_Set_Identifier (Number_Type (Designator)),
                       Property_Identifier =>
                         Identifier (Number_Type (Designator)));

                  if No (Pointed_Node) then
                     DAE
                       (Node1    => Number_Type (Designator),
                        Message1 => "does not point to anything");
                     Success := False;
                  else
                     Set_Referenced_Entity
                       (Number_Type (Designator),
                        Pointed_Node);
                  end if;

               elsif Kind (Number_Type (Designator)) = K_Integer_Type
                 or else Kind (Number_Type (Designator)) = K_Real_Type
               then
                  Success :=
                    Link_Type_Designator (Root, Number_Type (Designator));
               end if;
            end if;

         when others =>
            null;
      end case;

      return Success;
   end Link_Type_Designator;

   -----------------------------
   -- Retrieve_Connection_End --
   -----------------------------

   procedure Retrieve_Connection_End
     (Component          :     Node_Id;
      Connection_End     :     Node_Id;
      Corresponding_Node : out Node_Id;
      Is_Local           : out Boolean)
   is
      pragma Assert (Kind (Component) = K_Component_Implementation);
      pragma Assert (Kind (Connection_End) = K_Entity_Reference);

   begin
      if not Entity_Reference_Path_Has_Several_Elements (Connection_End) then
         --  The connection end is something like 'name'. It can
         --  either be a feature or a subcomponent. We begin searching
         --  a subcomponent, as they are the closest declarations

         Is_Local           := True;
         Corresponding_Node :=
           Find_Subcomponent
             (Component               => Component,
              Subcomponent_Identifier =>
                Item (First_Node (Path (Connection_End))));

         if No (Corresponding_Node) then
            Corresponding_Node :=
              Find_Feature
                (Component          => Component,
                 Feature_Identifier =>
                   Item (First_Node (Path (Connection_End))));
         end if;
         Set_Corresponding_Entity
           (Item (First_Node (Path (Connection_End))),
            Corresponding_Node);

      else
         --  The connection end is like 'name.name'. It is a feature
         --  of a subcomponent or a parameter of a subprogram call. We
         --  first find the subcomponent, and then the feature of the
         --  corresponding component type or implementation.

         Is_Local           := False;
         Corresponding_Node :=
           Find_Subcomponent
             (Component               => Component,
              Subcomponent_Identifier =>
                Item (First_Node (Path (Connection_End))));

         if No (Corresponding_Node) then
            --  If we did not find a suitable subcomponent, maybe it
            --  is a subprogram call.

            Corresponding_Node :=
              Find_Subprogram_Call
                (Component       => Component,
                 Call_Identifier => Item (First_Node (Path (Connection_End))));
         end if;

         if No (Corresponding_Node) then
            --  If we did not find a suitable subcomponent, maybe it
            --  is a port group in the feature section.

            Corresponding_Node :=
              Find_Feature
                (Component          => Component,
                 Feature_Identifier =>
                   Item (First_Node (Path (Connection_End))));
         end if;

         Set_Corresponding_Entity
           (Item (First_Node (Path (Connection_End))),
            Corresponding_Node);

         if Present (Corresponding_Node) then
            if Kind (Corresponding_Node) = K_Subcomponent and then
              Present
                (Get_Referenced_Entity (Entity_Ref (Corresponding_Node)))
            then
               Corresponding_Node :=
                 Find_Feature
                   (Component =>
                      Get_Referenced_Entity
                        (Entity_Ref (Corresponding_Node)),
                    Feature_Identifier =>
                      Item
                        (Next_Node (First_Node (Path (Connection_End)))));

            elsif Kind (Corresponding_Node) = K_Subprogram_Call
              and then Present
                (Get_Referenced_Entity (Entity_Ref (Corresponding_Node)))
            then
               Corresponding_Node :=
                 Find_Feature
                   (Component =>
                      Get_Referenced_Entity (Entity_Ref (Corresponding_Node)),
                    Feature_Identifier =>
                      Item (Next_Node (First_Node (Path (Connection_End)))));

            elsif Kind (Corresponding_Node) = K_Feature_Group_Spec
              and then Present (Entity_Ref (Corresponding_Node))
              and then Present
                (Get_Referenced_Entity (Entity_Ref (Corresponding_Node)))
            then
               Corresponding_Node :=
                 Find_Feature
                   (Component =>
                      Get_Referenced_Entity (Entity_Ref (Corresponding_Node)),
                    Feature_Identifier =>
                      Item (Next_Node (First_Node (Path (Connection_End)))));
               Is_Local := True;

            elsif Kind (Corresponding_Node) = K_Feature_Group_Spec
              and then Present (Inverse_Of (Corresponding_Node))
              and then Present
                (Get_Referenced_Entity (Inverse_Of (Corresponding_Node)))
            then
               Corresponding_Node :=
                 Find_Feature
                   (Component =>
                      Get_Referenced_Entity (Inverse_Of (Corresponding_Node)),
                    Feature_Identifier =>
                      Item (Next_Node (First_Node (Path (Connection_End)))));
               Is_Local := True;

            elsif Kind (Corresponding_Node) = K_Subcomponent_Access
              and then Present
                (Get_Referenced_Entity (Entity_Ref (Corresponding_Node)))
            then
               Corresponding_Node :=
                 Find_Feature
                   (Component =>
                      Get_Referenced_Entity (Entity_Ref (Corresponding_Node)),
                    Feature_Identifier =>
                      Item (Next_Node (First_Node (Path (Connection_End)))));

            else
               Corresponding_Node := No_Node;
            end if;

            Set_Corresponding_Entity
              (Item (Next_Node (First_Node (Path (Connection_End)))),
               Corresponding_Node);
         end if;
      end if;
   end Retrieve_Connection_End;

   -----------------------
   -- Unwind_Units_Type --
   -----------------------

   function Unwind_Units_Type
     (Root          : Node_Id;
      Property_Type : Node_Id) return Node_Id
   is

      function Recursive_Unwind_Units_Type
        (Property_Type : Node_Id) return Node_Id;

      ---------------------------------
      -- Recursive_Unwind_Units_Type --
      ---------------------------------

      function Recursive_Unwind_Units_Type
        (Property_Type : Node_Id) return Node_Id
      is
      begin
         --  To unwind the multiple level defined types, we use
         --  recusion.

         case Kind (Property_Type) is
            when K_Real_Type | K_Integer_Type =>
               --  This is the most basic case given on a first
               --  recursion level. Units are *always* defined in an
               --  integer type or a real type. If this type does not
               --  contain a unit designator, this means that there is
               --  no unit definition in the whole type definition.

               if Present (Unit_Designator (Property_Type)) then
                  return Recursive_Unwind_Units_Type
                    (Unit_Designator (Property_Type));

               else
                  return No_Node;
               end if;

            when K_Constant_Property_Declaration =>
               --  When unwinding a constant type declaration, we
               --  first look if the unit definition is given in the
               --  constant type declaration. Otherwise we unwind the
               --  constant type.

               if Present (Unique_Unit_Identifier (Property_Type)) then
                  return Recursive_Unwind_Units_Type
                      (Unique_Unit_Identifier (Property_Type));
               else
                  return Recursive_Unwind_Units_Type
                      (Constant_Type (Property_Type));
               end if;

            when K_Property_Type =>
               --  Unwind the property type designator

               return Recursive_Unwind_Units_Type
                   (Property_Type_Designator (Property_Type));

            --  All the cases below correspond to the intermediary
            --  recursion levels.

            when K_Range_Type =>
               --  Unwind the integer or the real type defined in the
               --  range type.

               return Recursive_Unwind_Units_Type
                   (Number_Type (Property_Type));

            when K_Unique_Property_Type_Identifier =>
               --  Link the property type because it may not be linked
               --  yet.

               declare
                  Pointed_Node : Node_Id;
               begin
                  if No (Entity (Property_Type)) then
                     Pointed_Node :=
                       Find_Property_Entity
                         (Root                    => Root,
                          Property_Set_Identifier =>
                            Property_Set_Identifier (Property_Type),
                          Property_Identifier => Identifier (Property_Type));

                     if No (Pointed_Node) then
                        DAE
                          (Node1    => Property_Type,
                           Message1 => "does not point to anything");
                        return No_Node;

                     elsif Kind (Pointed_Node) /=
                       K_Property_Type_Declaration
                     then
                        DAE
                          (Node1    => Property_Type,
                           Message1 => " points to ",
                           Node2    => Pointed_Node,
                           Message2 => ", which is not a property type");
                        return No_Node;

                     else
                        Set_Referenced_Entity (Property_Type, Pointed_Node);
                     end if;
                  end if;

                  if Kind (Get_Referenced_Entity (Property_Type)) /=
                    K_Property_Type_Declaration
                  then
                     DAE
                       (Node1    => Property_Type,
                        Message1 => "is not a property type");
                     return No_Node;
                  end if;

                  return Recursive_Unwind_Units_Type
                      (Property_Type_Designator
                         (Get_Referenced_Entity (Property_Type)));
               end;

            when K_Units_Type =>
               return Property_Type;

            --  No match, there is no units type definition

            when K_Record_Term_Element =>
               return Recursive_Unwind_Units_Type
                 (Property_Expression (Property_Type));

            when K_Number_Range_Term => --  XXX
               declare
                  First : constant Node_Id
                    := Recursive_Unwind_Units_Type
                    (Lower_Bound (Property_Type));
               begin
                  return First;
               end;

            when K_Signed_Aadlnumber =>
               if No (Unit_Identifier (Property_Type)) then
                  return No_Node;
               else
                  return No_Node;
               end if;

            when K_Record_Type_Element =>
               return Recursive_Unwind_Units_Type
                 (Property_Type_Designator (Property_Type));

            when others =>
               return No_Node;

         end case;
      end Recursive_Unwind_Units_Type;

   begin
      return Recursive_Unwind_Units_Type (Property_Type);
   end Unwind_Units_Type;

   ---------------------------
   -- Link_Flow_Connections --
   ---------------------------

   function Link_Flow_Connections (Flow : Node_Id) return Boolean is
      pragma Assert
        (Kind (Flow) = K_Flow_Implementation
         or else Kind (Flow) = K_End_To_End_Flow_Spec);

      Connections : constant List_Id :=
        Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Flow);
      Component : constant Node_Id := Container_Component (Flow);
      C         : Flow_Category;
      Cnx_Cat   : Connection_Type;

      Begin_Subclause : Node_Id;
      --  The feature or subcomponent connected to the begin of the
      --  list (if any).

      End_Subclause : Node_Id;
      --  The feature or subcomponent connected to the end of the list
      --  (if any).

      Previous_Subclause : Node_Id;
      Next_Subclause     : Node_Id;

      Fetch_Connection : Boolean;
      --  True iff the expected node in the list is a reference to a
      --  connection.

      Success      : Boolean := True;
      List_Node    : Node_Id;
      Pointed_Node : Node_Id;
   begin
      if Kind (Flow) = K_End_To_End_Flow_Spec then
         --  The pattern for the connection list is the following:
         --  connection -> flow -> connection -> flow -> ... ->
         --  connection. The parsing phase ensured the odd number.

         Fetch_Connection := True;

         Begin_Subclause :=
           Identifier
             (Corresponding_Entity
                (Item (First_Node (Path (Source_Flow (Flow))))));
         End_Subclause :=
           Identifier
             (Corresponding_Entity
                (Item (First_Node (Path (Sink_Flow (Flow))))));

      else
         --  The pattern of the remaining elements in the flow chain
         --  is an alternantion of connections and subcomponent
         --  flows. The nature of the beginning depends on the flow
         --  nature. The number of elements is automatically handled
         --  by the parser.

         C :=
           Flow_Category'Val (Ocarina.ME_AADL.AADL_Tree.Nodes.Category (Flow));

         case C is
            when FC_Source =>
               Fetch_Connection := False;

               Begin_Subclause := No_Node;
               End_Subclause   :=
                 Identifier
                   (Corresponding_Entity
                      (Item (First_Node (Path (Source_Flow (Flow))))));

            when FC_Sink =>
               Fetch_Connection := True;

               Begin_Subclause :=
                 Identifier
                   (Corresponding_Entity
                      (Item (First_Node (Path (Sink_Flow (Flow))))));
               End_Subclause := No_Node;

            when FC_Path =>
               Fetch_Connection := True;

               Begin_Subclause :=
                 Identifier
                   (Corresponding_Entity
                      (Item (First_Node (Path (Source_Flow (Flow))))));
               End_Subclause :=
                 Identifier
                   (Corresponding_Entity
                      (Item (First_Node (Path (Sink_Flow (Flow))))));
         end case;
      end if;

      if not Is_Empty (Connections) then
         List_Node := First_Node (Connections);

         Previous_Subclause := Begin_Subclause;

         --  If the list begins with a connection (Present
         --  (Begin_Subclause)) Next_Subclause is the second element
         --  or else End_Subclause. Otherwise Next_Subclause will be
         --  set automatically inside the loop.

         if Present (Previous_Subclause) then
            if Present (Next_Node (List_Node)) then
               Next_Subclause :=
                 Item (First_Node (Path (Next_Node (List_Node))));
            else
               Next_Subclause := End_Subclause;
            end if;
         end if;

         while Present (List_Node) loop
            if Fetch_Connection then
               Pointed_Node :=
                 Find_Connection
                   (Connection_Identifier =>
                      Item (First_Node (Path (List_Node))),
                    Component => Component,
                    In_Modes  => In_Modes (Flow));

               Set_Corresponding_Entity
                 (Item (First_Node (Path (List_Node))),
                  Pointed_Node);
               Set_Referenced_Entity (List_Node, Pointed_Node);
               Display_Node_Link (List_Node, Pointed_Node);

               if No (Pointed_Node) then
                  DAE
                    (Node1    => List_Node,
                     Message1 =>
                       " does not point to a connection" &
                       " active in the flow modes");
                  Success := False;
               else
                  --  Check that the connection souce and destination
                  --  are coherent with the flow chain.

                  --  We just compare Name_Id's because we are in
                  --  the same scope.

                  pragma Assert (Present (Previous_Subclause));
                  pragma Assert
                    (Present (Source (Pointed_Node))
                     and then not Is_Empty (Path (Source (Pointed_Node)))
                     and then Present
                       (First_Node (Path (Source (Pointed_Node)))));

                  pragma Assert (Present (Next_Subclause));
                  pragma Assert
                    (Present (Destination (Pointed_Node))
                     and then not Is_Empty (Path (Destination (Pointed_Node)))
                     and then Present
                       (First_Node (Path (Destination (Pointed_Node)))));

                  Cnx_Cat := Connection_Type'Val (Category (Pointed_Node));

                  if Cnx_Cat /= CT_Feature_Group then
                     --  In case of a classic connection, we just
                     --  check that the source of the connection is
                     --  pointing to the prefious element and that the
                     --  destination of the connection is pointing to
                     --  the next element.

                     if Name (Previous_Subclause) /=
                       Name (Item (First_Node (Path (Source (Pointed_Node)))))
                     then
                        DAE
                          (Node1    => List_Node,
                           Message1 =>
                             " points to a connection whose" &
                             " source is not pointing to the previous" &
                             " element in the flow");
                        Success := False;
                     end if;

                     if Name (Next_Subclause) /=
                       Name
                         (Item
                            (First_Node (Path (Destination (Pointed_Node)))))
                     then
                        DAE
                          (Node1    => List_Node,
                           Message1 =>
                             " points to a connection whose" &
                             " destination is not pointing to the next" &
                             " element in the flow");
                        Success := False;
                     end if;
                  else
                     --  Group connections may be bidirectional.
                     --  Therefore we need to check both ends and see
                     --  whether the found connection end have
                     --  features which are of coherent mode (in, out
                     --  or inout).

                     if Name (Previous_Subclause) /=
                       Name (Item (First_Node (Path (Source (Pointed_Node)))))
                     then
                        if Name (Previous_Subclause) /=
                          Name
                            (Item
                               (First_Node
                                  (Path (Destination (Pointed_Node)))))
                        then
                           DAE
                             (Node1    => List_Node,
                              Message1 =>
                                " points to a connection whose" &
                                " source is not pointing to the" &
                                " previous element in the flow");
                           Success := False;
                        end if;
                     end if;

                     if Name (Next_Subclause) /=
                       Name
                         (Item
                            (First_Node (Path (Destination (Pointed_Node)))))
                     then
                        if Name (Next_Subclause) /=
                          Name
                            (Item (First_Node (Path (Source (Pointed_Node)))))
                        then
                           DAE
                             (Node1    => List_Node,
                              Message1 =>
                                " points to a connection whose" &
                                " destination is not pointing to the" &
                                " next element in the flow");
                           Success := False;
                        end if;
                     end if;
                  end if;
               end if;

               Display_Node_Link (List_Node, Pointed_Node);
            else
               Pointed_Node :=
                 Link_Flow_Of_Subcomponent
                   (Component       => Component,
                    Flow_Identifier => List_Node,
                    In_Modes        => In_Modes (Flow));

               if No (Pointed_Node) then
                  DAE
                    (Node1    => List_Node,
                     Message1 =>
                       " does not point to a subcomponent" &
                       " active in the flow modes");
                  Success := False;
               elsif Kind (Flow) = K_End_To_End_Flow_Spec
                 and then
                   Flow_Category'Val (Category (Pointed_Node)) /=
                   FC_Path
               then
                  DAE
                    (Node1    => List_Node,
                     Message1 => " points to ",
                     Node2    => Pointed_Node,
                     Message2 => " which should be a flow path");
                  Success := False;
               end if;

               --  For the next connection, the previous subclause
               --  is the current subcomponent and the next
               --  subclause is the subclause pointed by the next
               --  subcomponent or else End_Subclause

               Previous_Subclause := Item (First_Node (Path (List_Node)));

               if Present (Next_Node (List_Node))
                 and then Present (Next_Node (Next_Node (List_Node)))
               then
                  Next_Subclause :=
                    Item
                      (First_Node (Path (Next_Node (Next_Node (List_Node)))));
               else
                  Next_Subclause := End_Subclause;
               end if;
            end if;

            --  Update the pattern

            Fetch_Connection := not Fetch_Connection;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Link_Flow_Connections;

end Ocarina.Analyzer.AADL.Links;
