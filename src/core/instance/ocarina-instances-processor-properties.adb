------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.INSTANCES.PROCESSOR.PROPERTIES                   --
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

with Ocarina.AADL_Values;
with Ocarina.Instances.Finder;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nodes;

package body Ocarina.Instances.Processor.Properties is

   use Ocarina.AADL_Values;
   use Ocarina.Instances.Finder;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Entities;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;

   function Evaluate_Property_Value
     (Instance_Root  : Node_Id;
      Container      : Node_Id;
      Property_Value : Node_Id) return Node_Id;
   --  Compute the value by fetching the property terms and applying
   --  the operations described in the property value. Return the
   --  first evaluated value (the following ones are chainded using
   --  Next_Node), or No_Node if nothing could be evaluated.

   procedure Expand_Property_Value
     (Instance_Root :     Node_Id;
      Container     :     Node_Id;
      Property      :     Node_Id;
      Result_Node   : out Node_Id;
      Result_List   : out List_Id);
   --  Expand the property terms of the property, and return all the
   --  property values. If the property is a list property then the
   --  result will be returned in Result_List and Result_Node will be
   --  set to No_Node. Otherwise, the result will be returned in
   --  Result_Node and Result_List will be set to No_List.

   procedure Resolve_Values
     (Root      : Node_Id;
      Container : Node_Id;
      Property  : Node_Id);
   --  Set a direct value (i.e. without value ()) for each value of
   --  the property association.

   procedure Resolve_Property_Associations
     (Properties : List_Id;
      Root       : Node_Id;
      Container  : Node_Id);

   procedure Resolve_Properties_Of_Component_Instance
     (Root      : Node_Id;
      Component : Node_Id);

   procedure Resolve_Properties_Of_Namespace_Instance
     (Root          : Node_Id;
      The_Namespace : Node_Id);

   --------------------------------------
   -- Compute_Property_Instance_Values --
   --------------------------------------

   procedure Compute_Property_Instance_Values (Root : Node_Id) is
      pragma Assert (Kind (Root) = K_Architecture_Instance);

      List_Node : Node_Id;
   begin
      Resolve_Properties_Of_Component_Instance
        (Root      => Root,
         Component => Root_System (Root));

      if Namespaces (Root) /= No_List then
         List_Node := AIN.First_Node (AIN.Namespaces (Root));

         while Present (List_Node) loop
            Resolve_Properties_Of_Namespace_Instance
              (Root          => Root,
               The_Namespace => List_Node);
            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;
   end Compute_Property_Instance_Values;

   --------------------
   -- Resolve_Values --
   --------------------

   procedure Resolve_Values
     (Root      : Node_Id;
      Container : Node_Id;
      Property  : Node_Id)
   is
      pragma Assert (Kind (Root) = K_Architecture_Instance);
      pragma Assert (AIN.Kind (Property) = K_Property_Association_Instance);
      pragma Assert (Present (Container));

      Prop_Value : constant Node_Id :=
        AIN.Property_Association_Value (Property);
      Expanded_Node : Node_Id;
      Expanded_List : List_Id;
   begin
      Expand_Property_Value
        (Root,
         Container,
         Property,
         Expanded_Node,
         Expanded_List);

      Set_Expanded_Single_Value (Prop_Value, Expanded_Node);
      Set_Expanded_Multi_Value (Prop_Value, Expanded_List);
   end Resolve_Values;

   ---------------------------
   -- Expand_Property_Value --
   ---------------------------

   procedure Expand_Property_Value
     (Instance_Root :     Node_Id;
      Container     :     Node_Id;
      Property      :     Node_Id;
      Result_Node   : out Node_Id;
      Result_List   : out List_Id)
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Present (Container));
      pragma Assert
        (No (Property)
         or else AIN.Kind (Property) = K_Property_Association_Instance
         or else ATN.Kind (Property) = K_Constant_Property_Declaration
         or else ATN.Kind (Property) = K_Property_Definition_Declaration);

      Value                : Node_Id;
      List_Node            : Node_Id;
      Computed_Value       : Node_Id;
      Evaluation_Container : Node_Id;
   begin
      Result_Node := No_Node;
      Result_List := No_List;

      if No (Property) then
         return;
      end if;

      --  First get the value of the property. For property
      --  associations, the evaluation context is the entity in which
      --  it has been declared.

      case AIN.Kind (Property) is
         when K_Property_Association_Instance =>
            Value                := AIN.Property_Association_Value (Property);
            Evaluation_Container :=
              Value_Container (AIN.Property_Association_Value (Property));
         --  when K_Constant_Property_Declaration =>
         --   Value := Constant_Value (Property);
         --   Evaluation_Container := Container;
         --  when K_Property_Definition_Declaration =>
         --   Value := Default_Value (Property);
         --   Evaluation_Container := Container;
         when others =>
            raise Program_Error;
      end case;

      if Present (Value) then
         if Present (Single_Value (Value)) then
            Result_Node :=
              Evaluate_Property_Value
                (Instance_Root  => Instance_Root,
                 Container      => Evaluation_Container,
                 Property_Value => Single_Value (Value));
         end if;

         if not ATNU.Is_Empty (Multi_Value (Value)) then
            Result_List := ATNU.New_List (K_List_Id, ATN.Loc (Value));
            List_Node   := ATN.First_Node (Multi_Value (Value));

            while Present (List_Node) loop
               Computed_Value :=
                 Evaluate_Property_Value
                   (Instance_Root  => Instance_Root,
                    Container      => Evaluation_Container,
                    Property_Value => List_Node);

               if Present (Computed_Value) then
                  --  The computed value may be No_Node if nothing is
                  --  pointed to. Then we simply ignore it. Else we
                  --  append the computed values.

                  ATNU.Append_Node_To_List (Computed_Value, Result_List);
               end if;

               List_Node := ATN.Next_Node (List_Node);
            end loop;
         end if;
      end if;
   end Expand_Property_Value;

   -----------------------------
   -- Evaluate_Property_Value --
   -----------------------------

   function Evaluate_Property_Value
     (Instance_Root  : Node_Id;
      Container      : Node_Id;
      Property_Value : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (No (Property_Value)
         or else Kind (Property_Value) = K_Literal
         or else Kind (Property_Value) = K_Property_Term
         or else Kind (Property_Value) = K_Enumeration_Term
         or else Kind (Property_Value) = K_Number_Range_Term
         or else Kind (Property_Value) = K_Reference_Term
         or else Kind (Property_Value) = K_Property_Term
         or else Kind (Property_Value) = K_Minus_Numeric_Term
         or else Kind (Property_Value) = K_Signed_AADLNumber
         or else Kind (Property_Value) = K_Not_Boolean_Term
         or else Kind (Property_Value) = K_And_Boolean_Term
         or else Kind (Property_Value) = K_Or_Boolean_Term
         or else Kind (Property_Value) = K_Parenthesis_Boolean_Term
         or else Kind (Property_Value) = K_Component_Classifier_Term
         or else Kind (Property_Value) = K_Record_Term);

      Evaluated_Value : Node_Id;
      Node            : Node_Id;
      Dummy           : List_Id;
      pragma Warnings (Off, Dummy); --  Not read in realease mode
   begin
      if No (Property_Value) then
         Evaluated_Value := No_Node;
      else
         case ATN.Kind (Property_Value) is
            when K_Literal =>
               --  We clone the literal value

               Evaluated_Value :=
                 ATNU.New_Node
                   (Kind (Property_Value),
                    ATN.Loc (Property_Value));
               Set_Value
                 (Evaluated_Value,
                  New_Value (Value (Value (Property_Value))));

            when K_Number_Range_Term =>
               declare
                  Evaluated_Lower_Bound : Node_Id;
                  Evaluated_Upper_Bound : Node_Id;
                  Evaluated_Delta_Term  : Node_Id;
               begin
                  Evaluated_Lower_Bound :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => Lower_Bound (Property_Value));
                  Evaluated_Upper_Bound :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => Upper_Bound (Property_Value));
                  Evaluated_Delta_Term :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => Delta_Term (Property_Value));

                  if Present (Evaluated_Lower_Bound)
                    and then Kind (Evaluated_Lower_Bound) = K_Literal
                    and then Present (Evaluated_Upper_Bound)
                    and then Kind (Evaluated_Upper_Bound) = K_Literal
                    and then Present (Evaluated_Delta_Term)
                    and then Kind (Evaluated_Delta_Term) = K_Literal
                  then
                     Evaluated_Value :=
                       ATNU.New_Node
                         (Kind (Property_Value),
                          ATN.Loc (Property_Value));

                     Set_Lower_Bound (Evaluated_Value, Evaluated_Lower_Bound);
                     Set_Upper_Bound (Evaluated_Value, Evaluated_Upper_Bound);
                     Set_Delta_Term (Evaluated_Value, Evaluated_Delta_Term);
                  else
                     Evaluated_Value := No_Node;
                  end if;
               end;

            when K_Reference_Term =>
               Evaluated_Value :=
                 ATNU.New_Node
                   (Kind (Property_Value),
                    ATN.Loc (Property_Value));
               Node :=
                 ATNU.New_Node
                   (Kind (Reference_Term (Property_Value)),
                    ATN.Loc (Reference_Term (Property_Value)));

               case AADL_Version is
                  when AADL_V1 =>
                     ATN.Set_Identifier
                       (Node,
                     --  XXX TODO here duplicate Identifier
                        ATN.Identifier (Reference_Term (Property_Value)));

                     ATN.Set_Path
                       (Node,
                        ATN.Path (Reference_Term (Property_Value)));
                     Set_Namespace_Path
                       (Node,
                        Namespace_Path (Reference_Term (Property_Value)));
                     Set_Namespace_Identifier
                       (Node,
                        ATE.Duplicate_Identifier
                          (Namespace_Identifier
                             (Reference_Term (Property_Value))));
                     Set_Referenced_Entity
                       (Node,
                        Find_Instance
                          (Instance_Root      => Instance_Root,
                           Reference_Instance => Container,
                           Path               =>
                             ATN.Path (Reference_Term (Property_Value))));

                  when AADL_V2 =>
                     Set_List_Items
                       (Node,
                        List_Items (Reference_Term (Property_Value)));
                     Set_Annex_Path
                       (Node,
                        Annex_Path (Reference_Term (Property_Value)));
                     Set_Referenced_Entity
                       (Node,
                        Find_Instance
                          (Instance_Root      => Instance_Root,
                           Reference_Instance => Container,
                           Path               =>
                             List_Items (Reference_Term (Property_Value))));
               end case;

               Set_Reference_Term (Evaluated_Value, Node);

            --  XXX Todo here evaluate Contained_Element_Path

            when K_Property_Term | K_Enumeration_Term =>
               Expand_Property_Value
                 (Instance_Root => Instance_Root,
                  Container     => Container,
                  Property      => ATE.Get_Referenced_Entity (Property_Value),
                  Result_Node   => Evaluated_Value,
                  Result_List   => Dummy);

               pragma Assert (Dummy = No_List);

            when K_Minus_Numeric_Term =>
               declare
                  Val     : Value_Type;
                  Literal : Node_Id;
               begin
                  Evaluated_Value :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => Numeric_Term (Property_Value));

                  pragma Assert (Dummy = No_List);

                  if Present (Evaluated_Value) then
                     if Kind (Evaluated_Value) = K_Literal then
                        Literal := Evaluated_Value;

                     elsif Kind (Evaluated_Value) = K_Signed_AADLNumber then
                        Literal := Number_Value (Evaluated_Value);
                     --  Since the number has been evaluated, the
                     --  number_value can only be a literal

                     else
                        Literal := No_Node;
                     end if;

                     if Present (Literal) then
                        if Get_Value_Type (Value (Literal)).T = LT_Integer then
                           Val       := Get_Value_Type (Value (Literal));
                           Val.ISign :=
                             not Get_Value_Type (Value (Literal)).ISign;
                           Set_Value (Value (Literal), Val);

                        elsif Get_Value_Type (Value (Literal)).T = LT_Real then
                           Val       := Get_Value_Type (Value (Literal));
                           Val.RSign := not Val.RSign;
                           Set_Value (Value (Literal), Val);

                        else
                           Evaluated_Value := No_Node;
                        end if;

                        if Present (Evaluated_Value) then
                           if Kind (Evaluated_Value) = K_Literal then
                              Evaluated_Value := Literal;

                           elsif Kind (Evaluated_Value) =
                             K_Signed_AADLNumber
                           then
                              Set_Number_Value (Evaluated_Value, Literal);
                           end if;
                        end if;

                     else
                        Evaluated_Value := No_Node;
                     end if;
                  end if;
               end;

            when K_Signed_AADLNumber =>
               declare
                  Evaluated_Number_Value : Node_Id;
               begin
                  Evaluated_Value :=
                    ATNU.New_Node
                      (Kind (Property_Value),
                       ATN.Loc (Property_Value));
                  Set_Unit_Identifier
                    (Evaluated_Value,
                     Unit_Identifier (Property_Value));
                  Evaluated_Number_Value :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => Number_Value (Property_Value));

                  if Present (Evaluated_Number_Value) then
                     --  XXX we should check the type
                     Set_Number_Value
                       (Evaluated_Value,
                        Evaluated_Number_Value);
                  else
                     Evaluated_Value := No_Node;
                  end if;
               end;

            when K_Not_Boolean_Term =>
               declare
                  Val : Value_Type;
               begin
                  Evaluated_Value :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => Boolean_Term (Property_Value));

                  if Present (Evaluated_Value)
                    and then Kind (Evaluated_Value) = K_Literal
                    and then
                      Get_Value_Type (Value (Evaluated_Value)).T =
                      LT_Boolean
                  then
                     Val      := Get_Value_Type (Value (Evaluated_Value));
                     Val.BVal := not Val.BVal;
                     Set_Value (Value (Evaluated_Value), Val);
                  else
                     Evaluated_Value := No_Node;
                  end if;
               end;

            when K_And_Boolean_Term =>
               declare
                  Auxiliary_Value : Node_Id;
                  Val             : Value_Type;
               begin
                  Evaluated_Value :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => First_Term (Property_Value));
                  Auxiliary_Value :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => Second_Term (Property_Value));

                  if Present (Evaluated_Value)
                    and then Kind (Evaluated_Value) = K_Literal
                    and then
                      Get_Value_Type (Value (Evaluated_Value)).T =
                      LT_Boolean
                    and then Present (Auxiliary_Value)
                    and then Kind (Auxiliary_Value) = K_Literal
                    and then
                      Get_Value_Type (Value (Auxiliary_Value)).T =
                      LT_Boolean
                  then
                     Val      := Get_Value_Type (Value (Evaluated_Value));
                     Val.BVal :=
                       Val.BVal
                       and then Get_Value_Type (Value (Auxiliary_Value)).BVal;
                     Set_Value (Value (Evaluated_Value), Val);
                  else
                     Evaluated_Value := No_Node;
                  end if;
               end;

            when K_Or_Boolean_Term =>
               declare
                  Auxiliary_Value : Node_Id;
                  Val             : Value_Type;
               begin
                  Evaluated_Value :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => First_Term (Property_Value));
                  Auxiliary_Value :=
                    Evaluate_Property_Value
                      (Instance_Root  => Instance_Root,
                       Container      => Container,
                       Property_Value => Second_Term (Property_Value));

                  if Present (Evaluated_Value)
                    and then Kind (Evaluated_Value) = K_Literal
                    and then
                      Get_Value_Type (Value (Evaluated_Value)).T =
                      LT_Boolean
                    and then Present (Auxiliary_Value)
                    and then Kind (Auxiliary_Value) = K_Literal
                    and then
                      Get_Value_Type (Value (Auxiliary_Value)).T =
                      LT_Boolean
                  then
                     Val      := Get_Value_Type (Value (Evaluated_Value));
                     Val.BVal :=
                       Val.BVal
                       or else Get_Value_Type (Value (Auxiliary_Value)).BVal;
                     Set_Value (Value (Evaluated_Value), Val);
                  else
                     Evaluated_Value := No_Node;
                  end if;
               end;

            when K_Parenthesis_Boolean_Term =>
               Evaluated_Value :=
                 Evaluate_Property_Value
                   (Instance_Root  => Instance_Root,
                    Container      => Container,
                    Property_Value => Boolean_Term (Property_Value));

               if Present (Evaluated_Value)
                 and then Kind (Evaluated_Value) = K_Literal
                 and then
                   Get_Value_Type (Value (Evaluated_Value)).T =
                   LT_Boolean
               then
                  null;
               else
                  Evaluated_Value := No_Node;
               end if;

            when K_Component_Classifier_Term =>
               Evaluated_Value :=
                 ATNU.New_Node
                   (Kind (Property_Value),
                    ATN.Loc (Property_Value));
               Set_Referenced_Entity
                 (Evaluated_Value,
                  ATE.Get_Referenced_Entity (Property_Value));
               Set_Component_Cat
                 (Evaluated_Value,
                  Component_Cat (Property_Value));

            when K_Record_Term =>
            --  Simply propagate the list to the instance

               Evaluated_Value :=
                 New_Node (Kind (Property_Value), ATN.Loc (Property_Value));
               Set_List_Items
                 (Evaluated_Value,
                  ATN.List_Items (Property_Value));

            when others =>
               raise Program_Error;
         end case;
      end if;

      return Evaluated_Value;
   end Evaluate_Property_Value;

   -----------------------------------
   -- Resolve_Property_Associations --
   -----------------------------------

   procedure Resolve_Property_Associations
     (Properties : List_Id;
      Root       : Node_Id;
      Container  : Node_Id)
   is
      pragma Assert (Present (Container));

      List_Node : Node_Id;
   begin
      if Properties /= No_List then
         List_Node := AIN.First_Node (Properties);

         while Present (List_Node) loop
            pragma Assert
              (AIN.Kind (List_Node) = K_Property_Association_Instance);
            Resolve_Values
              (Root      => Root,
               Container => Container,
               Property  => List_Node);
            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;
   end Resolve_Property_Associations;

   ----------------------------------------------
   -- Resolve_Properties_Of_Component_Instance --
   ----------------------------------------------

   procedure Resolve_Properties_Of_Component_Instance
     (Root      : Node_Id;
      Component : Node_Id)
   is
      pragma Assert (Kind (Component) = K_Component_Instance);

      List_Node      : Node_Id;
      Call_List_Node : Node_Id;
   begin
      --  Features

      if AIN.Features (Component) /= No_List then
         List_Node := AIN.First_Node (AIN.Features (Component));

         while Present (List_Node) loop
            Resolve_Property_Associations
              (Root       => Root,
               Properties => AIN.Properties (List_Node),
               Container  => List_Node);

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      --  Subcomponents

      if AIN.Subcomponents (Component) /= No_List then
         List_Node := AIN.First_Node (AIN.Subcomponents (Component));

         while Present (List_Node) loop
            if Present (Corresponding_Instance (List_Node)) then
               Resolve_Properties_Of_Component_Instance
                 (Root      => Root,
                  Component => Corresponding_Instance (List_Node));
            end if;
            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      --  Call sequences
      --  Some call sequences are anonymous

      if AIN.Calls (Component) /= No_List then
         List_Node := AIN.First_Node (AIN.Calls (Component));

         while Present (List_Node) loop
            if AIN.Subprogram_Calls (List_Node) /= No_List then
               Call_List_Node :=
                 AIN.First_Node (AIN.Subprogram_Calls (List_Node));

               while Present (Call_List_Node) loop
                  Resolve_Properties_Of_Component_Instance
                    (Root      => Root,
                     Component => Corresponding_Instance (Call_List_Node));
                  Call_List_Node := AIN.Next_Node (Call_List_Node);
               end loop;
            end if;

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      --  Connections
      --  Some connections are anonymous

      if AIN.Connections (Component) /= No_List then
         List_Node := AIN.First_Node (AIN.Connections (Component));

         while Present (List_Node) loop
            Resolve_Property_Associations
              (Root       => Root,
               Properties => AIN.Properties (List_Node),
               Container  => List_Node);
            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      --  Flows

      if AIN.Flows (Component) /= No_List then
         List_Node := AIN.First_Node (AIN.Flows (Component));

         while Present (List_Node) loop
            Resolve_Property_Associations
              (Root       => Root,
               Properties => AIN.Properties (List_Node),
               Container  => List_Node);
            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      --  Modes

      if AIN.Modes (Component) /= No_List then
         List_Node := AIN.First_Node (AIN.Modes (Component));

         while Present (List_Node) loop
            if AIN.Kind (List_Node) = K_Mode_Instance then
               Resolve_Property_Associations
                 (Root       => Root,
                  Properties => AIN.Properties (List_Node),
                  Container  => List_Node);
            end if;
            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      --  Properties

      Resolve_Property_Associations
        (Root       => Root,
         Properties => AIN.Properties (Component),
         Container  => Component);
   end Resolve_Properties_Of_Component_Instance;

   ----------------------------------------------
   -- Resolve_Properties_Of_Namespace_Instance --
   ----------------------------------------------

   procedure Resolve_Properties_Of_Namespace_Instance
     (Root          : Node_Id;
      The_Namespace : Node_Id)
   is
      pragma Assert (Kind (The_Namespace) = K_Namespace_Instance);

      List_Node     : Node_Id;
      The_Component : Node_Id;
   begin

      --  Declarations

      if AIN.Declarations (The_Namespace) /= No_List then
         List_Node := AIN.First_Node (AIN.Declarations (The_Namespace));

         while Present (List_Node) loop
            The_Component := AIN.Item (List_Node);

            pragma Assert (Kind (The_Component) = K_Component_Instance);

            Resolve_Properties_Of_Component_Instance
              (Root      => Root,
               Component => The_Component);

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;
   end Resolve_Properties_Of_Namespace_Instance;

end Ocarina.Instances.Processor.Properties;
