------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . P R O C E S S O R . P R O P E R T I E S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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
with Ocarina.AADL_Values;

with Ocarina.Analyzer.Messages;
with Ocarina.Analyzer.AADL.Queries;

with Ocarina.Builder.AADL.Properties;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;

package body Ocarina.Processor.Properties is

   use Ocarina.ME_AADL.AADL_Tree.Nodes;

   function Evaluate_Property_Value
     (Property_Value, Reference_Property : Node_Id) return Node_Id;
   --  Compute the value by fetching the property terms and applying
   --  the operations described in the property value. Return the
   --  first evaluated value (the following ones are chainded using
   --  Next_Node), or No_Node is nothing could be
   --  evaluated. Reference_Property is the property for the value of
   --  which is evaluated; it is used to display errors, in order to
   --  indicate what initiated the problem.

   function Expand_Property_Value
     (Property, Reference_Property : Node_Id) return Node_Id;
   --  Expand the property terms of the property, and return all the
   --  property values

   function Expand_Property_Type (Property_Type : Node_Id) return Node_Id;
   --  Computes the actual property type, by resolving references

   function Resolve_Type (Root, Property : Node_Id) return Boolean;
   --  Set a direct type (i.e. not a reference to a type declaration)
   --  for the property name declaration

   function Resolve_Value (Root, Property : Node_Id) return Boolean;
   --  Set a direct value (i.e. without value ()) for each value of
   --  the property association.

   function Resolve_Properties
     (Properties      : List_Id;
      Root, Container : Node_Id) return Boolean;

   function Resolve_Properties_Of_Component_Type
     (Root, Component : Node_Id) return Boolean;

   function Resolve_Properties_Of_Component_Implementation
     (Root, Component : Node_Id) return Boolean;

   function Resolve_Properties_Of_Port_Group_Type
     (Root, Port_Group : Node_Id) return Boolean;

   function Resolve_All_Properties (Root : Node_Id) return Boolean;

   function Resolve_All_Property_Names (Root : Node_Id) return Boolean;

   procedure Fetch (U : Node_Id; Fetched : out Node_Id; Base : out Boolean);
   --  Return the defining identifier corresponding to the
   --  multiplier U in the corresponding units type. Base is set to
   --  True if the fetched identifier is the base unit
   --  identifier. If the identifier is not found, return No_Node
   --  and False.

   -----------------------------
   -- Compute_Property_Values --
   -----------------------------

   function Compute_Property_Values (Root : Node_Id) return Boolean is
      Success : Boolean;

   begin
      Success := Resolve_All_Properties (Root);
      Success := Resolve_All_Property_Names (Root) and then Success;
      return Success;
   end Compute_Property_Values;

   --------------------------------------------
   -- Diffuse_Package_Properties_To_Entities --
   --------------------------------------------

   procedure Diffuse_Package_Properties_To_Entities (Root : Node_Id) is
      use Ocarina.Analyzer.AADL.Queries;
      use Ocarina.Builder.AADL.Properties;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      pragma Assert (Present (Root));
      pragma Assert (Kind (Root) = K_AADL_Specification);

      List_Node, Property_Node, Declaration_Node : Node_Id;
      Success                                    : Boolean := True;
      Diffused_Property                          : Node_Id;

   begin
      if Declarations (Root) /= No_List then
         List_Node := First_Node (Declarations (Root));

         while List_Node /= No_Node loop
            if Kind (List_Node) = K_Package_Specification
              and then
                Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node) /=
                No_List
              and then Declarations (List_Node) /= No_List
            then
               Property_Node :=
                 First_Node
                   (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node));

               while Property_Node /= No_Node loop
                  Declaration_Node := First_Node (Declarations (List_Node));

                  while Declaration_Node /= No_Node loop
                     if Kind (Declaration_Node) /=
                       K_Name_Visibility_Declaration
                       and then
                         Is_Private (Property_Node) =
                         Is_Private (Declaration_Node)
                       and then Property_Can_Apply_To_Entity
                         (Property_Node,
                          Declaration_Node)
                     then
                        Diffused_Property :=
                          Add_New_Property_Association
                            (Loc  => Loc (Property_Node),
                             Name =>
                               Duplicate_Identifier
                                 (Identifier (Property_Node)),
                             Property_Name  => Property_Name (Property_Node),
                             Container      => Declaration_Node,
                             In_Binding     => In_Binding (Property_Node),
                             In_Modes       => In_Modes (Property_Node),
                             Property_Value =>
                               Property_Association_Value (Property_Node),
                             Is_Constant => Is_Constant (Property_Node),
                             Is_Access   => Is_Access (Property_Node),
                             Is_Additive =>
                               Is_Additive_Association (Property_Node),
                             Applies_To => Applies_To_Prop (Property_Node),
                             Check_For_Conflicts => True,
                             Override            => False);
                        --  We add a new property, by duplicating the
                        --  one of the package

                        Success :=
                          Success and then Diffused_Property /= No_Node;
                     end if;

                     Declaration_Node := Next_Node (Declaration_Node);
                  end loop;

                  Remove_Node_From_List
                    (Property_Node,
                     Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node));
                  --  Simply detach the legacy property from the list. Its
                  --  elements are reused in the new properties

                  Property_Node := Next_Node (Property_Node);
               end loop;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;
   end Diffuse_Package_Properties_To_Entities;

   ------------------
   -- Resolve_Type --
   ------------------

   function Resolve_Type (Root, Property : Node_Id) return Boolean is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Entities;
      use Ocarina.Analyzer.AADL.Queries;

      pragma Assert (Present (Root));
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Present (Property));
      pragma Assert (Kind (Property) = K_Property_Definition_Declaration);

      Prop_Type : constant Node_Id := Property_Name_Type (Property);
      Expanded_Value, Prop_Value : Node_Id;
      Success                    : Boolean          := True;

   begin
      Set_Expanded_Type_Designator
        (Prop_Type,
         Expand_Property_Type (Property_Type_Designator (Prop_Type)));

      if Expanded_Type_Designator (Prop_Type) /= No_Node
        and then Kind (Expanded_Type_Designator (Prop_Type)) = K_Invalid_Node
      then
         --  FIXME : need for an error message
         Success := False;
      end if;

      if Default_Value (Property) /= No_Node then
         Prop_Value     := Default_Value (Property);
         Expanded_Value := Expand_Property_Value (Property, Property);

         if Expanded_Value = No_Node then
            Set_Expanded_Single_Value (Prop_Value, No_Node);
            Set_Expanded_Multi_Value (Prop_Value, No_List);

         elsif Kind (Expanded_Value) = K_Invalid_Node then
            --  FIXME : need for an error message
            Success := False;
            Set_Expanded_Single_Value (Prop_Value, No_Node);
            Set_Expanded_Multi_Value (Prop_Value, No_List);

         elsif Kind (Expanded_Value) = K_List_Id then
            Set_Expanded_Single_Value (Prop_Value, No_Node);
            Set_Expanded_Multi_Value (Prop_Value, List_Id (Expanded_Value));

         elsif Next_Node (Expanded_Value) = No_Node
           and then Single_Value (Prop_Value) /= No_Node
         then
            Set_Expanded_Single_Value (Prop_Value, Expanded_Value);
            Set_Expanded_Multi_Value (Prop_Value, No_List);

         else
            Set_Expanded_Single_Value (Prop_Value, No_Node);
            Set_Expanded_Multi_Value
              (Prop_Value,
               New_List (K_List_Id, Loc (Expanded_Value)));
            Append_Node_To_List
              (Expanded_Value,
               Expanded_Multi_Value (Prop_Value));
         end if;
      end if;

      return Success;
   end Resolve_Type;

   --------------------------
   -- Expand_Property_Type --
   --------------------------

   function Expand_Property_Type (Property_Type : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      Expanded_Type : Node_Id;

   begin
      case Kind (Property_Type) is
         when K_Unique_Property_Type_Identifier =>
            Expanded_Type := Get_Referenced_Entity (Property_Type);
            --  We first get the property type of the corresponding
            --  declaration

            Expanded_Type :=
              Expand_Property_Type (Property_Type_Designator (Expanded_Type));
         --  Then we expand it also

         when K_Number_Range =>
            Expanded_Type := New_Node (K_Number_Range, Loc (Property_Type));
            Set_Lower_Bound
              (Expanded_Type,
               Evaluate_Property_Value
                 (Lower_Bound (Property_Type),
                  Property_Type));
            Set_Upper_Bound
              (Expanded_Type,
               Evaluate_Property_Value
                 (Upper_Bound (Property_Type),
                  Property_Type));

            if
              (Upper_Bound (Expanded_Type) /= No_Node
               and then Kind (Upper_Bound (Expanded_Type)) = K_Invalid_Node)
              or else
              (Lower_Bound (Expanded_Type) /= No_Node
               and then Kind (Lower_Bound (Expanded_Type)) = K_Invalid_Node)
            then
               Set_Kind (Expanded_Type, K_Invalid_Node);
            end if;

         when K_Real_Type | K_Integer_Type =>
            Expanded_Type :=
              New_Node (Kind (Property_Type), Loc (Property_Type));
            Set_Unit_Designator
              (Expanded_Type,
               Unit_Designator (Property_Type));
            if Type_Range (Property_Type) /= No_Node then
               Set_Type_Range
                 (Expanded_Type,
                  New_Node (K_Number_Range, Loc (Type_Range (Property_Type))));
               Set_Lower_Bound
                 (Type_Range (Expanded_Type),
                  Evaluate_Property_Value
                    (Lower_Bound (Type_Range (Property_Type)),
                     Property_Type));
               Set_Upper_Bound
                 (Type_Range (Expanded_Type),
                  Evaluate_Property_Value
                    (Upper_Bound (Type_Range (Property_Type)),
                     Property_Type));

               if
                 (Upper_Bound (Type_Range (Expanded_Type)) /= No_Node
                  and then
                    Kind (Upper_Bound (Type_Range (Expanded_Type))) =
                    K_Invalid_Node)
                 or else
                 (Lower_Bound (Type_Range (Expanded_Type)) /= No_Node
                  and then
                    Kind (Lower_Bound (Type_Range (Expanded_Type))) =
                    K_Invalid_Node)
               then
                  Set_Kind (Expanded_Type, K_Invalid_Node);
               end if;
            end if;

         when K_Range_Type =>
            Expanded_Type := New_Node (K_Range_Type, Loc (Property_Type));
            Set_Number_Type
              (Expanded_Type,
               Expand_Property_Type (Number_Type (Property_Type)));

            if Number_Type (Expanded_Type) /= No_Node
              and then Kind (Number_Type (Expanded_Type)) = K_Invalid_Node
            then
               Set_Kind (Expanded_Type, K_Invalid_Node);
            end if;

         when others =>
            Expanded_Type := Property_Type;
      end case;

      return Expanded_Type;
   end Expand_Property_Type;

   -------------------
   -- Resolve_Value --
   -------------------

   function Resolve_Value (Root, Property : Node_Id) return Boolean is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.Analyzer.AADL.Queries;

      pragma Assert (Present (Root));
      pragma Assert (Kind (Root) = K_AADL_Specification);
      pragma Assert (Present (Property));
      pragma Assert (Kind (Property) = K_Property_Association);

      Prop_Value : constant Node_Id := Property_Association_Value (Property);
      Expanded_Value : Node_Id;
      Success        : Boolean          := True;

   begin
      Expanded_Value := Expand_Property_Value (Property, Property);

      if Expanded_Value = No_Node then
         Set_Expanded_Single_Value (Prop_Value, No_Node);
         Set_Expanded_Multi_Value (Prop_Value, No_List);

      elsif Kind (Expanded_Value) = K_Invalid_Node then
         --  FIXME : need for an error message
         Success := False;
         Set_Expanded_Single_Value (Prop_Value, No_Node);
         Set_Expanded_Multi_Value (Prop_Value, No_List);

      elsif Kind (Expanded_Value) = K_List_Id then
         Set_Expanded_Single_Value (Prop_Value, No_Node);
         Set_Expanded_Multi_Value (Prop_Value, List_Id (Expanded_Value));

      elsif Next_Node (Expanded_Value) = No_Node
        and then Single_Value (Prop_Value) /= No_Node
      then
         Set_Expanded_Single_Value (Prop_Value, Expanded_Value);
         Set_Expanded_Multi_Value (Prop_Value, No_List);

      else
         Set_Expanded_Single_Value (Prop_Value, No_Node);
         Set_Expanded_Multi_Value
           (Prop_Value,
            New_List (K_List_Id, Loc (Expanded_Value)));
         Append_Node_To_List
           (Expanded_Value,
            Expanded_Multi_Value (Prop_Value));
      end if;

      return Success;
   end Resolve_Value;

   ---------------------------
   -- Expand_Property_Value --
   ---------------------------

   function Expand_Property_Value
     (Property, Reference_Property : Node_Id) return Node_Id
   is
      --  Take a property declaration and return its expanded value

      use Ocarina.Analyzer.Messages;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Reference_Property /= No_Node);
      pragma Assert
        (Property = No_Node
         or else Kind (Property) = K_Property_Association
         or else Kind (Property) = K_Constant_Property_Declaration
         or else Kind (Property) = K_Property_Definition_Declaration
         or else Kind (Property) = K_Property_Type_Declaration
         or else Kind (Property) = K_Record_Term_Element
         or else DNKE (Property));

      Value, List_Node, Expanded_List_Node, Computed_Value : Node_Id;
      Expanded_List                                        : List_Id;
      Undefined_Values                                     : Boolean;
   begin
      if Property = No_Node
        or else Kind (Property) = K_Record_Term_Element
      then
         return No_Node;
      end if;

      --  First get the value of the property
      case Kind (Property) is
         when K_Property_Association =>
            Value := Property_Association_Value (Property);
         when K_Constant_Property_Declaration =>
            Value := Constant_Value (Property);
         when K_Property_Definition_Declaration =>
            Value := Default_Value (Property);
         when K_Property_Type_Declaration =>
            Value := No_Node;

         when others =>
            raise Program_Error;
      end case;

      if Value = No_Node then
         return Value;

      elsif Single_Value (Value) /= No_Node then
         Computed_Value :=
           Evaluate_Property_Value (Single_Value (Value), Reference_Property);

         if Computed_Value /= No_Node then
            if Kind (Computed_Value) = K_List_Id then
               Expanded_List_Node := First_Node (List_Id (Computed_Value));
            else
               Expanded_List_Node := Computed_Value;
            end if;

            while Expanded_List_Node /= No_Node loop
               Set_Loc (Expanded_List_Node, Loc (Single_Value (Value)));
               Expanded_List_Node := Next_Node (Expanded_List_Node);
            end loop;
            --  The location of the evaluated values should be the
            --  same as the primitive value
         end if;

         return Computed_Value;
      else
         List_Node     := First_Node (Multi_Value (Value));
         Expanded_List :=
           New_List (K_List_Id, Loc (Node_Id (Multi_Value (Value))));
         Undefined_Values := (List_Node /= No_Node);
         --  If the list is empty, then the value is defined. Else,
         --  the value may be undefined if all the elements of the
         --  list expand to an undefined value (i.e. no node)

         while List_Node /= No_Node loop
            Computed_Value := Evaluate_Property_Value (List_Node, Property);

            if Computed_Value /= No_Node then
               Undefined_Values := False;

               if Kind (Computed_Value) = K_Invalid_Node then
                  return Computed_Value;
               elsif Kind (Computed_Value) = K_List_Id then
                  Expanded_List_Node := First_Node (List_Id (Computed_Value));

                  while Expanded_List_Node /= No_Node loop
                     Set_Loc (Expanded_List_Node, Loc (List_Node));
                     Expanded_List_Node := Next_Node (Expanded_List_Node);
                  end loop;

                  Append_List_To_List
                    (List_Id (Computed_Value),
                     Expanded_List);
               else
                  Expanded_List_Node := Computed_Value;

                  while Expanded_List_Node /= No_Node loop
                     Set_Loc (Expanded_List_Node, Loc (List_Node));
                     Expanded_List_Node := Next_Node (Expanded_List_Node);
                  end loop;

                  Append_Node_To_List (Computed_Value, Expanded_List);
               end if;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;

         if Undefined_Values then
            return No_Node;
         else
            return Node_Id (Expanded_List);
         end if;
      end if;
   end Expand_Property_Value;

   -----------------------------
   -- Evaluate_Property_Value --
   -----------------------------

   function Evaluate_Property_Value
     (Property_Value, Reference_Property : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Entities;
      use Ocarina.Analyzer.Messages;
      use Ocarina.AADL_Values;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Property_Value = No_Node
         or else Kind (Property_Value) = K_Literal
         or else Kind (Property_Value) = K_Property_Term
         or else Kind (Property_Value) = K_Number_Range_Term
         or else Kind (Property_Value) = K_Reference_Term
         or else Kind (Property_Value) = K_Enumeration_Term
         or else Kind (Property_Value) = K_Minus_Numeric_Term
         or else Kind (Property_Value) = K_Signed_AADLNumber
         or else Kind (Property_Value) = K_Not_Boolean_Term
         or else Kind (Property_Value) = K_And_Boolean_Term
         or else Kind (Property_Value) = K_Or_Boolean_Term
         or else Kind (Property_Value) = K_Parenthesis_Boolean_Term
         or else Kind (Property_Value) = K_Component_Classifier_Term
         or else Kind (Property_Value) = K_Unique_Property_Const_Identifier
         or else Kind (Property_Value) = K_Record_Term
         or else Ocarina.ME_AADL.AADL_Tree.Entities.DNKE (Property_Value));

      pragma Assert (Reference_Property /= No_Node);

      Evaluated_Value : Node_Id := No_Node;
      Ref_Term        : Node_Id := No_Node;
   begin
      if Property_Value = No_Node then
         Evaluated_Value := No_Node;
      else
         case Kind (Property_Value) is
            when K_Enumeration_Term =>
               Evaluated_Value :=
                 New_Node (Kind (Property_Value), Loc (Property_Value));
               Set_Identifier (Evaluated_Value, Identifier (Property_Value));

               Set_Property_Set_Identifier
                 (Evaluated_Value,
                  Property_Set_Identifier (Property_Value));

               Set_Entity (Evaluated_Value, Entity (Property_Value));

            when K_Unique_Property_Const_Identifier =>
               Evaluated_Value :=
                 Expand_Property_Value
                   (Get_Referenced_Entity (Property_Value),
                    Reference_Property);

            when K_Literal =>
               Evaluated_Value :=
                 New_Node (Kind (Property_Value), Loc (Property_Value));
               Set_Value
                 (Evaluated_Value,
                  New_Value (Value (Value (Property_Value))));
            --  We clone the literal value

            when K_Number_Range_Term =>
               declare
                  Evaluated_Lower_Bound,
                  Evaluated_Upper_Bound,
                  Evaluated_Delta_Term : Node_Id;
               begin
                  Evaluated_Value :=
                    New_Node (Kind (Property_Value), Loc (Property_Value));
                  Evaluated_Lower_Bound :=
                    Evaluate_Property_Value
                      (Lower_Bound (Property_Value),
                       Reference_Property);
                  Evaluated_Upper_Bound :=
                    Evaluate_Property_Value
                      (Upper_Bound (Property_Value),
                       Reference_Property);
                  Evaluated_Delta_Term :=
                    Evaluate_Property_Value
                      (Delta_Term (Property_Value),
                       Reference_Property);

                  --  Check the consistency of the evaluated values

                  if Evaluated_Lower_Bound /= No_Node
                    and then
                    ((Kind (Evaluated_Lower_Bound) /= K_Literal
                      and then
                        Kind (Evaluated_Lower_Bound) /=
                        K_Signed_AADLNumber)
                     or else Next_Node (Evaluated_Lower_Bound) /= No_Node)
                  then
                     Display_Inconsistency_In_Property_Values
                       (Property_Value,
                        Evaluated_Lower_Bound,
                        Reference_Property);
                     Set_Kind (Evaluated_Lower_Bound, K_Invalid_Node);
                  end if;

                  if Evaluated_Upper_Bound /= No_Node
                    and then
                    ((Kind (Evaluated_Upper_Bound) /= K_Literal
                      and then
                        Kind (Evaluated_Upper_Bound) /=
                        K_Signed_AADLNumber)
                     or else Next_Node (Evaluated_Upper_Bound) /= No_Node)
                  then
                     Display_Inconsistency_In_Property_Values
                       (Property_Value,
                        Evaluated_Upper_Bound,
                        Reference_Property);
                     Set_Kind (Evaluated_Upper_Bound, K_Invalid_Node);
                  end if;

                  if Evaluated_Delta_Term /= No_Node
                    and then
                    ((Kind (Evaluated_Delta_Term) /= K_Literal
                      and then
                        Kind (Evaluated_Delta_Term) /=
                        K_Signed_AADLNumber)
                     or else Next_Node (Evaluated_Delta_Term) /= No_Node)
                  then
                     Display_Inconsistency_In_Property_Values
                       (Property_Value,
                        Evaluated_Delta_Term,
                        Reference_Property);
                     Set_Kind (Evaluated_Delta_Term, K_Invalid_Node);
                  end if;

                  --  Set the evaluated values

                  if Evaluated_Lower_Bound /= No_Node
                    and then Evaluated_Upper_Bound /= No_Node
                    and then
                    (Evaluated_Delta_Term /= No_Node
                     or else Delta_Term (Property_Value) = No_Node)
                  then
                     if Kind (Evaluated_Lower_Bound) /= K_Invalid_Node
                       and then Kind (Evaluated_Upper_Bound) /= K_Invalid_Node
                       and then
                       (Evaluated_Delta_Term = No_Node
                        or else Kind (Evaluated_Delta_Term) /= K_Invalid_Node)
                     then
                        --  If we could evaluate the lower and upper
                        --  bounds, and the delta term (unless there
                        --  was no original value), we store the
                        --  evaluated values

                        Set_Lower_Bound
                          (Evaluated_Value,
                           Evaluated_Lower_Bound);
                        Set_Upper_Bound
                          (Evaluated_Value,
                           Evaluated_Upper_Bound);
                        Set_Delta_Term (Evaluated_Value, Evaluated_Delta_Term);
                     else
                        --  If at least an evaluated value is invalid

                        Set_Kind (Evaluated_Value, K_Invalid_Node);
                     end if;
                  else
                     Evaluated_Value := No_Node;
                  end if;
               end;

            when K_Reference_Term =>
               case Kind (Reference_Term (Property_Value)) is
                  when K_Entity_Reference =>
                     Evaluated_Value :=
                       New_Node (Kind (Property_Value), Loc (Property_Value));

                     Ref_Term :=
                       New_Node
                         (Kind (Reference_Term (Property_Value)),
                          Loc (Reference_Term (Property_Value)));
                     Set_Identifier
                       (Ref_Term,
                        Duplicate_Identifier
                          (Identifier (Reference_Term (Property_Value))));
                     Set_Path
                       (Ref_Term,
                        Path (Reference_Term (Property_Value)));
                     Set_Namespace_Path
                       (Ref_Term,
                        Namespace_Path (Reference_Term (Property_Value)));
                     Set_Namespace_Identifier
                       (Ref_Term,
                        Duplicate_Identifier
                          (Namespace_Identifier
                             (Reference_Term (Property_Value))));
                     Set_Referenced_Entity
                       (Ref_Term,
                        Entity (Reference_Term (Property_Value)));

                     Set_Reference_Term (Evaluated_Value, Ref_Term);

                  when K_Contained_Element_Path =>
                     Evaluated_Value :=
                       New_Node (Kind (Property_Value), Loc (Property_Value));

                     Ref_Term :=
                       New_Node
                         (Kind (Reference_Term (Property_Value)),
                          Loc (Reference_Term (Property_Value)));
                     Set_List_Items
                       (Ref_Term,
                        List_Items (Reference_Term (Property_Value)));
                     Set_Referenced_Entity
                       (Ref_Term,
                        Entity (Reference_Term (Property_Value)));

                     Set_Annex_Path
                       (Ref_Term,
                        Annex_Path (Reference_Term (Property_Value)));

                     Set_Reference_Term (Evaluated_Value, Ref_Term);

                  when others =>
                     raise Program_Error;
               end case;

            when K_Property_Term =>
               Evaluated_Value :=
                 Expand_Property_Value
                   (Get_Referenced_Entity (Property_Value),
                    Reference_Property);

            when K_Minus_Numeric_Term =>
               declare
                  Val     : Value_Type;
                  Literal : Node_Id;
               begin
                  Evaluated_Value :=
                    Evaluate_Property_Value
                      (Numeric_Term (Property_Value),
                       Reference_Property);

                  if Evaluated_Value /= No_Node then
                     if Kind (Evaluated_Value) = K_Literal
                       and then Next_Node (Evaluated_Value) = No_Node
                     then
                        Literal := Evaluated_Value;

                     elsif Kind (Evaluated_Value) = K_Signed_AADLNumber
                       and then Next_Node (Evaluated_Value) = No_Node
                     then
                        Literal := Number_Value (Evaluated_Value);
                     --  Since the number has been evaluated, the
                     --  number_value can only be a literal

                     else
                        Display_Inconsistency_In_Property_Values
                          (Property_Value,
                           Evaluated_Value,
                           Reference_Property);
                        Literal         := No_Node;
                        Evaluated_Value :=
                          New_Node (K_Invalid_Node, Loc (Property_Value));
                     end if;

                     if Literal /= No_Node then
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
                           Display_Inconsistency_In_Property_Values
                             (Property_Value,
                              Literal,
                              Reference_Property);
                           Evaluated_Value :=
                             New_Node (K_Invalid_Node, Loc (Property_Value));
                        end if;

                        if Evaluated_Value /= No_Node then
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
                    New_Node (Kind (Property_Value), Loc (Property_Value));
                  Set_Unit_Identifier
                    (Evaluated_Value,
                     Unit_Identifier (Property_Value));
                  Evaluated_Number_Value :=
                    Evaluate_Property_Value
                      (Number_Value (Property_Value),
                       Reference_Property);

                  if Evaluated_Number_Value /= No_Node then
                     if Kind (Evaluated_Number_Value) /= K_Literal
                       or else Next_Node (Evaluated_Number_Value) /= No_Node
                     then
                        Display_Inconsistency_In_Property_Values
                          (Property_Value,
                           Evaluated_Number_Value,
                           Reference_Property);
                        Set_Kind (Evaluated_Value, K_Invalid_Node);
                     else
                        Set_Number_Value
                          (Evaluated_Value,
                           Evaluated_Number_Value);
                     end if;
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
                      (Boolean_Term (Property_Value),
                       Reference_Property);

                  if Evaluated_Value /= No_Node then
                     if Kind (Evaluated_Value) = K_Literal
                       and then
                         Get_Value_Type (Value (Evaluated_Value)).T =
                         LT_Boolean
                       and then Next_Node (Evaluated_Value) = No_Node
                     then
                        Val      := Get_Value_Type (Value (Evaluated_Value));
                        Val.BVal := not Val.BVal;
                        Set_Value (Value (Evaluated_Value), Val);
                     else
                        Display_Inconsistency_In_Property_Values
                          (Property_Value,
                           Evaluated_Value,
                           Reference_Property);
                        Set_Kind (Evaluated_Value, K_Invalid_Node);
                     end if;
                  else
                     Evaluated_Value := No_Node;
                  end if;
               end;

            when K_And_Boolean_Term =>
               declare
                  Auxiliary_Value  : Node_Id;
                  Val              : Value_Type;
                  Val1_OK, Val2_OK : Boolean;
               begin
                  Evaluated_Value :=
                    Evaluate_Property_Value
                      (First_Term (Property_Value),
                       Reference_Property);
                  Auxiliary_Value :=
                    Evaluate_Property_Value
                      (Second_Term (Property_Value),
                       Reference_Property);

                  if Evaluated_Value /= No_Node then
                     if Kind (Evaluated_Value) = K_Literal
                       and then
                         Get_Value_Type (Value (Evaluated_Value)).T =
                         LT_Boolean
                       and then Next_Node (Evaluated_Value) = No_Node
                     then
                        Val1_OK := True;
                     else
                        Display_Inconsistency_In_Property_Values
                          (Property_Value,
                           Evaluated_Value,
                           Reference_Property);
                        Set_Kind (Evaluated_Value, K_Invalid_Node);
                        Val1_OK := False;
                     end if;
                  else
                     Val1_OK         := False;
                     Evaluated_Value := No_Node;
                  end if;

                  if Auxiliary_Value /= No_Node then
                     if Kind (Auxiliary_Value) = K_Literal
                       and then
                         Get_Value_Type (Value (Auxiliary_Value)).T =
                         LT_Boolean
                       and then Next_Node (Auxiliary_Value) = No_Node
                     then
                        Val2_OK := True;
                     else
                        Display_Inconsistency_In_Property_Values
                          (Property_Value,
                           Auxiliary_Value,
                           Reference_Property);
                        Set_Kind (Auxiliary_Value, K_Invalid_Node);
                        Evaluated_Value := Auxiliary_Value;
                        Val2_OK         := False;
                     end if;
                  else
                     Val2_OK         := False;
                     Evaluated_Value := No_Node;
                  end if;

                  if Val1_OK and then Val2_OK then
                     Val      := Get_Value_Type (Value (Evaluated_Value));
                     Val.BVal :=
                       Val.BVal
                       and then Get_Value_Type (Value (Auxiliary_Value)).BVal;
                     Set_Value (Value (Evaluated_Value), Val);
                  end if;
               end;

            when K_Or_Boolean_Term =>
               declare
                  Auxiliary_Value  : Node_Id;
                  Val              : Value_Type;
                  Val1_OK, Val2_OK : Boolean;
               begin
                  Evaluated_Value :=
                    Evaluate_Property_Value
                      (First_Term (Property_Value),
                       Reference_Property);
                  Auxiliary_Value :=
                    Evaluate_Property_Value
                      (Second_Term (Property_Value),
                       Reference_Property);

                  if Evaluated_Value /= No_Node then
                     if Kind (Evaluated_Value) = K_Literal
                       and then
                         Get_Value_Type (Value (Evaluated_Value)).T =
                         LT_Boolean
                       and then Next_Node (Evaluated_Value) = No_Node
                     then
                        Val1_OK := True;
                     else
                        Display_Inconsistency_In_Property_Values
                          (Property_Value,
                           Evaluated_Value,
                           Reference_Property);
                        Set_Kind (Evaluated_Value, K_Invalid_Node);
                        Val1_OK := False;
                     end if;
                  else
                     Val1_OK         := False;
                     Evaluated_Value := No_Node;
                  end if;

                  if Auxiliary_Value /= No_Node then
                     if Kind (Auxiliary_Value) = K_Literal
                       and then
                         Get_Value_Type (Value (Auxiliary_Value)).T =
                         LT_Boolean
                       and then Next_Node (Auxiliary_Value) = No_Node
                     then
                        Val2_OK := True;
                     else
                        Display_Inconsistency_In_Property_Values
                          (Property_Value,
                           Auxiliary_Value,
                           Property_Value);
                        Set_Kind (Auxiliary_Value, K_Invalid_Node);
                        Evaluated_Value := Auxiliary_Value;
                        Val2_OK         := False;
                     end if;
                  else
                     Val2_OK         := False;
                     Evaluated_Value := No_Node;
                  end if;

                  if Val1_OK and then Val2_OK then
                     Val      := Get_Value_Type (Value (Evaluated_Value));
                     Val.BVal :=
                       Val.BVal
                       or else Get_Value_Type (Value (Auxiliary_Value)).BVal;
                     Set_Value (Value (Evaluated_Value), Val);
                  end if;
               end;

            when K_Parenthesis_Boolean_Term =>
               Evaluated_Value :=
                 Evaluate_Property_Value
                   (Boolean_Term (Property_Value),
                    Reference_Property);

               if Evaluated_Value /= No_Node then
                  if Kind (Evaluated_Value) = K_Literal
                    and then
                      Get_Value_Type (Value (Evaluated_Value)).T =
                      LT_Boolean
                    and then Next_Node (Evaluated_Value) = No_Node
                  then
                     null;
                  else
                     Display_Inconsistency_In_Property_Values
                       (Property_Value,
                        Evaluated_Value,
                        Reference_Property);
                     Set_Kind (Evaluated_Value, K_Invalid_Node);
                  end if;
               else
                  Evaluated_Value := No_Node;
               end if;

            when K_Component_Classifier_Term =>
               Evaluated_Value :=
                 New_Node (Kind (Property_Value), Loc (Property_Value));
               Set_Referenced_Entity
                 (Evaluated_Value,
                  Get_Referenced_Entity (Property_Value));
               Set_Component_Cat
                 (Evaluated_Value,
                  Component_Cat (Property_Value));

            when K_Record_Term =>
               Evaluated_Value :=
                 New_Node (Kind (Property_Value), Loc (Property_Value));

               declare
                  Record_Terms_List : constant List_Id :=
                    New_List (K_List_Id, Loc (Property_Value));

                  J : Node_Id := First_Node (List_Items (Property_Value));
                  List_Node : Node_Id;
                  Record_Term_Element_Node : Node_Id;
               begin
                  while Present (J) loop
                     Record_Term_Element_Node := New_Node
                       (K_Record_Term_Element, Loc (Property_Value));
                     List_Node := Evaluate_Property_Value
                       (Property_Expression (J), Reference_Property);
                     --                     Put_Line (Kind (List_Node)'Img);

                     Set_Identifier (Record_Term_Element_Node, Identifier (J));
                     Set_Property_Expression
                       (Record_Term_Element_Node, List_Node);

                     Append_Node_To_List
                       (Record_Term_Element_Node, Record_Terms_List);
                     J := Next_Node (J);
                  end loop;
                  Set_List_Items (Evaluated_Value, Record_Terms_List);
               end;

            when others =>
               raise Program_Error;
         end case;
      end if;

      return Evaluated_Value;
   end Evaluate_Property_Value;

   ------------------------
   -- Resolve_Properties --
   ------------------------

   function Resolve_Properties
     (Properties      : List_Id;
      Root, Container : Node_Id) return Boolean
   is
      pragma Assert (Container /= No_Node);

      Success   : Boolean := True;
      List_Node : Node_Id;

   begin
      if Properties = No_List then
         return True;
      end if;

      List_Node := First_Node (Properties);

      while List_Node /= No_Node loop
         pragma Assert (Kind (List_Node) = K_Property_Association);

         Success   := Resolve_Value (Root, List_Node) and then Success;
         List_Node := Next_Node (List_Node);
      end loop;

      return Success;
   end Resolve_Properties;

   ------------------------------------------
   -- Resolve_Properties_Of_Component_Type --
   ------------------------------------------

   function Resolve_Properties_Of_Component_Type
     (Root, Component : Node_Id) return Boolean
   is
      pragma Assert (Present (Component));
      pragma Assert (Kind (Component) = K_Component_Type);

      Success   : Boolean := True;
      List_Node : Node_Id;

   begin

      --  Features

      if Features (Component) /= No_List then
         List_Node := First_Node (Features (Component));

         while List_Node /= No_Node loop
            Success :=
              Resolve_Properties
                (Root       => Root,
                 Properties =>
                   Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 Container => List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Flows

      if Flows (Component) /= No_List then
         List_Node := First_Node (Flows (Component));

         while List_Node /= No_Node loop
            Success :=
              Resolve_Properties
                (Root       => Root,
                 Properties =>
                   Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 Container => List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Properties

      Success :=
        Resolve_Properties
          (Root       => Root,
           Properties =>
             Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Component),
           Container => Component)
        and then Success;

      return Success;
   end Resolve_Properties_Of_Component_Type;

   ----------------------------------------------------
   -- Resolve_Properties_Of_Component_Implementation --
   ----------------------------------------------------

   function Resolve_Properties_Of_Component_Implementation
     (Root, Component : Node_Id) return Boolean
   is
      pragma Assert (Present (Component));
      pragma Assert (Kind (Component) = K_Component_Implementation);

      Success                   : Boolean := True;
      List_Node, Call_List_Node : Node_Id;

   begin

      --  Type refinements

      if Refines_Type (Component) /= No_List then
         List_Node := First_Node (Refines_Type (Component));

         while List_Node /= No_Node loop
            Success :=
              Resolve_Properties
                (Root       => Root,
                 Properties =>
                   Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 Container => List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Subcomponents

      if Subcomponents (Component) /= No_List then
         List_Node := First_Node (Subcomponents (Component));

         while Present (List_Node) loop
            Success :=
              Resolve_Properties
                (Root       => Root,
                 Properties =>
                   Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 Container => List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Call sequences
      --  Some call sequences are anonymous

      if Calls (Component) /= No_List then
         List_Node := First_Node (Calls (Component));

         while List_Node /= No_Node loop
            if Subprogram_Calls (List_Node) /= No_List then
               Call_List_Node := First_Node (Subprogram_Calls (List_Node));

               while Call_List_Node /= No_Node loop
                  Success :=
                    Resolve_Properties
                      (Root       => Root,
                       Properties =>
                         Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
                           (Call_List_Node),
                       Container => Call_List_Node)
                    and then Success;
                  Call_List_Node := Next_Node (Call_List_Node);
               end loop;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Connections
      --  Some connections are anonymous

      if Connections (Component) /= No_List then
         List_Node := First_Node (Connections (Component));

         while List_Node /= No_Node loop
            Success :=
              Resolve_Properties
                (Root       => Root,
                 Properties =>
                   Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 Container => List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Flows

      if Flows (Component) /= No_List then
         List_Node := First_Node (Flows (Component));

         while List_Node /= No_Node loop
            Success :=
              Resolve_Properties
                (Root       => Root,
                 Properties =>
                   Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 Container => List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Modes

      if Modes (Component) /= No_List then
         List_Node := First_Node (Modes (Component));

         while Present (List_Node) loop
            if Kind (List_Node) = K_Mode then
               Success :=
                 Resolve_Properties
                   (Root       => Root,
                    Properties =>
                      Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                    Container => List_Node)
                 and then Success;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Properties

      Success :=
        Resolve_Properties
          (Root       => Root,
           Properties =>
             Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Component),
           Container => Component)
        and then Success;

      return Success;
   end Resolve_Properties_Of_Component_Implementation;

   -------------------------------------------
   -- Resolve_Properties_Of_Port_Group_Type --
   -------------------------------------------

   function Resolve_Properties_Of_Port_Group_Type
     (Root, Port_Group : Node_Id) return Boolean
   is
      pragma Assert (Present (Port_Group));
      pragma Assert (Kind (Port_Group) = K_Feature_Group_Type);

      Success   : Boolean := True;
      List_Node : Node_Id;

   begin
      --  Features

      if Features (Port_Group) /= No_List then
         List_Node := First_Node (Features (Port_Group));

         while List_Node /= No_Node loop
            Success :=
              Resolve_Properties
                (Root       => Root,
                 Properties =>
                   Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (List_Node),
                 Container => List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Properties

      Success :=
        Resolve_Properties
          (Root       => Root,
           Properties =>
             Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Port_Group),
           Container => Port_Group)
        and then Success;

      return Success;
   end Resolve_Properties_Of_Port_Group_Type;

   ----------------------------
   -- Resolve_All_Properties --
   ----------------------------

   function Resolve_All_Properties (Root : Node_Id) return Boolean is
      pragma Assert (Present (Root));
      pragma Assert (Kind (Root) = K_AADL_Specification);

      Success                      : Boolean := True;
      List_Node, Package_List_Node : Node_Id;

   begin
      if Declarations (Root) /= No_List then
         List_Node := First_Node (Declarations (Root));

         while List_Node /= No_Node loop
            case Kind (List_Node) is
               when K_Component_Implementation =>
                  Success :=
                    Resolve_Properties_Of_Component_Implementation
                      (Root      => Root,
                       Component => List_Node)
                    and then Success;

               when K_Component_Type =>
                  Success :=
                    Resolve_Properties_Of_Component_Type
                      (Root      => Root,
                       Component => List_Node)
                    and then Success;

               when K_Feature_Group_Type =>
                  Success :=
                    Resolve_Properties_Of_Port_Group_Type
                      (Root       => Root,
                       Port_Group => List_Node)
                    and then Success;

               when K_Package_Specification =>
                  Success :=
                    Resolve_Properties
                      (Root       => Root,
                       Properties =>
                         Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
                           (List_Node),
                       Container => List_Node)
                    and then Success;

                  if Declarations (List_Node) /= No_List then
                     Package_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Package_List_Node /= No_Node loop
                        case Kind (Package_List_Node) is
                           when K_Component_Implementation =>
                              Success :=
                                Resolve_Properties_Of_Component_Implementation
                                  (Root      => Root,
                                   Component => Package_List_Node)
                                and then Success;

                           when K_Component_Type =>
                              Success :=
                                Resolve_Properties_Of_Component_Type
                                  (Root      => Root,
                                   Component => Package_List_Node)
                                and then Success;

                           when K_Feature_Group_Type =>
                              Success :=
                                Resolve_Properties_Of_Port_Group_Type
                                  (Root       => Root,
                                   Port_Group => Package_List_Node)
                                and then Success;

                           when others =>
                              null;
                        end case;

                        Package_List_Node := Next_Node (Package_List_Node);
                     end loop;
                  end if;

               when others =>
                  null;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Resolve_All_Properties;

   --------------------------------
   -- Resolve_All_Property_Names --
   --------------------------------

   function Resolve_All_Property_Names (Root : Node_Id) return Boolean is
      pragma Assert (Present (Root));
      pragma Assert (Kind (Root) = K_AADL_Specification);

      Success                           : Boolean := True;
      List_Node, Property_Set_List_Node : Node_Id;

   begin
      if Declarations (Root) /= No_List then
         List_Node := First_Node (Declarations (Root));

         while List_Node /= No_Node loop
            case Kind (List_Node) is
               when K_Property_Set =>
                  if Declarations (List_Node) /= No_List then
                     Property_Set_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Property_Set_List_Node /= No_Node loop
                        case Kind (Property_Set_List_Node) is
                           when K_Property_Definition_Declaration =>
                              Success :=
                                Resolve_Type
                                  (Root     => Root,
                                   Property => Property_Set_List_Node)
                                and then Success;

                           when others =>
                              null;
                        end case;

                        Property_Set_List_Node :=
                          Next_Node (Property_Set_List_Node);
                     end loop;
                  end if;

               when others =>
                  null;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Resolve_All_Property_Names;

   ---------------------
   -- Convert_To_Base --
   ---------------------

   function Convert_To_Base (L : Node_Id; U : Node_Id) return Node_Id is
      use Ocarina.AADL_Values;
      use Utils;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.Analyzer.Messages;

      Fetched        : Node_Id;
      N              : Node_Id;
      Base           : Boolean;
      Result         : Value_Type;
      Count          : Natural;
      Max_Iterations : Natural;
      Units_Type     : Node_Id;
   begin
      Fetch (U, Fetched, Base);

      if not Base then
         --  To avoid infinite loops and detect bad formed units
         --  types.

         Units_Type :=
           Corresponding_Entity
             (Unit_Identifier (Corresponding_Entity (Fetched)));

         Max_Iterations := Length (Unit_Definitions (Units_Type));
      end if;

      Result := Value (Value (L));
      Count  := 0;

      while not Base loop
         Result :=
           Result *
           Value (Value (Numeric_Literal (Corresponding_Entity (Fetched))));

         Fetch
           (Unit_Identifier (Corresponding_Entity (Fetched)),
            Fetched,
            Base);

         Count := Count + 1;

         if Count > Max_Iterations + 1 then
            DAE
              (Message0 => "Units Type ",
               Node1    => Units_Type,
               Message1 => " is ill-defined: it contains cycles");
            exit;
         end if;
      end loop;
      N := New_Node (K_Literal, Loc (L));
      Set_Value (N, New_Value (Result));

      return N;
   end Convert_To_Base;

   -----------
   -- Fetch --
   -----------

   procedure Fetch (U : Node_Id; Fetched : out Node_Id; Base : out Boolean) is
      use Utils;
      Units_Type      : Node_Id;
      Unit_Definition : Node_Id;
   begin
      if Kind (Corresponding_Entity (U)) = K_Units_Type then
         --  We have the base identifier

         Units_Type := Corresponding_Entity (U);
      else
         Units_Type :=
           Corresponding_Entity (Unit_Identifier (Corresponding_Entity (U)));
      end if;

      --  This phase is neccessary because the Unit_Identifier of a
      --  Unit_definition is not linked directly to its corresponding
      --  unit definition.

      Fetched := Base_Identifier (Units_Type);

      if To_Lower (Name (Fetched)) = To_Lower (Name (U)) then
         Base := True;
      else
         Base            := False;
         Unit_Definition := First_Node (Unit_Definitions (Units_Type));

         while Present (Unit_Definition) loop
            Fetched := Identifier (Unit_Definition);

            exit when To_Lower (Name (Fetched)) = To_Lower (Name (U));

            Fetched         := No_Node;
            Unit_Definition := Next_Node (Unit_Definition);
         end loop;
      end if;
   end Fetch;

end Ocarina.Processor.Properties;
