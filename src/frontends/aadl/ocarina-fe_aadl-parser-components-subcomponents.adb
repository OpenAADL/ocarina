------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             OCARINA.FE_AADL.PARSER.COMPONENTS.SUBCOMPONENTS              --
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

with Locations;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

with Ocarina.FE_AADL.Lexer;
with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Parser.Components.Modes;
with Ocarina.FE_AADL.Parser.Components.Arrays;
with Ocarina.FE_AADL.Parser.Components.Prototypes;
with Ocarina.FE_AADL.Parser.Properties;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.Builder.AADL.Components.Subcomponents;

package body Ocarina.FE_AADL.Parser.Components.Subcomponents is

   --------------------
   -- P_Subcomponent --
   --------------------

   --  AADL_V1
   --  subcomponent ::=
   --     defining_subcomponent_identifier :
   --     component_category [ component classifier_reference ]
   --     [ { { subcomponent_property_association
   --         | contained_property_association }+ } ]
   --     [ in_modes ] ;

   --  subcomponent_refinement ::=
   --     defining_subcomponent_identifier : refined to
   --     component_category [ component classifier_reference ]
   --     [ { { subcomponent_property_association
   --         | contained_property_association }+ } ]
   --     [ in_modes ] ;

   --  AADL_V2
   --  subcomponent ::=
   --     defining_subcomponent_identifier :
   --     component_category
   --       [ unique_component_classifier_reference [prototype_bindings]
   --         | prototype_identifier ]
   --       [ array_dimensions ]
   --     [ { { subcomponent_property_association
   --           | contained_property_association }+ } ]
   --     [ in_modes ] ;

   --  subcomponent_refinement ::=
   --     defining_subcomponent_identifier : refined to
   --     component_category
   --       [ unique_component_classifier_reference [prototype_bindings]
   --       | prototype_identifier ]
   --       [ array_dimensions ]
   --     [ { { subcomponent_property_association
   --           | contained_property_association }+ } ]
   --     [ in_modes ] ;

   function P_Subcomponent
     (Container : Types.Node_Id;
      Refinable : Boolean) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Parser.Components.Modes;
      use Parser.Properties;
      use Parser.Identifiers;
      use Parser.Components.Arrays;
      use Parser.Components.Prototypes;
      use Ocarina.Builder.AADL.Components.Subcomponents;

      Subcomponent       : Node_Id;          --  output
      Identifier         : Node_Id;
      Is_Refinement      : Boolean;
      Category           : Component_Category;
      Component_Ref      : Node_Id;
      Component_Modes    : Node_Id;
      Array_Dimensions   : Node_Id;
      Prototype_Bindings : List_Id := No_List;
      Code               : Parsing_Code;
      Loc                : Location;
      OK                 : Boolean;

   begin
      P_Identifier_Refined_To
        (Refinable_To_RT (Refinable),
         False,
         PC_Subcomponent,
         PC_Subcomponent_Refinement,
         T_Semicolon,
         Identifier,
         Is_Refinement,
         OK);

      if not OK then
         return No_Node;
      end if;

      if Is_Refinement then
         Code := PC_Subcomponent_Refinement;
      else
         Code := PC_Subcomponent;
      end if;

      Scan_Token;
      Category := P_Component_Category;

      if Category = CC_Unknown then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Identifier then
         Restore_Lexer (Loc);
         Component_Ref := P_Entity_Reference (Code);

         if No (Component_Ref) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         --
         --  Prototypes Bindings
         --

         if AADL_Version = AADL_V2 then
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Left_Parenthesis then
               Prototype_Bindings :=
                 P_Items_List
                   (P_Prototype_Binding'Access,
                    No_Node,
                    T_Comma,
                    T_Right_Parenthesis,
                    PC_Prototype_Bindings,
                    False);

               if Is_Empty (Prototype_Bindings) then
                  DPE (PC_Subcomponent, EMC_List_Is_Empty);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
            else
               Restore_Lexer (Loc);
            end if;
         end if;
      else
         Component_Ref := No_Node;
         Restore_Lexer (Loc);
      end if;

      Subcomponent :=
        Add_New_Subcomponent
          (Loc => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
           Name                => Identifier,
           Comp_Impl           => Container,
           Is_Refinement       => Is_Refinement,
           Category            => Category,
           In_Modes            => No_Node,
           Prototypes_Bindings => Prototype_Bindings);

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Left_Square_Bracket then
         case AADL_Version is
            when AADL_V2 =>
               Array_Dimensions := P_Array_Dimensions (Subcomponent);
               if No (Array_Dimensions) then
                  DPE (Code);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
               Set_Array_Dimensions (Subcomponent, Array_Dimensions);

            when AADL_V1 =>
               DPE (Code, EMC_Not_Allowed_In_AADL_V1);
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;

      else
         Restore_Lexer (Loc);
      end if;

      OK :=
        P_Property_Associations
          (Subcomponent,
           True,
           PAT_Simple_Or_Contained,
           Code);

      if not OK then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_In then
         Component_Modes := P_In_Modes (PC_In_Modes);

         if No (Component_Modes) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Set_In_Modes (Subcomponent, Component_Modes);
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      if Subcomponent /= No_Node then
         Set_Entity_Ref (Subcomponent, Component_Ref);
      end if;

      return Subcomponent;
   end P_Subcomponent;

end Ocarina.FE_AADL.Parser.Components.Subcomponents;
