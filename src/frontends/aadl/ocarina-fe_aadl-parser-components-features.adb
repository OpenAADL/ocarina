------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.FE_AADL.PARSER.COMPONENTS.FEATURES                 --
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
with Ocarina.FE_AADL.Lexer;
with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Parser.Components.Arrays;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.FE_AADL.Parser.Properties;
with Ocarina.Builder.AADL.Components.Features;

package body Ocarina.FE_AADL.Parser.Components.Features is

   function P_In_Out_Item
     (Container     : Types.Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      Code          : Parsing_Code) return Node_Id;
   --  Parse item begins with 'in' or 'out'
   --  Examples: Port_Spec, Parameter, ... and refinements

   function P_Feature_Group_Spec
     (Container     : Types.Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean) return Node_Id;
   --  Current token must be reserved word 'feature' or 'port'
   --  If Is_Refinement = TRUE,
   --  And If Rules is AADL_V1 parse a Port_Group_Spec
   --  And If Rules is AADL_V2 parse a Feature_Group_Spec

   function P_Port_Spec
     (Container     : Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      Is_In         : Boolean;
      Is_Out        : Boolean) return Node_Id;
   --  Current token must be reserved word 'data' or 'event'
   --  If Is_Refinement = TRUE, parse a Port_Refinement

   function P_Parameter
     (Container     : Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      Is_In         : Boolean;
      Is_Out        : Boolean) return Node_Id;
   --  Parse Parameter and Parameter_Refinement

   function P_Subprogram_Spec
     (Container     : Types.Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean) return Node_Id;
   --  Current token must be reserved word 'subprogram' or 'server'
   --  If Is_Refinement = TRUE, parse Data_Subprogram_Refinement or
   --                                 Server_Subprogram_Refinement
   --  else, parse Data_Subprogram_Spec or Server_Subprogram

   function P_Subcomponent_Access
     (Container     : Types.Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean) return Node_Id;
   --  Parse Subcomponent_Access and Subcomponent_Access_Refinement

   -------------------
   -- P_In_Out_Item --
   -------------------

   function P_In_Out_Item
     (Container     : Types.Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      Code          : Parsing_Code) return Node_Id
   is
      use Ocarina.ME_AADL.Tokens;
      use Lexer;

      Is_In  : Boolean := False;
      Is_Out : Boolean := False;

   begin
      if Token = T_In then
         Is_In := True;
         Scan_Token;
         if Token = T_Out then
            Is_Out := True;
            Scan_Token;
         end if;
      else
         Is_Out := True;
         Scan_Token;
         if Token = T_In then
            Is_In := True;
            Scan_Token;
         end if;
      end if;

      case Token is
         when T_Data | T_Event | T_Feature =>
            if Code = PC_Parameter or else Code = PC_Parameter_Refinement then
               DPE (Code, T_Parameter);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            else
               return P_Port_Spec
                   (Container,
                    Identifier,
                    Is_Refinement,
                    Is_In,
                    Is_Out);
            end if;

         when T_Parameter =>
            if Code = PC_Port_Spec or else Code = PC_Port_Refinement then
               DPE (Code, (T_Data, T_Event));
               Skip_Tokens (T_Semicolon);
               return No_Node;
            else
               return P_Parameter
                   (Container,
                    Identifier,
                    Is_Refinement,
                    Is_In,
                    Is_Out);
            end if;

         when others =>
            DPE (Code);
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;
   end P_In_Out_Item;

   -----------------
   -- P_Parameter --
   -----------------

   --  parameter ::=
   --     defining_parameter_identifier :
   --        ( in | out | in out ) parameter data_classifier_reference
   --     [ { { parameter_property_association }+ } ] ;

   --  parameter_refinement ::=
   --     defining_parameter_identifier : refined to
   --        ( in | out | in out ) parameter data_classifier_reference
   --     [ { { parameter_property_association }+ } ] ;

   function P_Parameter
     (Container     : Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      Is_In         : Boolean;
      Is_Out        : Boolean) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Parser.Properties;
      use Parser.Identifiers;
      use Ocarina.Builder.AADL.Components.Features;

      Param     : Node_Id := No_Node;
      Class_Ref : Node_Id := No_Node;
      Code      : Parsing_Code;
      Loc       : Location;
      OK        : Boolean;

   begin
      if Is_Refinement then
         Code := PC_Parameter_Refinement;
      else
         Code := PC_Parameter;
      end if;

      Save_Lexer (Loc);
      Scan_Token;   --  next token and ignore 'parameter'

      --  The data classifier is not mandatory
      if Token = T_Identifier then
         Restore_Lexer (Loc);
         Class_Ref := P_Entity_Reference (Code);

         if No (Class_Ref) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         Class_Ref := No_Node;
      end if;

      Param :=
        Add_New_Parameter
          (Loc           => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
           Container     => Container,
           Name          => Identifier,
           Is_In         => Is_In,
           Is_Out        => Is_Out,
           Is_Refinement => Is_Refinement);

      OK := P_Property_Associations (Param, True, PAT_Simple, Code);

      if not OK then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      if Param /= No_Node then
         Set_Entity_Ref (Param, Class_Ref);
      end if;

      return Param;
   end P_Parameter;

   -----------------
   -- P_Port_Spec --
   -----------------

   --  port_spec ::=
   --     defining_port_identifier : ( in | out | in out ) port_type
   --  XXX dimensions ???
   --        [ { { port_property_association }+ } ] ;

   --  port_refinement ::=
   --     defining_port_identifier : refined to
   --        ( in | out | in out ) port_type
   --        [ { { port_property_association }+ } ] ;

   function P_Port_Spec
     (Container     : Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      Is_In         : Boolean;
      Is_Out        : Boolean) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Locations;
      use Parser.Properties;
      use Parser.Identifiers;
      use Ocarina.Builder.AADL.Components.Features;
      use Parser.Components.Arrays;

      Class_Ref        : Node_Id := No_Node;
      Port_Spec        : Node_Id := No_Node;
      Is_Data          : Boolean := False;
      Is_Event         : Boolean := False;
      Is_Feature       : Boolean := False;
      Code             : Parsing_Code;
      OK               : Boolean;
      Loc              : Location;
      Array_Dimensions : Node_Id;

   begin
      if Is_Refinement then
         Code := PC_Port_Refinement;
      else
         Code := PC_Port_Spec;
      end if;

      if Token = T_Event then
         Is_Event := True;
         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_Data then
            Is_Data := True;
         else
            Restore_Lexer (Loc);
         end if;

      elsif Token = T_Data then
         Is_Data := True;

      elsif Token = T_Feature then
         Is_Feature := True;

      else
         DPE (PC_Port_Type, ((T_Event, T_Data)));
         return No_Node;
      end if;

      if not Is_Feature then
         Scan_Token;

         if Token /= T_Port then
            DPE (PC_Port_Type, T_Port);
            return No_Node;
         end if;
      end if;

      if Is_Data or else Is_Feature then
         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_Identifier then
            Restore_Lexer (Loc);
            Class_Ref := P_Entity_Reference (PC_Port_Type);

            if No (Class_Ref) then
               --  Error when parsing Classifier_Reference, quit
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
         end if;
      end if;

      Port_Spec :=
        Add_New_Port_Spec
          (Loc => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
           Container         => Container,
           Name              => Identifier,
           Is_In             => Is_In,
           Is_Out            => Is_Out,
           Is_Event          => Is_Event,
           Is_Data           => Is_Data,
           Is_Feature        => Is_Feature,
           Is_Refinement     => Is_Refinement,
           Associated_Entity => Class_Ref);

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Left_Square_Bracket then
         case AADL_Version is
            when AADL_V2 =>
               Array_Dimensions := P_Array_Dimensions (Port_Spec);
               if No (Array_Dimensions) then
                  DPE (Code);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
               Set_Array_Dimensions (Port_Spec, Array_Dimensions);

            when AADL_V1 =>
               DPE (Code, EMC_Not_Allowed_In_AADL_V1);
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;

      else
         Restore_Lexer (Loc);
      end if;

      OK := P_Property_Associations (Port_Spec, True, PAT_Simple, Code);

      if not OK then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      return Port_Spec;
   end P_Port_Spec;

   -----------------------
   -- P_Subprogram_Spec --
   -----------------------

   --  data_subprogram_spec ::=
   --     defining_subprogram_identifier_list : subprogram
   --     [ unique_subprogram_reference ]
   --     [ { { subprogram_property_association }+ } ] ;

   --  data_subprogram_refinement ::=
   --     defining_subprogram_identifier_list : refined to subprogram
   --     [ unique_subprogram_reference ]
   --     [ { { subprogram_property_association }+ } ] ;

   --  server_subprogram ::=
   --     defining_subprogram_identifier_list : server subprogram
   --     [ unique_subprogram_reference ]
   --     [ { { subprogram_property_association }+ } ] ;

   --  server_subprogram_refinement ::=
   --     defining_subprogram_identifier_list : refined to server subprogram
   --     [ unique_subprogram_reference ]
   --     [ { { subprogram_property_association }+ } ] ;

   --  unique_subprogram_reference ::=
   --     subprogram_classifier_reference |
   --     subprogram_feature_classifier_reference

   --  subprogram_feature_classifier_reference ::=
   --     [ package_name :: ] data_type_identifier . subprogram_identifier

   function P_Subprogram_Spec
     (Container     : Types.Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Parser.Properties;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.Builder.AADL.Components.Features;

      Subprog_Spec : Node_Id := No_Node;
      Is_Server    : Boolean := False;
      Subprog_Ref  : Node_Id := No_Node;
      Loc          : Location;
      Code         : Parsing_Code;
      OK           : Boolean;

      --  Sub-function determining parsing code to display error messages

      function Subprogram_Parsing_Code return Parsing_Code;
      pragma Inline (Subprogram_Parsing_Code);

      function Subprogram_Parsing_Code return Parsing_Code is
      begin
         if Is_Refinement then
            if Is_Server then
               return PC_Server_Subprogram_Refinement;
            else
               return PC_Data_Subprogram_Refinement;
            end if;
         else
            if Is_Server then
               return PC_Server_Subprogram;
            else
               return PC_Data_Subprogram_Spec;
            end if;
         end if;
      end Subprogram_Parsing_Code;

   begin
      if Token = T_Server then
         Is_Server := True;
         Scan_Token;
      end if;

      Code := Subprogram_Parsing_Code;

      if Token /= T_Subprogram then
         DPE (Code, T_Subprogram);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Identifier then
         Restore_Lexer (Loc);
         Subprog_Ref := P_Entity_Reference (Code);

         if No (Subprog_Ref) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      if Is_Server then
         Subprog_Spec :=
           Add_New_Server_Subprogram
             (Loc => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
              Name          => Identifier,
              Container     => Container,
              Is_Refinement => Is_Refinement);
      else
         Subprog_Spec :=
           Add_New_Data_Subprogram_Spec
             (Loc => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
              Name          => Identifier,
              Container     => Container,
              Is_Refinement => Is_Refinement);
      end if;

      Set_Entity_Ref (Subprog_Spec, Subprog_Ref);

      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_Semicolon then
         Restore_Lexer (Loc);
         OK :=
           P_Property_Associations
             (Subprog_Spec,
              True,
              PAT_Simple_Or_Contained,
              Code);

         if not OK then
            Subprog_Spec := No_Node;
         else

            Save_Lexer (Loc);
            Scan_Token;
            if Token /= T_Semicolon then
               DPE (Code, T_Semicolon);
               Restore_Lexer (Loc);
               return No_Node;
            end if;
         end if;
      end if;

      return Subprog_Spec;
   end P_Subprogram_Spec;

   ---------------------------
   -- P_Subcomponent_Access --
   ---------------------------

   --  AADL_V1
   --  subcomponent_access ::=
   --      defining_subcomponent_access_identifier :
   --          subcomponent_access_classifier
   --      [ { { access_property_association }+ } ] ;

   --  subcomponent_access_refinement ::=
   --      defining_subcomponent_access_identifier : refined to
   --          subcomponent_access_classifier
   --      [ { { access_property_association }+ } ] ;

   --  AADL_V2
   --  subprogram_access ::=
   --      defining_subprogram_access_identifier :
   --         ( provides | requires ) subprogram access
   --            [ subprogram_unique_component_classifier_reference
   --            | prototype_identifier ]
   --      [ { { access_property_association }+ } ] ;

   function P_Subcomponent_Access
     (Container     : Types.Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Parser.Properties;
      use Parser.Identifiers;
      use Ocarina.Builder.AADL.Components.Features;

      Subcomp_Access        : Node_Id := No_Node;
      Subcomp_Access_Class  : Node_Id := No_Node;
      Is_Provided           : Boolean := False;
      Subcomponent_Category : Component_Category;
      Code                  : Parsing_Code;
      OK                    : Boolean;
      Loc                   : Location;

   begin
      if Token = T_Provides then
         Is_Provided := True;
      elsif Token /= T_Requires then
         DPE (PC_Subcomponent_Access_Classifier, (T_Provides, T_Requires));
         return No_Node;
      end if;

      Scan_Token;
      if Token = T_Data then
         Subcomponent_Category := CC_Data;
      elsif Token = T_Bus then
         Subcomponent_Category := CC_Bus;
      elsif Token = T_Subprogram then
         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_Group then
            Subcomponent_Category := CC_Subprogram_Group;
         else
            Subcomponent_Category := CC_Subprogram;
            Restore_Lexer (Loc);
         end if;
      else
         if AADL_Version = AADL_V1 then
            DPE (PC_Subcomponent_Access_Classifier, (T_Data, T_Bus));
         else
            DPE
              (PC_Subcomponent_Access_Classifier,
               (T_Data, T_Bus, T_Subprogram));
         end if;
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Access then
         DPE (PC_Subcomponent_Access_Classifier, T_Access);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Identifier then
         Restore_Lexer (Loc);
         Subcomp_Access_Class :=
           P_Entity_Reference (PC_Subcomponent_Access_Classifier);

         if Subcomp_Access_Class = No_Node then
            --  Error when parsing Subcomponent_Access_Classifier, quit
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      if Is_Refinement then
         Code := PC_Subcomponent_Access_Refinement;
      else
         Code := PC_Subcomponent_Access;
      end if;

      Subcomp_Access :=
        Add_New_Subcomponent_Access
          (Loc           => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
           Name          => Identifier,
           Container     => Container,
           Is_Refinement => Is_Refinement,
           Category      => Subcomponent_Category,
           Is_Provided   => Is_Provided);

      OK := P_Property_Associations (Subcomp_Access, True, PAT_Access, Code);

      if not OK then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      if Subcomp_Access /= No_Node then
         Set_Entity_Ref (Subcomp_Access, Subcomp_Access_Class);
      end if;

      return Subcomp_Access;
   end P_Subcomponent_Access;

   ---------------
   -- P_Feature --
   ---------------

   --  AADL_V1
   --  feature ::=   port_spec | port_group_spec
   --              | server_subprogram | data_subprogram_spec
   --              | subcomponent_access | parameter
   --
   --  feature_refinement ::=   port_refinement
   --                         | port_group_refinement
   --                         | server_subprogram_refinement
   --                         | data_subprogram_refinement
   --                         | subcomponent_access_refinement
   --                         | parameter_refinement
   --  Note : Feature Group design Port Group in AADL_V1

   --  AADL_V2
   --  feature ::=   port_spec |
   --              | feature_group_spec
   --              | subcomponent_access
   --              | parameter
   --
   --  feature_refinement ::=   port_refinement
   --                         | feature_group_refinement
   --                         | subcomponent_access_refinement
   --                         | parameter_refinement

   function P_Feature
     (Container : Types.Node_Id;
      Refinable : Boolean) return Node_Id
   is
      use Parser.Identifiers;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;

      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      Code          : Parsing_Code;
      OK            : Boolean;
      Node          : Node_Id;
   begin
      P_Identifier_Refined_To
        (Refinable_To_RT (Refinable),
         False,
         PC_Feature,
         PC_Feature_Refinement,
         T_Semicolon,
         Identifier,
         Is_Refinement,
         OK);
      if not OK then
         return No_Node;
      end if;
      if Is_Refinement then
         Code := PC_Feature_Refinement;
      else
         Code := PC_Feature;
      end if;

      Scan_Token;

      case Token is
         when T_In | T_Out =>
            Node := P_In_Out_Item (Container, Identifier, Is_Refinement, Code);

         when T_Port | T_Feature =>
            Node :=
              P_Feature_Group_Spec (Container, Identifier, Is_Refinement);

         when T_Server | T_Subprogram =>
            case AADL_Version is
               when AADL_V1 =>
                  Node :=
                    P_Subprogram_Spec (Container, Identifier, Is_Refinement);

               when AADL_V2 =>
                  DPE (Code, EMC_Not_Allowed_In_AADL_V2);
                  Skip_Tokens (T_Semicolon);
                  Node := No_Node;
            end case;

         when T_Provides | T_Requires =>
            Node :=
              P_Subcomponent_Access (Container, Identifier, Is_Refinement);

         when others =>
            DPE (Code);
            Skip_Tokens (T_Semicolon);
            Node := No_Node;
      end case;

      return Node;
   end P_Feature;

   --------------------------
   -- P_Feature_Refinement --
   --------------------------

   --  AADL_V1
   --  feature_refinement ::=   port_refinement
   --                         | port_group_refinement
   --                         | server_subprogram_refinement
   --                         | data_subprogram_refinement
   --                         | subcomponent_access_refinement
   --                         | parameter_refinement

   --  AADL_V2
   --  feature_refinement ::=   port_refinement
   --                         | feature_group_refinement
   --                         | subcomponent_access_refinement
   --                         | parameter_refinement

   function P_Feature_Refinement (Container : Types.Node_Id) return Node_Id is
      use Parser.Identifiers;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert
        (Container /= No_Node
         and then
         (Kind (Container) = K_Component_Implementation
          or else Kind (Container) = K_Component_Type
          or else Kind (Container) = K_Feature_Group_Type));

      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      OK            : Boolean;

   begin
      P_Identifier_Refined_To
        (RT_Refinement,
         False,
         PC_Feature,
         PC_Feature_Refinement,
         T_Semicolon,
         Identifier,
         Is_Refinement,
         OK);
      if not OK then
         return No_Node;
      end if;

      Scan_Token;
      case Token is
         when T_In | T_Out =>
            return P_In_Out_Item
                (Container,
                 Identifier,
                 True,
                 PC_Feature_Refinement);

         when T_Port =>
            return P_Feature_Group_Spec (Container, Identifier, True);

         when T_Server =>
            case AADL_Version is
               when AADL_V1 =>
                  return P_Subprogram_Spec (Container, Identifier, True);

               when others =>
                  DPE (PC_Feature_Refinement, EMC_Not_Allowed_In_AADL_V2);
                  Skip_Tokens ((T_End, T_Semicolon));
                  return No_Node;
            end case;

         when T_Subprogram =>
            return P_Subprogram_Spec (Container, Identifier, True);

         when T_Provides | T_Requires =>
            return P_Subcomponent_Access (Container, Identifier, True);

         when others =>
            DPE (PC_Feature_Refinement);
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;
   end P_Feature_Refinement;

   --------------------------
   -- P_Feature_Group_Spec --
   --------------------------

   --  AADL_V1

   --  port_group_spec ::=
   --     defining_port_group_identifier : port group
   --                                      unique_port_group_type_reference
   --     [ { { portgroup_property_association }+ } ] ;

   --  port_group_refinement ::=
   --     defining_port_group_identifier : refined to port group
   --                                      unique_port_group_type_reference
   --     [ { { portgroup_property_association }+ } ] ;

   --  unique_port_group_type_reference ::=
   --     [ package_name :: ] port_group_type_identifier

   --  AADL_V2

   --  feature_group_spec ::=
   --     defining_feature_group_identifier : feature group
   --     [ [ inverse of ]
   --     ( unique_feature_group_type_reference [ prototype_bindings ] )
   --     | prototype_identifier ) ]
   --     [ { { featuregroup_property_association }+ } ] ;

   --  feature_group_refinement ::=
   --     defining_feature_group_identifier : refined to feature group
   --     [ [ inverse of ]
   --       ( unique_feature_group_type_reference [ prototype_bindings ] )
   --       | prototype_identifier )
   --     ]
   --     [ { { featuregroup_property_association }+ } ] ;

   --  unique_feature_group_type_reference ::=
   --     [ package_name :: ] feature_group_type_identifier

   function P_Feature_Group_Spec
     (Container     : Types.Node_Id;
      Identifier    : Node_Id;
      Is_Refinement : Boolean) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Locations;
      use Parser.Properties;
      use Parser.Identifiers;
      use Ocarina.Builder.AADL.Components.Features;

      pragma Assert
        (Container /= No_Node
         and then
         (Kind (Container) = K_Component_Implementation
          or else Kind (Container) = K_Component_Type
          or else Kind (Container) = K_Feature_Group_Type));

      Inverse_Of         : Node_Id := No_Node;
      Group_Type_Ref     : Node_Id := No_Node;
      Feature_Group_Spec : Node_Id := No_Node;
      Code               : Parsing_Code;
      OK                 : Boolean;
      Loc                : Location;

   begin
      if Is_Refinement then
         Code := PC_Feature_Group_Or_Port_Group_Refinement;
      else
         Code := PC_Port_Group_Spec_Or_Feature_Group_Spec;
      end if;

      --  ignore 'port' or 'feature'
      Scan_Token;

      if Token /= T_Group then
         DPE (Code, T_Group);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Identifier then
         Restore_Lexer (Loc);
         Group_Type_Ref :=
           P_Entity_Reference (PC_Unique_Feature_Group_Type_Reference);

         if No (Group_Type_Ref) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

      elsif Token = T_Inverse then
         case AADL_Version is
            when AADL_V2 =>
               Scan_Token;
               if Token /= T_Of then
                  DPE (Code, T_Of);
                  Skip_Tokens ((T_End, T_Semicolon));
                  return No_Node;
               end if;

               Inverse_Of := P_Entity_Reference (Code);

               if No (Inverse_Of) then
                  --  Error when parsing
                  --  Unique_Feature_Group_Type_Reference, quit
                  Skip_Tokens ((T_End, T_Semicolon));
                  return No_Node;
               end if;

            when others =>
               DPE (Code, EMC_Not_Allowed_In_AADL_V1);
               Skip_Tokens ((T_End, T_Semicolon));
               return No_Node;
         end case;

      else
         Restore_Lexer (Loc);
         Group_Type_Ref := No_Node;
         --  OK, no port group type is given
      end if;

      Feature_Group_Spec :=
        Add_New_Feature_Group_Spec
          (Loc           => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
           Name          => Identifier,
           Container     => Container,
           Is_Refinement => Is_Refinement);

      OK :=
        P_Property_Associations (Feature_Group_Spec, True, PAT_Simple, Code);

      if not OK then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Set_Entity_Ref (Feature_Group_Spec, Group_Type_Ref);
      Set_Inverse_Of (Feature_Group_Spec, Inverse_Of);

      return Feature_Group_Spec;
   end P_Feature_Group_Spec;

   -------------------------------------------------
   -- P_Feature_Group_or_Port_Group_or_Port_Spec  --
   -------------------------------------------------

   function P_Feature_Group_Or_Port_Group_Or_Port_Spec
     (Container : Types.Node_Id;
      Refinable : Boolean) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.FE_AADL.Parser.Identifiers;

      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      OK            : Boolean;

   begin
      P_Identifier_Refined_To
        (Refinable_To_RT (Refinable),
         False,
         PC_Feature_Group_Or_Port_Group_Or_Port_Spec,
         PC_Feature,
         T_Semicolon,
         Identifier,
         Is_Refinement,
         OK);
      if not OK then
         return No_Node;
      end if;

      Scan_Token;
      case Token is
         when T_In | T_Out =>  --  parse port_spec or port_refinement
            if Is_Refinement then
               return P_In_Out_Item
                   (Container     => Container,
                    Identifier    => Identifier,
                    Is_Refinement => True,
                    Code          => PC_Port_Refinement);
            else
               return P_In_Out_Item
                   (Container     => Container,
                    Identifier    => Identifier,
                    Is_Refinement => False,
                    Code          => PC_Port_Spec);
            end if;

         --  parse port_group_spec or port_group_refinement or
         --        feature_group_spec or feature_group_refinement
         when T_Port | T_Feature =>
            return P_Feature_Group_Spec
                (Container     => Container,
                 Identifier    => Identifier,
                 Is_Refinement => Is_Refinement);

         when others =>
            if Is_Refinement then
               case AADL_Version is
                  when AADL_V1 =>
                     DPE
                       (PC_Port_Refinement_Or_Port_Group_Refinement,
                        (T_In, T_Out, T_Port));
                  when AADL_V2 =>
                     DPE
                       (PC_Feature_Refinement_Or_Feature_Group_Refinement,
                        (T_In, T_Out, T_Port, T_Feature));
               end case;
            else
               DPE
                 (PC_Feature_Group_Or_Port_Group_Or_Port_Spec,
                  (T_In, T_Out, T_Port, T_Feature));
            end if;
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;
   end P_Feature_Group_Or_Port_Group_Or_Port_Spec;

end Ocarina.FE_AADL.Parser.Components.Features;
