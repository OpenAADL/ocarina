------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L . P A R S E R . N A M E S P A C E S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.Namet; use Ocarina.Namet;
with Ocarina.FE_AADL.Lexer;
with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Parser.Annexes;
with Ocarina.FE_AADL.Parser.Components;
with Ocarina.FE_AADL.Parser.Properties;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.Builder.AADL.Namespaces;
with Ocarina.Options;
with Ocarina.Files;

package body Ocarina.FE_AADL.Parser.Namespaces is

   use Ocarina.ME_AADL;

   function P_Expected_Package_Name (Expected_Id : Node_Id) return Boolean;

   function P_Package_Declaration
     (Package_Spec         : Types.Node_Id;
      Private_Declarations : Boolean) return Integer;

   function P_Name_Visibility_Declaration
     (Namespace            : Types.Node_Id;
      Start_Loc            : Locations.Location;
      Private_Declarations : Boolean) return Node_Id;

   function P_Alias_Declaration
     (Namespace            : Types.Node_Id;
      Start_Loc            : Locations.Location;
      Private_Declarations : Boolean) return Types.Node_Id;

   ------------------------
   -- P_AADL_Declaration --
   ------------------------

   --  AADL_global_declaration ::= package_spec | property_set

   --  AADL_declaration        ::=   component_classifier
   --                              | port_group_classifier
   --                              | annex_library

   --  component_classifier    ::=   component_type
   --                              | component_type_extension
   --                              | component_implementation
   --                              | component_implementation_extension

   --  port_group_classifier   ::= port_group_type | port_group_type_extension

   --  NOTES:
   --     package_spec            begins with 'package'
   --     property_set            begins with 'property set'
   --     component_classifier    begins with a component_category
   --     port_group_classifier   begins with 'port type'
   --     annex_library           begins with 'annex'

   function P_AADL_Declaration
     (AADL_Specification : Types.Node_Id) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Parser.Components;
      use Parser.Properties;

      Loc : Location;

   begin
      Scan_Token;

      case Token is
         when T_Package =>
            return P_Package_Specification (AADL_Specification);

         when T_Property =>
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Set then
               return P_Property_Set (AADL_Specification, Loc);
            else
               DPE (PC_Property_Set, T_Set);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Abstract |
           T_Data        |
           T_Subprogram  |
           T_Thread      |
           T_Process     |
           T_Memory      |
           T_Processor   |
           T_Bus         |
           T_Device      |
           T_Virtual     |
           T_System      =>
            if AADL_Version = AADL_V1 then
               return P_Component (AADL_Specification);
            else
               DPE (PC_AADL_Declaration, EMC_Not_Allowed_In_AADL_V2);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Port =>
            if AADL_Version = AADL_V1 then
               Save_Lexer (Loc);
               Scan_Token;

               if Token = T_Group then
                  case AADL_Version is
                     when AADL_V1 =>
                        return P_Feature_Group_Type (AADL_Specification, Loc);
                     when others =>
                        DPE (PC_Port_Group_Type, EMC_Not_Allowed_In_AADL_V2);
                        Skip_Tokens (T_Semicolon);
                        return No_Node;
                  end case;
               else
                  DPE (PC_Port_Group_Type, T_Group);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

            else
               DPE (PC_AADL_Declaration, EMC_Not_Allowed_In_AADL_V2);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when others =>
            DPE (PC_AADL_Declaration);
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;
   end P_AADL_Declaration;

   --------------------------
   -- P_AADL_Specification --
   --------------------------

   --  AADL_specification ::= { AADL_global_declaration | AADL_declaration }+

   function P_AADL_Specification
     (AADL_Specification : Types.Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Locations;
      use Ocarina.Builder.AADL.Namespaces;

      Declaration   : Node_Id;
      OK            : Boolean := True;
      Loc           : Location;
      Specification : Node_Id;
   begin
      if AADL_Specification = No_Node then
         Specification := Initialize_Unnamed_Namespace (Token_Location);
      else
         Specification := AADL_Specification;
      end if;

      loop
         Save_Lexer (Loc);
         Scan_Token;
         exit when Token = T_EOF
           or else (AADL_Version = AADL_V2 and then Token = T_End_Annex);

         Restore_Lexer (Loc);

         Declaration := P_AADL_Declaration (Specification);

         if Declaration = No_Node then
            OK := False;   --  try to parse another declaration
         end if;
      end loop;

      if OK then
         return Specification;
      else
         return No_Node;
      end if;
   end P_AADL_Specification;

   -----------------------------
   -- P_Expected_Package_Name --
   -----------------------------

   function P_Expected_Package_Name (Expected_Id : Node_Id) return Boolean is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      Loc2, Start_Loc : Location;
      Node : constant Node_Id := New_Node (K_Identifier, Token_Location);
   begin
      Set_Corresponding_Entity (Node, No_Node);

      Save_Lexer (Start_Loc);
      Loc2 := Start_Loc;
      Scan_Token;
      if Token = T_Identifier then
         Set_Name (Node, Token_Name);
         Set_Display_Name (Node, Token_Display_Name);
      else
         Restore_Lexer (Start_Loc);
         return False;
      end if;

      loop
         Save_Lexer (Loc2);
         Scan_Token;
         if Token = T_Colon_Colon then
            Get_Name_String (Name (Node));
            Add_Str_To_Name_Buffer (Image (T_Colon_Colon));
            Set_Name (Node, Name_Find);
            Get_Name_String (Display_Name (Node));
            Add_Str_To_Name_Buffer (Image (T_Colon_Colon));
            Set_Display_Name (Node, Name_Find);
         else
            Restore_Lexer (Loc2);

            if Name (Node) = Name (Expected_Id) then
               return True;
            else
               DPE (PC_Defining_Identifier, Display_Name (Expected_Id));
               Restore_Lexer (Start_Loc);
               return False;
            end if;
         end if;

         Scan_Token;
         if Token = T_Identifier then
            Get_Name_String (Name (Node));
            Get_Name_String_And_Append (Token_Name);
            Set_Name (Node, Name_Find);
            Get_Name_String (Display_Name (Node));
            Get_Name_String_And_Append (Token_Display_Name);
            Set_Display_Name (Node, Name_Find);
         else
            Restore_Lexer (Start_Loc);
            return False;
         end if;
      end loop;
   end P_Expected_Package_Name;

   ---------------------------
   -- P_Package_Declaration --
   ---------------------------

   --  AADL_V1
   --  package_declaration ::= { aadl_declaration }+
   --     [ properties ( { property_association }+ | none_statement ) ]

   --  AADL_V2
   --  package_declaration ::= { name_visibility }* { AADL_declaration }+

   function P_Package_Declaration
     (Package_Spec         : Types.Node_Id;
      Private_Declarations : Boolean) return Integer
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Parser.Annexes;
      use Parser.Components;
      use Parser.Properties;

      pragma Assert (Kind (Package_Spec) = K_Package_Specification);

      Properties      : List_Id := No_List;
      Declaration     : Node_Id;
      Name_Visibility : Node_Id := No_Node;
      Loc             : Location;
      Nb_Items        : Integer := 0;
      Success         : Boolean := True;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      case AADL_Version is
         when AADL_V2 =>
            case Token is
               when T_With | T_Identifier | T_Renames =>
                  Name_Visibility :=
                    P_Name_Visibility_Declaration
                      (Package_Spec,
                       Loc,
                       Private_Declarations);

                  if Name_Visibility /= No_Node then
                     Nb_Items := Nb_Items + 1;
                  end if;
               when others =>
                  Restore_Lexer (Loc);
            end case;
         when others =>
            Restore_Lexer (Loc);
      end case;

      --  Parse declarations
      loop
         Save_Lexer (Loc);
         Scan_Token;

         case Token is
            when T_Abstract |
              T_Data        |
              T_Subprogram  |
              T_Thread      |
              T_Process     |
              T_Memory      |
              T_Processor   |
              T_Bus         |
              T_Device      |
              T_Virtual     |
              T_System      =>
               Declaration :=
                 P_Component
                   (Package_Spec,
                    Private_Declaration => Private_Declarations);

            when T_Port =>  --  Port group
               Scan_Token;
               if Token = T_Group then
                  case AADL_Version is
                     when AADL_V1 =>
                        Declaration :=
                          P_Feature_Group_Type
                            (Package_Spec,
                             Loc,
                             Private_Declaration => Private_Declarations);
                     when others =>
                        DPE (PC_Port_Group_Type, EMC_Not_Allowed_In_AADL_V2);
                        Skip_Tokens (T_Semicolon);
                        Success := False;
                  end case;
               else
                  DPE (PC_Port_Group_Type, T_Group);
                  Skip_Tokens (T_Semicolon);
                  Success := False;
               end if;

            when T_Feature =>  --  Feature group
               Scan_Token;
               if Token = T_Group then
                  case AADL_Version is
                     when AADL_V2 =>
                        Declaration :=
                          P_Feature_Group_Type
                            (Package_Spec,
                             Loc,
                             Private_Declaration => Private_Declarations);
                     when others =>
                        DPE
                          (PC_Feature_Group_Type,
                           EMC_Not_Allowed_In_AADL_V1);
                        Skip_Tokens (T_Semicolon);
                        Success := False;
                  end case;
               else
                  DPE (PC_Feature_Group_Type, T_Group);
                  Skip_Tokens (T_Semicolon);
                  Success := False;
               end if;

            when T_Annex =>
               Declaration := P_Annex_Subclause (Package_Spec);

            when others =>
               Restore_Lexer (Loc);
               exit;
         end case;

         if Present (Declaration) then
            if Kind (Declaration) /= K_Annex_Subclause then
               Set_Is_Private (Declaration, Private_Declarations);
            end if;
            Nb_Items := Nb_Items + 1;

         else
            Success := False;
         end if;
      end loop;

      --  Parse properties, only in AADL_V1

      if AADL_Version = AADL_V1 then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Properties then
            Properties :=
              P_Items_List
                (P_Property_Association'Access,
                 Package_Spec,
                 PC_Properties);

            if No (Properties) then
               --  Error when parsing properties, quit
               Success := False;
            else
               declare
                  P : Node_Id := First_Node (Properties);
               begin
                  while Present (P) loop
                     Set_Is_Private (P, Private_Declarations);
                     P := Next_Node (P);
                  end loop;
               end;
            end if;
         else
            --  No property declared
            Restore_Lexer (Loc);
         end if;
      end if;

      if Success then
         return Nb_Items;
      else
         return 0;
      end if;
   end P_Package_Declaration;

   -----------------------------
   -- P_Package_Specification --
   -----------------------------

   --  AADL_V1
   --  package_spec ::=
   --     package defining_package_name
   --        ( public package_declaration [ private package_declaration ] |
   --          private package_declaration )
   --     end defining_package_name;

   --  AADL_V2
   --  package_spec ::=
   --      package defining_package_name
   --         ( public package_declarations [ private package_declarations ]
   --         | private package_declarations )
   --      [ properties ( { property_association }+ | none_statement ) ]
   --  end defining_package_name ;

   function P_Package_Specification
     (Namespace : Types.Node_Id) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Entities;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Parser.Properties;
      use Ocarina.Builder.AADL.Namespaces;

      pragma Assert
        (Namespace /= No_Node
         and then Kind (Namespace) = K_AADL_Specification);

      Package_Spec                    : Node_Id;    --  result
      Defining_Name                   : Node_Id;    --  package name
      Properties                      : List_Id := No_List;
      Loc                             : Location;
      Success                         : Boolean := True;
      Nb_Public_Items                 : Integer := 0;
      Nb_Private_Items                : Integer := 0;
      Private_Section, Public_Section : Boolean := False;
   begin

      Defining_Name := P_Package_Name (No_Node);

      if No (Defining_Name) then
         --  Defining_Package_Name is not parsed correctly, quit
         DPE (PC_Defining_Name, T_Identifier);
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Package_Spec :=
        Add_New_Package (Token_Location, Defining_Name, Namespace);

      --  we do not know the parent of this package context
      --  Ex: P1::P2 is parent of P1::P2::P3 but NOT the unnamed namespace
      --  context parent will be determined in analyse phase

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Public then
         Public_Section := True;

         if Package_Has_Public_Declarations_Or_Properties (Package_Spec) then
            Success := False;
            DPE (PC_Package_Specification);
            --  XXX We should display a more explicative message to
            --  indicate that a public part already exists in this
            --  package
            return No_Node;
         end if;

         Nb_Public_Items :=
           P_Package_Declaration (Package_Spec, Private_Declarations => False);

         if Nb_Public_Items = 0 then
            Save_Lexer (Loc);

            while (Token /= T_Private)
              and then (Token /= T_End)
              and then Token /= T_EOF
            loop
               Save_Lexer (Loc);
               Scan_Token;
            end loop;

            if Token = T_Private then
               Restore_Lexer (Loc);
            else
               Skip_Tokens (T_Semicolon);

               DPE (PC_Package_Specification, EMC_Empty_Package);
               return No_Node;
            end if;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Private then
         Private_Section := True;

         if Package_Has_Private_Declarations_Or_Properties (Package_Spec) then
            Success := False;
            DPE (PC_Package_Specification);
         end if;

         Nb_Private_Items :=
           P_Package_Declaration (Package_Spec, Private_Declarations => True);

         if Nb_Private_Items = 0 then
            while Token /= T_End loop
               Save_Lexer (Loc);
               Scan_Token;
            end loop;

            Skip_Tokens (T_Semicolon);
            Success := False;

            DPE (PC_Package_Specification, EMC_Empty_Package);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Parse properties, only in AADL_V2

      if AADL_Version = AADL_V2 then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Properties then
            Properties :=
              P_Items_List
                (P_Property_Association'Access,
                 Package_Spec,
                 PC_Properties);

            if No (Properties) then
               --  Error when parsing properties, quit
               Success := False;
            end if;
         else
            --  No property declared
            Restore_Lexer (Loc);
         end if;

         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Private then
            DPE (PC_Package_Specification);
            return No_Node;
         else
            Restore_Lexer (Loc);
         end if;
      end if;

      Scan_Token;
      if Token /= T_End then
         if Nb_Private_Items = 0 then
            if Nb_Public_Items = 0 then
               DPE (PC_Package_Specification, (T_Public, T_Private, T_End));
            else
               DPE (PC_Package_Specification, (T_Private, T_End));
            end if;
         else
            DPE (PC_Package_Specification, T_End);
         end if;

         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      else
         if not Public_Section and then not Private_Section then
            --  If the package was empty
            DPE (PC_Package_Specification, (T_Public, T_Private));
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      end if;

      if not P_Expected_Package_Name (Identifier (Package_Spec)) then
         --  Error when parsing Defining_Name, quit
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (PC_Package_Specification, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      if Success then
         return Package_Spec;
      else
         return No_Node;
      end if;
   end P_Package_Specification;

   --------------------
   -- P_Package_Name --
   --------------------

   --  package_name ::=
   --     { package_identifier :: }* package_identifier

   function P_Package_Name (Container : Node_Id) return Node_Id is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Identifiers;
      use Ocarina.Builder.AADL.Namespaces;

      Start_Loc    : Location;
      Package_Name : Node_Id;
      List_Names   : List_Id;

   begin
      Save_Lexer (Start_Loc);

      List_Names :=
        P_Items_List (P_Identifier'Access, Container, T_Colon_Colon);

      if Is_Empty (List_Names) then
         DPE (PC_Package_Name, EMC_List_Is_Empty);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Package_Name := Add_New_Package_Name (Start_Loc, List_Names);

      if Package_Name = No_Node then
         DPE (PC_Package_Name);
         return No_Node;
      end if;

      return Package_Name;
   end P_Package_Name;

   -----------------------------------
   -- P_Name_Visibility_Declaration --
   -----------------------------------

   --  AADL_V2
   --  name_visibility_declaration ::=
   --     import_declaration | alias_declaration

   function P_Name_Visibility_Declaration
     (Namespace            : Types.Node_Id;
      Start_Loc            : Locations.Location;
      Private_Declarations : Boolean) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.Builder.AADL.Namespaces;

      Loc             : Location;
      List_Items      : List_Id;
      Item            : Node_Id;
      Name_Visibility : Node_Id;
      Nb_Items        : Integer := 0;
   begin
      Save_Lexer (Loc);
      List_Items := New_List (K_List_Id, Loc);

      loop
         case Token is
            when T_With =>
               Item :=
                 P_Import_Declaration (Namespace, Loc, Private_Declarations);

            when T_Renames | T_Identifier =>
               Item :=
                 P_Alias_Declaration (Namespace, Loc, Private_Declarations);
            when others =>
               Restore_Lexer (Loc);
               exit;
         end case;

         if Item /= No_Node then
            Append_Node_To_List (Item, List_Items);
            Nb_Items := Nb_Items + 1;
         else
            DPE (PC_Name_Visibility_Declaration);
            return No_Node;
         end if;

         Save_Lexer (Loc);
         Scan_Token;
      end loop;

      if Nb_Items /= 0 then
         Name_Visibility :=
           Add_New_Name_Visibility_Declaration
             (Start_Loc,
              Namespace,
              List_Items,
              Private_Declarations);
      else
         DPE (PC_Name_Visibility_Declaration);
         return No_Node;
      end if;

      return Name_Visibility;
   end P_Name_Visibility_Declaration;

   --------------------------
   -- P_Import_Declaration --
   --------------------------

   --  AADL_V2
   --  import_declaration ::=
   --    with ( package_name | property_set_identifier )
   --         { , ( package_name | property_set_identifier ) }+ ;

   function P_Import_Declaration
     (Namespace            : Types.Node_Id;
      Start_Loc            : Locations.Location;
      Private_Declarations : Boolean := False) return Types.Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Identifiers;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.Builder.AADL.Namespaces;

      Loc          : Location;
      Node         : Node_Id := No_Node;
      Imports_List : List_Id := No_List;
      Import_Node  : Node_Id := No_Node;
   begin
      Imports_List := New_List (K_List_Id, Start_Loc);

      loop
         Save_Lexer (Loc);
         Node := P_Identifier (Namespace);
         if No (Node) then
            DPE (PC_Import_Declaration, T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;

         case Token is
            when T_Comma =>
               Append_Node_To_List (Node, Imports_List);

            when T_Colon_Colon =>
               Restore_Lexer (Loc);
               Node := P_Package_Name (Namespace);
               if No (Node) then
                  DPE (PC_Import_Declaration);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               else
                  Append_Node_To_List (Node, Imports_List);
               end if;

               Scan_Token;
               if Token /= T_Comma then
                  exit;
               end if;

            when T_Semicolon =>
               Append_Node_To_List (Node, Imports_List);
               exit;

            when others =>
               DPE
                 (PC_Import_Declaration,
                  (T_Comma, T_Colon_Colon, T_Semicolon));
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;
      end loop;

      if No (Imports_List) then
         DPE (PC_Import_Declaration, T_With);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      if Token /= T_Semicolon then
         DPE (PC_Import_Declaration, T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         Import_Node :=
           Add_New_Import_Declaration
             (Start_Loc,
              Namespace,
              Imports_List,
              Private_Declarations);

         if Ocarina.Options.Auto_Load_AADL_Files
           and then AADL_Version = AADL_V2
           and then not Ocarina.Options.Use_Scenario_File
         then
            declare
               I : Node_Id;
            begin
               I := First_Node (Imports_List);

               while Present (I) loop
                  if Kind (I) = K_Identifier then
                     Ocarina.Files.Add_File_To_Parse_List
                       (Name (I), Add_Suffix => True);
                  elsif Kind (I) = K_Package_Name then
                     declare
                        J : Node_Id;
                     begin
                        J :=
                          First_Node
                            (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers (I));
                        if Present (J) then
                           Get_Name_String (Name (J));
                           J := Next_Node (J);
                           while Present (J) loop
                              Add_Str_To_Name_Buffer
                                ("-" & Get_Name_String (Name (J)));
                              J := Next_Node (J);
                           end loop;
                           Ocarina.Files.Add_File_To_Parse_List
                             (Name_Find, Add_Suffix => True);
                        end if;
                     end;
                  end if;

                  I := Next_Node (I);
               end loop;
            end;
         end if;

         return Import_Node;
      end if;
   end P_Import_Declaration;

   -------------------------
   -- P_Alias_Declaration --
   -------------------------

   --  AADL_V2
   --  alias_declaration ::=
   --    ( defining_identifier renames package package_name ; )
   --    | ( [ defining_identifier ] renames
   --          ( component_category unique_component_type_reference |
   --          |  feature group unique_feature_group_type_reference ) ; )
   --    | ( renames package_name::all ; )

   function P_Alias_Declaration
     (Namespace            : Types.Node_Id;
      Start_Loc            : Locations.Location;
      Private_Declarations : Boolean) return Types.Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Identifiers;
      use Components;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.Builder.AADL.Namespaces;

      Loc, Loc2      : Location;
      Is_All         : Boolean                         := False;
      Name           : Node_Id                         := No_Node;
      Package_Name   : Node_Id                         := No_Node;
      Identifier     : Node_Id                         := No_Node;
      Node           : Node_Id                         := No_Node;
      Classifier_Ref : Node_Id                         := No_Node;
      Identifiers    : List_Id                         := No_List;
      Component_Cat  : Component_Category              := CC_Unknown;
      Entity_Cat     : Ocarina.ME_AADL.Entity_Category := EC_Undefined;
   begin

      if Token = T_Identifier then
         Identifier := Make_Current_Identifier (No_Node);
         Scan_Token;
      end if;

      if Token /= T_Renames then
         DPE (PC_Alias_Declaration, T_Renames);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Package =>
            Entity_Cat   := EC_Package;
            Package_Name := P_Package_Name (Namespace);

         when T_Abstract |
           T_Data        |
           T_Thread      |
           T_Subprogram  |
           T_Process     |
           T_Processor   |
           T_Virtual     |
           T_Memory      |
           T_Bus         |
           T_Device      |
           T_System      =>
            Entity_Cat     := EC_Component;
            Component_Cat  := P_Component_Category;
            Classifier_Ref := P_Entity_Reference (PC_Alias_Declaration);

         when T_Feature =>
            Save_Lexer (Loc);
            Scan_Token;

            if Token /= T_Group then
               DPE (PC_Alias_Declaration, T_Group);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            else
               Entity_Cat     := EC_Feature_Group_Type;
               Classifier_Ref := P_Entity_Reference (PC_Alias_Declaration);
            end if;

         when T_Identifier =>
            Save_Lexer (Loc2);
            Restore_Lexer (Loc);
            Entity_Cat := EC_Package;

            Identifiers := New_List (K_List_Id, Token_Location);
            loop
               Name := P_Identifier (No_Node);
               if Present (Name) then
                  Append_Node_To_List (Name, Identifiers);
               else
                  DPE (PC_Alias_Declaration, T_Identifier);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Save_Lexer (Loc);
               Scan_Token;
               if Token = T_Colon_Colon then
                  Save_Lexer (Loc);
                  Scan_Token;
                  if Token = T_All then
                     Is_All := True;
                     exit;
                  else
                     Restore_Lexer (Loc);
                  end if;
               else
                  DPE (PC_Alias_Declaration, T_Colon_Colon);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
            end loop;

            Package_Name := Add_New_Package_Name (Loc2, Identifiers);
            if No (Package_Name) then
               DPE (PC_Alias_Declaration);
               return No_Node;
            end if;

         when others =>
            DPE (PC_Alias_Declaration, T_Semicolon);
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_Semicolon then
         DPE (PC_Alias_Declaration, T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         Node :=
           Add_New_Alias_Declaration
             (Start_Loc,
              Namespace,
              Identifier,
              Package_Name,
              Classifier_Ref,
              Entity_Cat,
              Component_Cat,
              Is_All,
              Private_Declarations);
      end if;

      return Node;

   end P_Alias_Declaration;

end Ocarina.FE_AADL.Parser.Namespaces;
