------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . F E _ A A D L . P A R S E R . A N N E X E S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2016 ESA & ISAE.      --
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
with Ocarina.FE_AADL.Lexer;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.FE_AADL.Parser.Components.Modes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.Tokens;
with Ocarina.Builder.AADL.Annexes;
with Ocarina.Parser;
with Ocarina.Options;

package body Ocarina.FE_AADL.Parser.Annexes is

   function P_Annex
     (Namespace           : Types.Node_Id;
      Code                : Parsing_Code;
      Private_Declaration : Boolean) return Node_Id;
   --  Parse Annex_Library or Annex_Subclause, current token must be 'annex'

   -------------
   -- P_Annex --
   -------------

   function P_Annex
     (Namespace           : Types.Node_Id;
      Code                : Parsing_Code;
      Private_Declaration : Boolean) return Node_Id
   is

      use Lexer;
      use Locations;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.FE_AADL.Parser.Components.Modes;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Builder.AADL.Annexes;
      use Ocarina.Parser;
      use Ocarina.Options;

      Annex           : Node_Id;   --  output
      Annex_Root      : Node_Id           := No_Node;
      Identifier      : Node_Id;
      In_Modes        : Node_Id           := No_Node;
      Loc             : Location;
      Loc_Start_Annex : Location;
      Annex_Content   : Name_Id           := No_Name;
      Annex_Location  : constant Location := Token_Location;
   begin

      Identifier := P_Identifier (No_Node);
      if Identifier = No_Node then
         DPE (Code, T_Identifier);
         Skip_Tokens ((T_End_Annex, T_Semicolon));
         return No_Node;
      end if;

      Scan_Token;

      if AADL_Version = AADL_V2 and then Token = T_None then
         Annex_Content := No_Name;
      elsif Token = T_Begin_Annex then
         Save_Lexer (Loc_Start_Annex);
         if Perform_Annex_Action (Name (Identifier)) then
            Annex_Root := Parse
              (Name (Identifier),
               No_Node,
               Loc_Start_Annex,
               No_Location,
               Namespace);
         end if;

         --  Keep the raw text of the annex in case we do not have a
         --  pretty printer for the current annex.

         Restore_Lexer (Loc_Start_Annex);
         Scan_Raw_Text (T_End_Annex);
         Annex_Content := Raw_Text_Value;

         Scan_Token;
         if Token /= T_End_Annex then
            DPE (Code, T_End_Annex);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         DPE (Code, T_Begin_Annex);
         Skip_Tokens ((T_End_Annex, T_Semicolon));
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --  AADLv2 Parse In_Modes in Annex_Subclause
      if AADL_Version = AADL_V2
        and then Code = PC_Annex_Subclause
        and then Token = T_In
      then
         In_Modes := P_In_Modes (PC_In_Modes);

         if No (In_Modes) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Save_Lexer (Loc);
         Scan_Token;
      end if;

      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      if Code = PC_Annex_Library then
         Annex :=
           Add_New_Annex_Library
             (Annex_Location,
              Identifier,
              Namespace,
              Is_Private => Private_Declaration);
      else
         Annex :=
           Add_New_Annex_Subclause
             (Annex_Location,
              Identifier,
              Namespace,
              In_Modes);
      end if;

      if Present (Annex) then
         Set_Annex_Content (Annex, Annex_Content);
         Set_Corresponding_Annex (Annex, Annex_Root);

         return Annex;
      else
         return No_Node;
      end if;
   end P_Annex;

   ---------------------
   -- P_Annex_Library --
   ---------------------

   --  annex_library ::=
   --     annex annex_identifier
   --       ( ( {** annex_specific_reusable_constructs **} ) | none ) ;

   function P_Annex_Library
     (Namespace           : Types.Node_Id;
      Private_Declaration : Boolean := False) return Node_Id
   is
   begin
      return P_Annex (Namespace, PC_Annex_Library, Private_Declaration);
   end P_Annex_Library;

   -----------------------
   -- P_Annex_Subclause --
   -----------------------

   --  annex_subclause ::=
   --     annex annex_identifier
   --       ( ( {** annex_specific_language_constructs **} ) | none )
   --       [ in_modes ];

   function P_Annex_Subclause (Namespace : Types.Node_Id) return Node_Id is
   begin
      return P_Annex (Namespace, PC_Annex_Subclause, False);
   end P_Annex_Subclause;

   ------------------
   -- P_Annex_Path --
   ------------------

   --  AADL_V2
   --  annex_path ::=
   --     [ { annex_identifier } ] { ** model_element_identifier }+

   function P_Annex_Path (Container : Types.Node_Id) return Node_Id is

      use Lexer;
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.Builder.AADL.Annexes;

      Start_Loc        : Location;
      Loc              : Location;
      Annex_Identifier : Node_Id := No_Node;
      Elt_Identifier   : Node_Id := No_Node;
      List_Identifiers : List_Id := No_List;
      Annex_Node       : Node_Id;
   begin
      Save_Lexer (Start_Loc);

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Curly_Bracket then
         Annex_Identifier := P_Identifier (No_Node);

         Scan_Token;
         if Token /= T_Right_Curly_Bracket then
            DPE (PC_Annex_Path);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      List_Identifiers := New_List (K_List_Id, Loc);

      loop
         if Token = T_Multiply then
            Save_Lexer (Loc);
            Scan_Token;

            if Token /= T_Multiply then
               DPE (PC_Annex_Path);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            else
               Elt_Identifier := P_Identifier (No_Node);

               if No (Elt_Identifier) then
                  DPE (PC_Annex_Path, T_Identifier);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               else
                  Append_Node_To_List (Elt_Identifier, List_Identifiers);
               end if;
            end if;

         else
            exit;
         end if;
      end loop;

      if Is_Empty (List_Identifiers) then
         DPE (PC_Annex_Path, EMC_List_Is_Empty);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         Annex_Node :=
           Add_New_Annex_Path
             (Start_Loc,
              Container,
              Annex_Identifier,
              List_Identifiers);
      end if;

      if No (Annex_Node) then
         DPE (PC_Annex_Path);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      return Annex_Node;

   end P_Annex_Path;

end Ocarina.FE_AADL.Parser.Annexes;
