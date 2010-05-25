------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . A S N 1 _ T R E E . N U T I L S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010, GET-Telecom Paris.                   --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Ocarina.Backends.ASN1_Tree.Nodes;

package Ocarina.Backends.ASN1_Tree.Nutils is

   package ASN1_Nodes renames Ocarina.Backends.ASN1_Tree.Nodes;

   type Token_Type is
     (
      --   Token name      Token type
      --   Keywords
      Tok_Null,            -- NULL   **** First Keyword
      Tok_Module,          -- MODULE

      --  Graphic Characters
      Tok_Xor,             -- ^
      Tok_Sharp,           -- #
      Tok_Mod,             -- %
      Tok_Not,             -- !
      Tok_Left_Brace,      -- {
      Tok_Right_Brace,     -- }
      Tok_Or,              -- ||
      Tok_And,             -- &&
      Tok_Ampersand,       -- &
      Tok_Minus,           -- -
      Tok_Underscore,      -- _
      Tok_Plus,            -- +
      Tok_Asterisk,        -- *
      Tok_Slash,           -- /
      Tok_Quote,           -- "
      Tok_Dot,             -- .
      Tok_Apostrophe,      -- '
      Tok_Left_Paren,      -- (
      Tok_Right_Paren,     -- )
      Tok_Left_Hook,       -- [
      Tok_Right_Hook,      -- ]
      Tok_Comma,           -- ,
      Tok_Less,            -- <
      Tok_Equal,           -- =
      Tok_Equal_Equal,     -- ==
      Tok_Greater,         -- >
      Tok_Not_Equal,       -- /=
      Tok_Greater_Equal,   -- >=
      Tok_Less_Equal,      -- <=
      Tok_Colon,           -- :
      Tok_Greater_Greater, -- >>
      Tok_Less_Less,       -- <<
      Tok_Semicolon,       -- ;
      Tok_Arrow,           -- ->
      Tok_Vertical_Bar);   -- |

   Token_Image : array (Token_Type) of Name_Id;

   subtype Keyword_Type is Token_Type
     range Tok_Null .. Tok_Module;

   type Operator_Type  is
     (Op_Not,             -- not
      Op_And,             -- and
      Op_In,              -- in
      Op_And_Then,        -- and then
      Op_Or,              -- or
      Op_Or_Else,         -- or else
      Op_And_Symbol,      -- &
      Op_Double_Asterisk, -- **
      Op_Minus,           -- -
      Op_Plus,            -- +
      Op_Asterisk,        -- *
      Op_Slash,           -- /
      Op_Less,            -- <
      Op_Equal,           -- =
      Op_Equal_Equal,     -- ==
      Op_Greater,         -- >
      Op_Not_Equal,       -- !=
      Op_Greater_Equal,   -- >=
      Op_Less_Equal,      -- <=
      Op_Greater_Greater, -- >>
      Op_Less_Less,       -- <<
      Op_Semicolon,       -- ;
      Op_Arrow,           -- ->
      Op_Vertical_Bar,    -- |
      Op_None);           -- No operation

   Operator_Image : array
     (Operator_Type'Pos (Op_And) ..  Operator_Type'Pos (Op_Vertical_Bar))
     of Name_Id;

   subtype Keyword_Operator is Operator_Type
     range Operator_Type'First .. Op_Or_Else;

   function Length (L : List_Id) return Natural;

   function Is_Empty (L : List_Id) return Boolean;

   procedure Reset;

   procedure Initialize;

   function New_List (Kind : ASN1_Nodes.Node_Kind; From : Node_Id := No_Node)
     return List_Id;

   function Image (O : Operator_Type) return String;

   function Image (T : Token_Type) return String;

   function Copy_Node (N : Node_Id) return Node_Id;

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id);

   procedure Insert_After_Node (E : Node_Id; N : Node_Id);

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);

   function Remove_Suffix_From_Name
     (Suffix : String; Name   : Name_Id) return Name_Id;

   function Add_Suffix_To_Name
     (Suffix : String; Name   : Name_Id) return Name_Id;

   function Add_Prefix_To_Name
     (Prefix : String; Name   : Name_Id) return Name_Id;

   function New_Node
     (Kind : ASN1_Nodes.Node_Kind;
      From : Node_Id := No_Node)
     return Node_Id;

   procedure New_Token (T : Token_Type; I : String := "");

   function Make_ASN1_File (Identifier : Node_Id) return Node_Id;

   function Make_Defining_Identifier (Name : Name_Id) return Node_Id;

   function Make_Enumerated_Value (Name : Name_Id) return Node_Id;

   function Make_Enumerated_Value (Name : Name_Id; V : Unsigned_Long_Long)
      return Node_Id;

   function Make_Enumerated return Node_Id;

   function Make_Enumerated (L : List_Id) return Node_Id;

   function Make_Type_Definition (Name : Name_Id; Decl : Node_Id)
     return Node_Id;

   function Make_Literal (Value : Value_Id) return Node_Id;

end Ocarina.Backends.ASN1_Tree.Nutils;
