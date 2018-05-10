------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . A S N 1 _ T R E E . N U T I L S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2010-2015 ESA & ISAE.                    --
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

with Ocarina.Backends.ASN1_Tree.Nodes;
with Ocarina.Backends.ASN1_Values;

package Ocarina.Backends.ASN1_Tree.Nutils is

   package ASN1_Nodes renames Ocarina.Backends.ASN1_Tree.Nodes;

   use Ocarina.Backends.ASN1_Values;

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

   subtype Keyword_Type is Token_Type range Tok_Null .. Tok_Module;

   type Operator_Type is
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
   (Operator_Type'Pos (Op_And) ..
        Operator_Type'Pos (Op_Vertical_Bar)) of Name_Id;

   subtype Keyword_Operator is
     Operator_Type range Operator_Type'First .. Op_Or_Else;

   function Length (L : List_Id) return Natural;

   function Is_Empty (L : List_Id) return Boolean;

   procedure Reset;

   procedure Initialize;

   function New_List
     (Kind : ASN1_Nodes.Node_Kind;
      From : Node_Id := No_Node) return List_Id;

   function Image (O : Operator_Type) return String;

   function Image (T : Token_Type) return String;

   function Copy_Node (N : Node_Id) return Node_Id;

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id);

   procedure Insert_After_Node (E : Node_Id; N : Node_Id);

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id;

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id;

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id) return Name_Id;

   function New_Node
     (Kind : ASN1_Nodes.Node_Kind;
      From : Node_Id := No_Node) return Node_Id;

   procedure New_Token (T : Token_Type; I : String := "");

   function Make_ASN1_File (Identifier : Node_Id) return Node_Id;
   --  Build a node that describes an ASN1 file. The name
   --  of the file is described in the parameter.

   function Make_Defining_Identifier (Name : Name_Id) return Node_Id;
   --  Build a node that contains an identifier.

   function Make_Enumerated_Value (Name : Name_Id) return Node_Id;
   --  Build a node that describes a member of an enumeration but
   --  does not associate any value with it.

   function Make_Enumerated_Value
     (Name : Name_Id;
      V    : Unsigned_Long_Long) return Node_Id;
   --  Make a node that describes a member of an enumeration and associated
   --  an integer value to it. The first parameter is the name of the
   --  enumeration while the second is its corresponding value.

   function Make_Enumerated return Node_Id;
   --  Make an enumeration definition list with an empty list of
   --  values.

   function Make_Enumerated (L : List_Id) return Node_Id;
   --  Make an enumerated nodes. The parameters are the values
   --  of the enumeration. The returned node_id can be associated
   --  with a type definition.

   function Make_Type_Definition
     (Name : Name_Id;
      Decl : Node_Id) return Node_Id;
   --  Make a Type_Definition node. First parameter is the name
   --  of the type, the second one is the declaration of this type
   --  Enumerated/Choice/... node.

   function Make_Literal (Value : Value_Id) return Node_Id;
   --  Make a Literal that contains a value (Value-Id), see
   --  Ocarina.Backends.ASN1_Values for more information.

   function Make_Sequence (Sequence_Members : List_Id) return Node_Id;
   --  Make a sequence that contains the members given in parameter.

   function Make_Sequence_Member
     (Member_Name : Name_Id;
      Member_Type : Node_Id) return Node_Id;
   --  Make a sequence member identified by Member_Name which has the
   --  type Member_Type.

   function Make_Choice (Choice_Members : List_Id) return Node_Id;
   --  Make a choice that contains the members given in parameter.

   function Make_Choice_Member
     (Member_Name : Name_Id;
      Member_Type : Node_Id) return Node_Id;
   --  Make a choice member identified by Member_Name which has the
   --  type Member_Type.

   function Make_Type_Designator
     (Type_Name        : Node_Id;
      Type_Constraints : Node_Id := No_Node) return Node_Id;
   --  Make a node that points to a type. The type name is either a reference
   --  to a type node, either a defining_identifier node that contains
   --  the name of the pointed type. The second parameter is optional
   --  and defines the constraints associated with the type designation.
   --  You can look at the Make_Type_Constraints function for more
   --  information.

   function Make_Type_Constraints
     (Size_Up   : Value_Id := No_Value;
      Size_Down : Value_Id := No_Value) return Node_Id;
   --  Make the Type_Constraints node. This node contains all potential
   --  constraints that a type designator can contain. So, all parameters
   --  are not mandatory and have a default value.

end Ocarina.Backends.ASN1_Tree.Nutils;
