------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . L N T . N U T I L S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

with Ocarina.Types;
with Locations;
with Ocarina.Backends.LNT.Nodes;

package Ocarina.Backends.LNT.Nutils is

   type Token_Type is
     (
      --   Keywords
      Tok_Any,             --  any **** First Keyword
      Tok_Channel,
      Tok_Do,              --  do
      Tok_Inout,           --  inout
      Tok_Of,              --  OF
      Tok_Raises,          --  raises
      Tok_Then,            --  then
      Tok_As,               --  as
      Tok_Else,            -- ELSE
      Tok_Elsif,           -- ELSIF
      Tok_End,             -- END
      Tok_Function,        -- FUNCTION
      Tok_Is,              -- IS
      Tok_Out,             -- OUT
      Tok_Return,          -- RETURN
      Tok_Select,          -- SELECT
      Tok_Type,            -- TYPE
      Tok_If,              -- IF
      Tok_Raise,           -- RAISE
      Tok_Case,            -- CASE
      Tok_For,             -- FOR
      Tok_Loop,            -- LOOP
      Tok_While,           -- WHILE
      Tok_In,              -- IN
      Tok_Null,            -- NULL
      Tok_With,            -- WITH
      Tok_Break,           -- break
      Tok_Hide,            -- hide
      Tok_List,            -- list
      Tok_Par,             -- par
      Tok_Var,
      Tok_By,
      Tok_Printedby,
      Tok_Set,
      Tok_Where,
      Tok_Eval,
      Tok_Implementedby,
      Tok_Module,
      Tok_Process,
      Tok_Sorted,
      Tok_Comparedby,
      Tok_Range,
      Tok_Array,
      Tok_External,
      Tok_Stop,
      --  eq, ==, ne, <>, /=, lt, <, le, <=, gt, >, ge, >=
      --  *** for all predefined types
      --  Graphic Characters
      Tok_Double_Asterisk, -- **
      Tok_Ampersand,       -- &
      Tok_Minus,           -- -
      Tok_Plus,            -- +
      Tok_Asterisk,        -- *
      Tok_Slash,           -- /
      Tok_Dot,             -- .
      Tok_Apostrophe,      -- '
      Tok_Left_Paren,      -- (
      Tok_Right_Paren,     -- )
      Tok_Comma,           -- ,
      Tok_Less,            -- <
      Tok_Equal,           -- =
      Tok_Greater,         -- >
      Tok_Not_Equal,       -- /=
      Tok_Greater_Equal,   -- >=
      Tok_Less_Equal,      -- <=
      Tok_Box,             -- <>
      Tok_Colon_Equal,     -- :=
      Tok_Colon,           -- :
      Tok_Greater_Greater, -- >>
      Tok_Less_Less,       -- <<
      Tok_Semicolon,       -- ;
      Tok_Arrow,           -- ->
      Tok_Arrow_Double,    -- =>
      Tok_Vertical_Bar,    -- |
      Tok_Dot_Dot,         -- ..
      Tok_Minus_Minus);    -- --

   Token_Image : array (Token_Type) of Name_Id;
   subtype Keyword_Type is Token_Type range Tok_Any .. Tok_Stop;

   package OLNT renames Ocarina.Backends.LNT.Nodes;

   function New_Node (Kind : OLNT.Node_Kind) return Types.Node_Id;
   --  Create a new node
   function New_Node (Kind : OLNT.Node_Kind; Loc : Locations.Location)
     return Node_Id;
   --  Create a new node

   procedure New_Token (T : Token_Type; I : String := "");

   --  Create a new Token and set its image to I (if given)

   function New_Identifier (Component_Name : Name_Id;
                            Prefix : String := "";
                            The_Old_Char : Character := '.';
                            The_New_Char : Character := '_')
     return Node_Id;
   function New_Identifier (Component_Name : Name_Id;
                            Prefix : String := "";
                            The_Old_Char : Character := '.';
                            The_New_Char : Character := '_')
     return Name_Id;
   function New_List (Kind : OLNT.Node_Kind) return Types.List_Id;
   function New_List (
                      N0 : Node_Id := No_Node;
                      N1 : Node_Id := No_Node;
                      N2 : Node_Id := No_Node;
                      N3 : Node_Id := No_Node;
                      N4 : Node_Id := No_Node;
                      N5 : Node_Id := No_Node;
                      N6 : Node_Id := No_Node;
                      N7 : Node_Id := No_Node;
                      N8 : Node_Id := No_Node;
                      N9 : Node_Id := No_Node;
                      N10 : Node_Id := No_Node;
                      N11 : Node_Id := No_Node)
     return Types.List_Id;

   function Make_Node_Container
     (Item       : Node_Id;
      Extra_Item : Node_Id := No_Node)
     return Node_Id;
   --  Creates a container for the nodes Item and Extra_Item to be able
   --  to put one node in several lists

   function Is_Empty (L : Types.List_Id) return Boolean;
   --  Return true if the list is empty, else return false

   procedure Append_Node_To_List (E : Types.Node_Id; L : Types.List_Id);
   --  Append the node to the list

   procedure Append_List_To_List (S : Types.List_Id;
                                  D : in out Types.List_Id);
   --  Append S to D

   procedure Push_Node_Into_List (E : Types.Node_Id; L : Types.List_Id);

   function Length (L : List_Id) return Natural;

   procedure Delete_Node_From_List (E : Types.Node_Id; L : Types.List_Id);

   function To_Lower (N : Name_Id) return Name_Id;
   procedure To_Lower (S : in out String);

   function Make_Identifier
     (Ident_Name : Types.Name_Id)
     return Types.Node_Id;

   function Make_Identifier
     (Ident : String)
     return Types.Node_Id;
   --  Create a new identifier

   function Image (T : Token_Type) return String;

   procedure Push_Entity (E : Node_Id);
   procedure Pop_Entity;
end Ocarina.Backends.LNT.Nutils;
