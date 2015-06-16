------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . F E _ A A D L . P A R S E R                --
--                                                                          --
--                                 S p e c                                  --
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

--  This package gathers the functions that parse the global elements
--  of an AADL description, and also common parsing functions
--  (e.g. identifier parsing).

with Locations;
with Ocarina.Types; use Ocarina.Types;

with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Parser_Errors; use Ocarina.FE_AADL.Parser_Errors;

package Ocarina.FE_AADL.Parser is

   --  Default configuration parameters for the parser

   First_Parsing     : Boolean := True;
   Add_Pre_Prop_Sets : Boolean := False;

   function Process
     (AADL_Root : Node_Id;
      From      : Locations.Location;
      To        : Locations.Location := Locations.No_Location) return Node_Id;
   --  Parse the file described by buffer locations and return the
   --  Node_Id or the root of the resulting AADL tree, or No_Node if
   --  the parsing failed. If AADL_Root is not No_Node, then return
   --  itself, as it is the tree root. Start parsing from location
   --  From. If To is not No_Location, continue until end of file.

   procedure Init;
   --  Initialize the parser and register it to the general Ocarina
   --  parser.

   function Process_Predefined_Property_Sets
     (AADL_Root : Node_Id) return Node_Id;

   type P_Item_Function_Ptr is access function
     (Container : Node_Id) return Node_Id;

   type P_Refinable_Item_Function_Ptr is access function
     (Container : Node_Id;
      Refinable : Boolean) return Node_Id;
   --  Pointer to a function which parses an item

private

   function P_Items_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Code      : Parsing_Code) return Integer;

   function P_Items_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Code      : Parsing_Code) return List_Id;
   --  Parse list items of syntax: ( { Item }+ | none_statement )

   function P_Items_List
     (Func         : P_Refinable_Item_Function_Ptr;
      Container    : Node_Id;
      Refinable    : Boolean;
      Code         : Parsing_Code;
      At_Least_One : Boolean := True) return Integer;

   function P_Items_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Separator : Ocarina.ME_AADL.Tokens.Token_Type) return List_Id;
   --  Parse ( { Item Separator }* Item )

   function P_Items_List
     (Func            : P_Item_Function_Ptr;
      Container       : Node_Id;
      Separator       : Ocarina.ME_AADL.Tokens.Token_Type;
      Delimiter       : Ocarina.ME_AADL.Tokens.Token_Type;
      Code            : Parsing_Code;
      With_Terminator : Boolean := False) return List_Id;
   --  ( [ { Item Separator }* Item Delimiter ] )
   --    or ( [ { Item Separator }* Item Separator Delimiter ] )

   --  Func is a pointer which is used to parse (Item)
   --  Code indicates the object that we are parsing

   function P_Elements_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Delimiter : Ocarina.ME_AADL.Tokens.Token_Type) return List_Id;
   --  parse ( { Element }* Element Delimiter )

   function P_None_Statement return Boolean;
   --  Parse ( none ; ), this function does NOT return a Node_Id because
   --  no useful data will be retrieved
   --  Return TRUE if the reserved word 'none' is followed by a ';'

end Ocarina.FE_AADL.Parser;
