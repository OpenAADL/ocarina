------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . F E _ A A D L _ B A . P A R S E R             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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
--  of Behavior specification.

with Locations;
with Ocarina.Types;

with Ocarina.ME_AADL_BA.Tokens;
with Ocarina.FE_AADL_BA.Parser_Errors;

package Ocarina.FE_AADL_BA.Parser is

   use Ocarina.Types;
   use Ocarina.FE_AADL_BA.Parser_Errors;

   function Process
     (AADL_Root : Node_Id;
      From      : Locations.Location;
      To        : Locations.Location := Locations.No_Location) return Node_Id;

   procedure Init;
   --  Initialize the parser and register it to the general Ocarina
   --  parser.

   type P_Item_Function_Ptr is access function
     (Container : Node_Id) return Node_Id;

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
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Separator : Ocarina.ME_AADL_BA.Tokens.BA_Token_Type) return List_Id;
   --  Parse ( { Item Separator }* Item )

   function P_Elements_List
     (Func       : P_Item_Function_Ptr;
      Container  : Node_Id;
      Delimiters : Ocarina.ME_AADL_BA.Tokens.BA_Token_List_Type;
      Code       : Parsing_Code) return List_Id;
   --  Parse ( { Element }* Element Delimiter )

end Ocarina.FE_AADL_BA.Parser;
