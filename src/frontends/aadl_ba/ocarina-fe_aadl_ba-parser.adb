------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . F E _ A A D L _ B A . P A R S E R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2018 ESA & ISAE.        --
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

with Ocarina.Output;

with GNAT.OS_Lib;

with Ocarina.Parser;

with Ocarina.FE_AADL_BA.Lexer;
with Ocarina.FE_AADL_BA.Parser.Specifications;

with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

package body Ocarina.FE_AADL_BA.Parser is

   use Ocarina.Output;
   use Locations;
   use GNAT.OS_Lib;

   use Ocarina.FE_AADL_BA.Lexer;
   use Ocarina.FE_AADL_BA.Parser.Specifications;
   use Ocarina.ME_AADL_BA.Tokens;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   Language : constant String := "behavior_specification";

   procedure Exit_On_Error (Error : Boolean; Reason : String);
   pragma Unreferenced (Exit_On_Error);

   -------------------
   -- Exit_On_Error --
   -------------------

   procedure Exit_On_Error (Error : Boolean; Reason : String) is
   begin
      if Error then
         Set_Standard_Error;
         Write_Line (Reason);
         OS_Exit (1);
      end if;
   end Exit_On_Error;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Ocarina.Parser.Register_Parser (Language, Process'Access);
   end Init;

   ------------------
   -- P_Items_List --
   ------------------

   --  ( { Item }+ | none_statement )

   function P_Items_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Code      : Parsing_Code)
     return Integer
   is
      Loc   : Location;
      Nb_Items : Integer := 0;
      Item  : Node_Id;

   begin
      loop
         Save_Lexer (Loc);
         Item := Func.all (Container);
         if Present (Item) then
            Nb_Items := Nb_Items + 1;
         else
            --  Error when parsing item, restore lexer

            Restore_Lexer (Loc);
            if Nb_Items = 0 then
               --  list must contain at least one element, {Item}+

               Nb_Items := -1;
               DPE (Code, EMC_List_Is_Empty);
            end if;
            exit;
         end if;
      end loop;

      return Nb_Items;
   end P_Items_List;

   ------------------
   -- P_Items_List --
   ------------------

   function P_Items_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Code      : Parsing_Code)
     return List_Id
   is
      Loc   : Location;
      Items : List_Id;
      Item  : Node_Id;

   begin
      Items := New_List (K_List_Id, Token_Location);
      loop
         Save_Lexer (Loc);
         Item := Func.all (Container);
         if Present (Item) then
            Append_Node_To_List (Item, Items);
         else
            --  Error when parsing item, restore lexer
            Restore_Lexer (Loc);
            if Is_Empty (Items) then
               --  list must contain at least one element, { Item }+
               DPE (Code, EMC_List_Is_Empty);
            end if;
            exit;
         end if;
      end loop;

      return Items;
   end P_Items_List;

   ------------------
   -- P_Items_List --
   ------------------

   --  ( { Item Separator }* Item )

   function P_Items_List
     (Func         : P_Item_Function_Ptr;
      Container    : Node_Id;
      Separator    : Ocarina.ME_AADL_BA.Tokens.BA_Token_Type)
     return List_Id
   is
      Item  : Node_Id;
      Items : List_Id;
      Loc   : Location;

   begin
      Items := New_List (K_List_Id, Token_Location);
      loop
         Item := Func.all (Container);
         if Present (Item) then
            Append_Node_To_List (Item, Items);
         else
            return No_List;
         end if;

         Save_Lexer (Loc);
         Scan_Token;
         if Token /= Separator then
            Restore_Lexer (Loc);
            exit;
         end if;
      end loop;

      Set_Loc (Node_Id (Items),
               Ocarina.ME_AADL_BA.BA_Tree.Nodes.Loc (First_Node (Items)));
      return Items;
   end P_Items_List;

   --------------------
   -- P_Element_List --
   --------------------

   --  parse ( { Element }* Element Delimiter )

   function P_Elements_List
     (Func         : P_Item_Function_Ptr;
      Container    : Node_Id;
      Delimiters   : Ocarina.ME_AADL_BA.Tokens.BA_Token_List_Type;
      Code         : Parsing_Code)
     return List_Id
   is
      Element       : Node_Id;
      Elements_List : List_Id;
      Loc           : Location;

      Escape        : Boolean := False;
   begin
      Elements_List := New_List (K_List_Id, Token_Location);
      loop
         Element := Func.all (Container);
         if Present (Element) then
            Append_Node_To_List (Element, Elements_List);
         else
            DPE (Code, EMC_List_Is_Empty);
            Skip_Tokens (Delimiters);
            return No_List;
         end if;

         Save_Lexer (Loc);
         Scan_Token;

         for Index in Delimiters'Range loop
            if Token =  (Delimiters (Index)) then
               Restore_Lexer (Loc);
               Escape := True;
               exit;
            end if;
         end loop;

         if Escape then
            exit;
         else
            Restore_Lexer (Loc);
         end if;
      end loop;

      Set_Loc (Node_Id (Elements_List),
               Ocarina.ME_AADL_BA.BA_Tree.Nodes.Loc (First_Node
                                                       (Elements_List)));
      return Elements_List;
   end P_Elements_List;

   -------------
   -- Process --
   -------------

   function Process
     (AADL_Root : Node_Id;
      From      : Location;
      To        : Location := No_Location;
      Container : Node_Id  := No_Node)
     return Node_Id
   is
      pragma Unreferenced (Container);
      Buffer        : Location;
   begin

      Buffer := From;
      if To /= No_Location then
         Buffer.EOF := To.Last_Pos;
      end if;
      Restore_Lexer (Buffer);

      return P_Behavior_Specification (AADL_Root);
   end Process;

end Ocarina.FE_AADL_BA.Parser;
