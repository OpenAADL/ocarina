------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . B E _ A A D L                       --
--                                                                          --
--                                 S p e c                                  --
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

--  This package gathers functions that regenerate AADL specifications
--  from an abtract syntax tree

with Ocarina.Types;          use Ocarina.Types;
with Ocarina.ME_AADL.Tokens; use Ocarina.ME_AADL.Tokens;

package Ocarina.BE_AADL is

   procedure Init;
   --  Set some variables that are used by all the subpackages of the
   --  printer.

   generic
      with function Is_Printable
        (Node      : Node_Id;
         Criterion : Node_Id) return Boolean;
      --  This is a general purpose comparison routine. The Criterion
      --  may not be necessary in some cases.

   procedure Print_Constrained_Subtree
     (Node      : Node_Id;
      Criterion : Node_Id := No_Node);
   --  Print the AADL declarations for which the given Is_Printable
   --  (<Node>, Criterion) is True.

   procedure Generate_AADL_Model
     (Node       : Node_Id;
      Whole_Tree : Boolean := True);
   --  Prints all the AADL source corresponding to the subtree having
   --  Node as a root. If given the absolute root of an AADL syntactic
   --  tree, prints all the parsed AADL source. If the Whole_Tree is
   --  True, disregards Node and prints the whole declarative tree.

private

   No_Labels : constant Token_List_Type := (1 => T_Error);
   No_Token  : constant Token_Type      := T_Error;

   procedure Print_Tokens (Tokens : Token_List_Type);
   pragma Inline (Print_Tokens);
   --  Print tokens list with one space between each

   procedure Print_Token (Token : Token_Type);
   pragma Inline (Print_Token);

   procedure Print_None_Statement;
   pragma Inline (Print_None_Statement);
   --  Print 'none;'

   procedure Print_Item_Refined_To (Node : Node_Id);

   procedure Node_Not_Handled (Node : Node_Id);

   function Always_Printable
     (Node      : Node_Id;
      Criterion : Node_Id) return Boolean;
   --  This function always returns True. Used to create some
   --  particular case instantiations of generic procedures.

end Ocarina.BE_AADL;
