------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . B E _ A A D L                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

--  This package gathers functions that regenerate AADL specifications
--  from an abtract syntax tree

with Types;                  use Types;
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
