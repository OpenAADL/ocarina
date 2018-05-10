------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . L N T . D E B U G            --
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

with Ocarina.Output; use Ocarina.Output;

with Ocarina.Backends.LNT.Nodes; use Ocarina.Backends.LNT.Nodes;

package Ocarina.Backends.LNT.Debug is

   N_Indents : Integer := -1;

   procedure W_Eol         (N : Natural := 1) renames Output.Write_Eol;
   procedure W_Int         (N : Int)          renames Output.Write_Int;
   procedure W_Line        (N : String)       renames Output.Write_Line;
   procedure W_Str         (N : String)       renames Output.Write_Str;
   procedure W_Indents;

   procedure W_Boolean     (N : Boolean);
   procedure W_Byte        (N : Byte);
   procedure W_List_Id     (L : List_Id);
   procedure W_Node_Id     (N : Node_Id);
   procedure W_Node_Header (N : Node_Id);
   procedure W_Full_Tree (N : Node_Id);

   procedure W_Node_Attribute
     (A : String;
      K : String;
      V : String;
      N : Int := 0);

   function Image (N : Node_Kind) return String;
   function Image (N : Name_Id) return String;
   function Image (N : Node_Id) return String;
   function Image (N : List_Id) return String;
   function Image (N : Mode_Id) return String;
   function Image (N : Value_Id) return String;
   function Image (N : Operator_Id) return String;
   function Image (N : Boolean) return String;
   function Image (N : Byte) return String;
   function Image (N : Int) return String;

end Ocarina.Backends.LNT.Debug;
