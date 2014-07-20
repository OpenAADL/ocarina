------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.ME_AADL.AADL_INSTANCES.DEBUG                    --
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

with Ocarina.Output; use Ocarina.Output;
with Ocarina.Types;  use Ocarina.Types;

with Ocarina.ME_AADL.AADL_Instances.Nodes;
use Ocarina.ME_AADL.AADL_Instances.Nodes;

package Ocarina.ME_AADL.AADL_Instances.Debug is

   N_Indents : Integer := -1;

   procedure W_Eol (N : Natural := 1) renames Ocarina.Output.Write_Eol;
   procedure W_Int (N : Int) renames Ocarina.Output.Write_Int;
   procedure W_Line (N : String) renames Ocarina.Output.Write_Line;
   procedure W_Str (N : String) renames Ocarina.Output.Write_Str;
   procedure W_Indents;

   procedure W_Boolean (N : Boolean);
   procedure W_Byte (N : Byte);
   procedure W_List_Id (L : List_Id);
   procedure W_Node_Id (N : Node_Id);
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
   function Image (N : Value_Id) return String;
   function Image (N : Boolean) return String;
   function Image (N : Byte) return String;
   function Image (N : Int) return String;

   procedure wni (N : Node_Id);
   pragma Export (C, wni, "wni_instances");

end Ocarina.ME_AADL.AADL_Instances.Debug;
