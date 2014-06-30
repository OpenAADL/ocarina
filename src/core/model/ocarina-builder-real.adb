------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 O C A R I N A . B U I L D E R . R E A L                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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

with Namet;

with Ocarina.ME_REAL.REAL_Tree.Nutils;
with Ocarina.ME_REAL.REAL_Tree.Utils;
with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Ocarina.ME_REAL.Tokens;

with Ocarina.REAL_Values;

package body Ocarina.Builder.REAL is

   use Namet;
   use Ocarina.ME_REAL.REAL_Tree.Nutils;
   use Ocarina.ME_REAL.REAL_Tree.Utils;
   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Ocarina.ME_REAL.Tokens;

   ------------------
   -- Make_Element --
   ------------------

   function Make_Element (Name : Name_Id; T : Value_Id) return Node_Id is
      N, Id : Node_Id;
   begin
      Id := New_Node (K_Identifier, Token_Location);
      Set_Name (Id, Name);

      N := New_Node (K_Element, Token_Location);
      Set_Identifier (N, Id);
      Set_Element_Type (N, T);
      Set_Set_Reference (N, No_Node);

      return N;
   end Make_Element;

   -------------------
   -- Make_Variable --
   -------------------

   function Make_Variable (Name : Name_Id) return Node_Id is
      use Ocarina.REAL_Values;

      N, Id : Node_Id;
   begin
      Id := New_Node (K_Identifier, Token_Location);
      Set_Name (Id, Name);

      N := New_Node (K_Variable, Token_Location);
      Set_Identifier (N, Id);
      Set_Var_Value (N, No_Value);
      Set_Var_Type (N, No_Value);

      return N;
   end Make_Variable;

   --------------
   -- Make_Set --
   --------------

   function Make_Set (Name : Name_Id; T : Value_Id) return Node_Id is
      N, Id : Node_Id;
   begin
      Id := New_Node (K_Identifier, Token_Location);
      Set_Name (Id, Name);

      N := New_Node (K_Set, Token_Location);
      Set_Identifier (N, Id);
      Set_Set_Type (N, T);
      Set_Annotation (N, No_Node);
      Set_Set_Expression (N, No_Node);
      Set_Predefined_Type (N, SV_No_Type);

      return N;
   end Make_Set;

   ------------------------
   -- Make_Set_Reference --
   ------------------------

   function Make_Set_Reference return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Set_Reference, Token_Location);
      Set_Referenced_Set (N, No_Node);

      return N;
   end Make_Set_Reference;

   ------------------------
   -- Make_Var_Reference --
   ------------------------

   function Make_Var_Reference (Name : Name_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Var_Reference, Token_Location);
      Set_Referenced_Var (N, No_Node);
      Set_Name (N, Name);

      return N;
   end Make_Var_Reference;

   ------------------------
   -- Make_Anonymous_Set --
   ------------------------

   function Make_Anonymous_Set (Set_Type : Value_Id) return Node_Id is
      N : Node_Id;
   begin
      Set_Str_To_Name_Buffer (Image (T_Anonymous_Set));
      N := Make_Set (Name_Find, Set_Type);

      return N;
   end Make_Anonymous_Set;

end Ocarina.Builder.REAL;
