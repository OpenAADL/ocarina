------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B E _ A A D L . I D E N T I F I E R S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.BE_AADL.Identifiers is

   use Namet;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;

   ----------------------------
   -- Print_Entity_Reference --
   ----------------------------

   procedure Print_Entity_Reference (Node : Node_Id) is
      pragma Assert
        (Kind (Node) = K_Entity_Reference
         or else Kind (Node) = K_Unique_Property_Type_Identifier
         or else Kind (Node) = K_Property_Term
         or else Kind (Node) = K_Reference_Term
         or else Kind (Node) = K_Enumeration_Term
         or else Kind (Node) = K_Component_Classifier_Term
         or else Kind (Node) = K_Unique_Property_Const_Identifier);

   begin
      if Kind (Node) = K_Unique_Property_Type_Identifier
        or else Kind (Node) = K_Property_Term
        or else Kind (Node) = K_Enumeration_Term
        or else Kind (Node) = K_Unique_Property_Const_Identifier
      then
         if Property_Set_Identifier (Node) /= No_Node then
            Print_Identifier (Property_Set_Identifier (Node));
            Print_Token (T_Colon_Colon);
         end if;
      elsif Namespace_Identifier (Node) /= No_Node then
         Print_Identifier (Namespace_Identifier (Node));
         Print_Token (T_Colon_Colon);
      end if;

      Print_Identifier (Identifier (Node));
   end Print_Entity_Reference;

   ----------------------
   -- Print_Identifier --
   ----------------------

   procedure Print_Identifier (Node : Node_Id) is
   begin
      Write_Name (Display_Name (Node));
   end Print_Identifier;

end Ocarina.BE_AADL.Identifiers;
