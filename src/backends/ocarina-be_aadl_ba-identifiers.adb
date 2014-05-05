------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B E _ A A D L _ B A . I D E N T I F I E R S        --
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

with Ocarina.Namet;
with Ocarina.Output;

with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;
with Ocarina.BE_AADL_BA.Expressions;

package body Ocarina.BE_AADL_BA.Identifiers is

   use Ocarina.Namet;
   use Ocarina.Output;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.BE_AADL_BA.Expressions;

   ----------------------
   -- Print_Identifier --
   ----------------------

   procedure Print_Identifier (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Identifier);
   begin
      Write_Name (Display_Name (Node));
   end Print_Identifier;

   ---------------------------------
   -- Print_Identifier_With_Value --
   ---------------------------------

   procedure Print_Identifier_With_Value (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Identifier
                       or else Kind (Node) = K_Identifier_With_Value);
   begin
      Write_Name (Display_Name (Node));

      if Kind (Node) = K_Identifier_With_Value then
         Print_Token (T_Left_Parenthesis);

         if Present (Value_Constant (Node)) then
            case Kind (Value_Constant (Node)) is
               when K_Literal           => Print_Literal
                                                      (Value_Constant (Node));
               when K_Property_Constant => Print_Property_Constant
                                                      (Value_Constant (Node));
               when K_Identifier        => Print_Identifier
                                                      (Value_Constant (Node));
               when others              => Write_Line (Bug_Str);
            end case;

         elsif Is_Others (Node) then
            Print_Token (T_Others);
         end if;

         Print_Token (T_Right_Parenthesis);
      end if;
   end Print_Identifier_With_Value;

   ------------------------------------
   -- Print_Component_Classifier_Ref --
   ------------------------------------

   procedure Print_Component_Classifier_Ref (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Component_Classifier_Ref);

      List_Node : Node_Id;
   begin
      if not Is_Empty (Package_Name (Node)) then
         List_Node := First_Node (Package_Name (Node));

         while Present (List_Node) loop
            Print_Identifier (List_Node);
            Print_Token (T_Colon_Colon);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if Present (Component_Type (Node)) then
         Print_Identifier (Component_Type (Node));
      end if;

      if Present (Component_Impl (Node)) then
         Print_Token (T_Dot);
         Print_Identifier (Component_Impl (Node));
      end if;

   end Print_Component_Classifier_Ref;

end Ocarina.BE_AADL_BA.Identifiers;
