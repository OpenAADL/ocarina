------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                   O C A R I N A . B E _ A A D L _ B A                    --
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

with Output;

with Ocarina.Backends;

with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Debug;

with Ocarina.BE_AADL_BA.Specifications;

package body Ocarina.BE_AADL_BA is

   use Output;
   use Ocarina.Backends;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.ME_AADL_BA.BA_Tree.Debug;
   use Ocarina.BE_AADL_BA.Specifications;

   procedure Generate_Behavior_Spec (Node : Node_Id);
   --  Prints all the Behavior Specification source corresponding to
   --  the subtree having Node as a root. If given the absolute root
   --  of an Behavior specification syntactic tree, prints all the
   --  parsed Behavior specification source.

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Register_Backend
        ("behavior_specification",
         Generate_Behavior_Spec'Access,
         Behavior_PP);
   end Init;

   -------------------
   -- Print_Subtree --
   -------------------

   procedure Print_Subtree (Node : Node_Id) is

      pragma Assert (Present (Node));

   begin
      case Kind (Node) is
         when K_Behavior_Annex =>
            Print_Behavior_Annex (Node);

         when others =>
            Node_Not_Handled (Node);
            --  This case should not happen
      end case;
   end Print_Subtree;

   ----------------------------
   -- Generate_Behavior_Spec --
   ----------------------------

   procedure Generate_Behavior_Spec (Node : Node_Id) is
      pragma Assert (Present (Node));

   begin
      Print_Subtree (Node);
   end Generate_Behavior_Spec;

   --------------------------
   -- Print_None_Statement --
   --------------------------

   procedure Print_None_Statement is
   begin
      Write_Indentation;
      Print_Token (T_None);
      Print_Token (T_Semicolon);
   end Print_None_Statement;

   -----------------
   -- Print_Token --
   -----------------

   procedure Print_Token (Token : Ocarina.ME_AADL_BA.Tokens.BA_Token_Type) is
   begin
      Write_Str (Image (Token));
   end Print_Token;

   ------------------
   -- Print_Tokens --
   ------------------

   procedure Print_Tokens
     (Tokens : Ocarina.ME_AADL_BA.Tokens.BA_Token_List_Type)
   is
   begin
      for Index in Tokens'Range loop
         Print_Token (Tokens (Index));
         if Index < Tokens'Last then
            Write_Space;
         end if;
      end loop;
   end Print_Tokens;

   ----------------------
   -- Node_Not_Handled --
   ----------------------

   procedure Node_Not_Handled (Node : Node_Id) is
      pragma Assert (Node /= No_Node);
   begin
      W_Str ("*** This node is not handled by the Behavior Spec printer: ");
      W_Node_Header (Node);

      raise Program_Error;
   end Node_Not_Handled;

end Ocarina.BE_AADL_BA;
