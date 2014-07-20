------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BE_AADL.COMPONENTS.SUBPROGRAM_CALLS                --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Output;

with Ocarina.ME_AADL.AADL_Tree.Nodes; use Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

with Ocarina.BE_AADL.Components.Modes;
with Ocarina.BE_AADL.Properties;
with Ocarina.BE_AADL.Identifiers;

package body Ocarina.BE_AADL.Components.Subprogram_Calls is

   use Ocarina.Output;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.BE_AADL.Properties;
   use Ocarina.BE_AADL.Components.Modes;
   use Ocarina.BE_AADL.Identifiers;

   ---------------------------
   -- Print_Subprogram_Call --
   ---------------------------

   procedure Print_Subprogram_Call (Node : Node_Id) is
   begin
      Print_Identifier (Identifier (Node));
      Write_Space;
      Print_Tokens ((T_Colon, T_Subprogram));
      Write_Space;
      Print_Entity_Reference (Entity_Ref (Node));
      Print_Contained_Property_Associations
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));
      Print_Token (T_Semicolon);
   end Print_Subprogram_Call;

   ------------------------------------
   -- Print_Subprogram_Call_Sequence --
   ------------------------------------

   procedure Print_Subprogram_Call_Sequence (Node : Node_Id) is
      Ident : constant Node_Id := Identifier (Node);
      Calls : constant List_Id :=
        Ocarina.ME_AADL.AADL_Tree.Nodes.Subprogram_Calls (Node);
      List_Node : Node_Id;

   begin
      if Present (Ident) then
         Write_Indentation;
         Print_Identifier (Ident);
         Write_Space;
         Print_Token (T_Colon);
         Write_Eol;
      end if;

      if not Is_Empty (Calls) then
         List_Node := First_Node (Calls);

         while Present (List_Node) loop
            Write_Indentation;

            if List_Node = First_Node (Calls) then
               Print_Token (T_Left_Curly_Bracket);
            else
               Write_Space;
            end if;

            Print_Subprogram_Call (List_Node);

            if List_Node = Last_Node (Calls) then
               Print_Token (T_Right_Curly_Bracket);
            end if;

            Write_Eol;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Print_Contained_Property_Associations
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));
      Print_In_Modes (In_Modes (Node));
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Subprogram_Call_Sequence;

end Ocarina.BE_AADL.Components.Subprogram_Calls;
