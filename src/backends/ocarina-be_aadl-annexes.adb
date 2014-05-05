------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              O C A R I N A . B E _ A A D L . A N N E X E S               --
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

with Ocarina.Namet;
with Ocarina.Output;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.BE_AADL.Identifiers;
with Ocarina.BE_AADL.Components.Modes;

with Ocarina.Backends;

package body Ocarina.BE_AADL.Annexes is

   use Ocarina.Namet;
   use Ocarina.Output;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.BE_AADL.Identifiers;
   use Ocarina.BE_AADL.Components.Modes;
   use Ocarina.Backends;

   procedure Print_Annex_Content (Node : Node_Id);

   -------------------------
   -- Print_Annex_Content --
   -------------------------

   procedure Print_Annex_Content (Node : Node_Id) is
   begin
      Write_Name (Raw_Text (Node));
   end Print_Annex_Content;

   ---------------------------
   -- Print_Annex_Subclause --
   ---------------------------

   procedure Print_Annex_Subclause (Node : Node_Id) is
      Content : constant Node_Id := Annex_Content (Node);
      In_Mode : constant Node_Id := In_Modes (Node);

   begin
      Write_Indentation;
      Print_Token (T_Annex);
      Write_Space;

      Print_Identifier (Identifier (Node));
      Write_Space;

      case AADL_Version is
         when AADL_V2 =>
            if Present (Corresponding_Annex (Node)) then
               Print_Token (T_Begin_Annex);
               Generate_Code (Corresponding_Annex (Node),
                              Name (Identifier (Node)));
               Write_Indentation;
               Print_Token (T_End_Annex);

            elsif Raw_Text (Content) = No_Name then
               Print_Token (T_None);
            else
               Print_Token (T_Begin_Annex);
               Print_Annex_Content (Content);
               Write_Indentation;
               Write_Indentation (-1);
               Print_Token (T_End_Annex);
            end if;

            if Present (In_Mode) then
               Print_In_Modes (In_Mode);
            end if;

         when AADL_V1 =>
            Print_Token (T_Begin_Annex);

            if Present (Content) then
               Print_Annex_Content (Content);
            else
               Write_Eol;
            end if;

            Write_Indentation;
            Write_Indentation (-1);
            Print_Token (T_End_Annex);
      end case;

      Print_Token (T_Semicolon);
   end Print_Annex_Subclause;

   ----------------------
   -- Print_Annex_Path --
   ----------------------

   procedure Print_Annex_Path (Node : Node_Id) is

      pragma Assert (Ocarina.ME_AADL.AADL_Tree.Nodes.Kind
                     (Node) = K_Annex_Path);

      List_Node : Node_Id;
   begin

      if Present (Identifier (Node)) then
         Print_Token (T_Left_Curly_Bracket);
         Write_Space;
         Print_Identifier (Identifier (Node));
         Write_Space;
         Print_Token (T_Right_Curly_Bracket);
      end if;

      List_Node :=
        First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers (Node));
      Print_Token (T_Multiply);
      Print_Token (T_Multiply);
      Print_Identifier (Node);

      while Present (List_Node) loop
         if List_Node /= First_Node
           (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers (Node)) then
            Write_Space;
            Print_Token (T_Multiply);
            Print_Token (T_Multiply);
         end if;

         Print_Identifier (List_Node);

         List_Node := Next_Node (List_Node);
      end loop;

   end Print_Annex_Path;

end Ocarina.BE_AADL.Annexes;
