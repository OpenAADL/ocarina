------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B E _ A A D L . C O M P O N E N T S . F L O W S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Output;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

with Ocarina.BE_AADL.Components.Modes;
with Ocarina.BE_AADL.Properties;
with Ocarina.BE_AADL.Identifiers;

package body Ocarina.BE_AADL.Components.Flows is

   use Ocarina.Output;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.BE_AADL.Components.Modes;
   use Ocarina.BE_AADL.Identifiers;
   use Ocarina.ME_AADL;

   procedure Print_Flow_Category (Category : Byte);

   -------------------------
   -- Print_Flow_Category --
   -------------------------

   procedure Print_Flow_Category (Category : Byte) is
   begin
      case Flow_Category'Val (Category) is
         when FC_Source =>
            Print_Token (T_Source);
         when FC_Sink =>
            Print_Token (T_Sink);
         when FC_Path =>
            Print_Token (T_Path);
      end case;
   end Print_Flow_Category;

   -------------------------------
   -- Print_Flow_Implementation --
   -------------------------------

   procedure Print_Flow_Implementation (Node : Node_Id) is
      use Ocarina.BE_AADL.Properties;

      Flow_Modes    : constant Node_Id   := In_Modes (Node);
      Flow_Kind     : constant Node_Kind := Kind (Node);
      Is_End_To_End : Boolean;
      Is_Refinement : Boolean;
      Cat           : Byte;
      List_Node     : Node_Id;

   begin
      if Flow_Kind = K_Flow_Implementation_Refinement
        or else Flow_Kind = K_End_To_End_Flow_Refinement
      then
         Is_Refinement := True;
      else
         Is_Refinement := False;
      end if;

      if Flow_Kind = K_End_To_End_Flow_Spec
        or else Flow_Kind = K_End_To_End_Flow_Refinement
      then
         Is_End_To_End := True;
      else
         Is_End_To_End := False;
         Cat           := Category (Node);
      end if;

      Write_Indentation;
      Print_Identifier (Identifier (Node));
      Write_Space;

      Print_Token (T_Colon);
      Write_Space;

      if Is_Refinement then
         Print_Tokens ((T_Refined, T_To));
         Write_Space;
      end if;

      if Is_End_To_End then
         Print_Tokens ((T_End, T_To, T_End, T_Flow));
      else
         Print_Token (T_Flow);
         Write_Space;
         Print_Flow_Category (Cat);
      end if;

      if not Is_Refinement then
         Write_Space;

         if Flow_Kind = K_End_To_End_Flow_Spec
           or else Flow_Category'Val (Cat) = FC_Path
         then
            Print_Entity_Reference (Source_Flow (Node));
         elsif Flow_Category'Val (Cat) = FC_Sink then
            Print_Entity_Reference (Sink_Flow (Node));
         end if;

         if not Is_Empty (Connections (Node)) then
            List_Node := First_Node (Connections (Node));

            while Present (List_Node) loop
               if List_Node /= First_Node (Connections (Node))
                 or else Flow_Kind = K_End_To_End_Flow_Spec
                 or else Flow_Category'Val (Cat) /= FC_Source
               then
                  Write_Space;
                  Print_Token (T_Direct_Connection);
                  Write_Space;
               end if;

               Print_Entity_Reference (List_Node);
               List_Node := Next_Node (List_Node);
            end loop;
         end if;

         if Flow_Kind = K_End_To_End_Flow_Spec
           or else Flow_Category'Val (Cat) = FC_Path
         then
            Write_Space;
            Print_Token (T_Direct_Connection);
            Write_Space;
            Print_Entity_Reference (Sink_Flow (Node));
         elsif Flow_Category'Val (Cat) = FC_Source then
            if not Is_Empty (Connections (Node)) then
               Write_Space;
               Print_Token (T_Direct_Connection);
            end if;

            Write_Space;
            Print_Entity_Reference (Source_Flow (Node));
         end if;

         Write_Eol;
      end if;

      Print_Contained_Property_Associations
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));
      Print_In_Modes (Flow_Modes);
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Flow_Implementation;

   ---------------------
   -- Print_Flow_Spec --
   ---------------------

   procedure Print_Flow_Spec (Node : Node_Id) is
      use Ocarina.BE_AADL.Properties;

      Cat        : constant Byte    := Category (Node);
      Flow_Modes : constant Node_Id := In_Modes (Node);

   begin
      Write_Indentation;
      Print_Item_Refined_To (Node);
      Write_Space;

      Print_Token (T_Flow);
      Write_Space;

      Print_Flow_Category (Cat);
      Write_Space;

      if not Is_Refinement (Node) then
         case Flow_Category'Val (Cat) is
            when FC_Source =>
               Print_Entity_Reference (Source_Flow (Node));
            when FC_Sink =>
               Print_Entity_Reference (Sink_Flow (Node));
            when FC_Path =>
               Print_Entity_Reference (Source_Flow (Node));
               Write_Space;
               Print_Token (T_Direct_Connection);
               Write_Space;
               Print_Entity_Reference (Sink_Flow (Node));
         end case;
      end if;

      Print_Contained_Property_Associations
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));
      Print_In_Modes (Flow_Modes);
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Flow_Spec;

end Ocarina.BE_AADL.Components.Flows;
