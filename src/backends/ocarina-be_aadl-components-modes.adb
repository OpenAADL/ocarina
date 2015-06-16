------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B E _ A A D L . C O M P O N E N T S . M O D E S      --
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

with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nodes; use Ocarina.ME_AADL.AADL_Tree.Nodes;

with Ocarina.BE_AADL.Properties;
with Ocarina.BE_AADL.Identifiers;

package body Ocarina.BE_AADL.Components.Modes is

   use Ocarina.Output;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.BE_AADL.Identifiers;

   --------------------
   -- Print_In_Modes --
   --------------------

   procedure Print_In_Modes (Node : Node_Id) is
      List_Node : Node_Id;
      Mode_List : List_Id;

   begin
      if Present (Node) then
         Increment_Indentation;
         Write_Eol;
         Write_Indentation;
         Print_Tokens ((T_In, T_Modes, T_Left_Parenthesis));

         Mode_List := Ocarina.ME_AADL.AADL_Tree.Nodes.Modes (Node);

         if Is_Empty (Mode_List) then
            Print_Token (T_None);
         else
            List_Node := First_Node (Mode_List);

            while Present (List_Node) loop
               case Kind (List_Node) is
                  when K_Entity_Reference =>
                     Print_Entity_Reference (List_Node);
                  when K_Pair_Of_Entity_References =>
                     Print_Token (T_Left_Parenthesis);
                     Print_Entity_Reference (First_Reference (List_Node));
                     Write_Space;
                     Print_Token (T_Direct_Connection);
                     Write_Space;
                     Print_Entity_Reference (Second_Reference (List_Node));
                     Print_Token (T_Right_Parenthesis);
                  when others =>
                     raise Program_Error;
               end case;

               exit when No (Next_Node (List_Node));
               Print_Token (T_Comma);
               Write_Space;

               List_Node := Next_Node (List_Node);
            end loop;

            Decrement_Indentation;
         end if;

         Print_Token (T_Right_Parenthesis);
      end if;
   end Print_In_Modes;

   ----------------
   -- Print_Mode --
   ----------------

   procedure Print_Mode (Node : Node_Id) is
      use Ocarina.BE_AADL.Properties;

   begin
      Write_Indentation;
      Print_Item_Refined_To (Node);
      Write_Space;

      if Is_Initial (Node) then
         Print_Token (T_Initial);
         Write_Space;
      end if;

      Print_Token (T_Mode);
      Print_Contained_Property_Associations
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Mode;

   ---------------------------
   -- Print_Mode_Transition --
   ---------------------------

   procedure Print_Mode_Transition (Node : Node_Id) is
      use Ocarina.BE_AADL.Properties;

      List_Node : Node_Id;

   begin
      Write_Indentation;

      if not Is_Empty (Source_Modes (Node)) then
         List_Node := First_Node (Source_Modes (Node));

         while Present (List_Node) loop
            if List_Node /= First_Node (Source_Modes (Node)) then
               Print_Token (T_Comma);
               Write_Space;
            end if;

            Print_Identifier (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Write_Space;
      Print_Token (T_Left_Step_Bracket);
      Write_Space;

      if not Is_Empty (Triggers (Node)) then
         List_Node := First_Node (Triggers (Node));

         while Present (List_Node) loop
            if List_Node /= First_Node (Triggers (Node)) then
               Print_Token (T_Comma);
               Write_Space;
            end if;

            if Ocarina.ME_AADL.AADL_Tree.Nodes.Kind (List_Node) =
              K_Entity_Reference
            then
               Print_Entity_Reference (List_Node);
            elsif Ocarina.ME_AADL.AADL_Tree.Nodes.Kind (List_Node) =
              K_Mode_Transition_Trigger
            then
               if Ocarina.ME_AADL.AADL_Tree.Nodes.Is_Self (List_Node) then
                  Print_Token (T_Self);
                  Print_Token (T_Dot);
               elsif Ocarina.ME_AADL.AADL_Tree.Nodes.Is_Processor
                   (List_Node)
               then
                  Print_Token (T_Processor);
                  Print_Token (T_Dot);
               end if;
               Print_Identifier
                 (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifier (List_Node));
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Write_Space;
      Print_Token (T_Right_Step_Bracket);
      Write_Space;

      Print_Identifier (Destination_Mode (Node));
      Print_Contained_Property_Associations
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Mode_Transition;

end Ocarina.BE_AADL.Components.Modes;
