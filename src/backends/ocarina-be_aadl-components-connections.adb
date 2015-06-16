------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BE_AADL.COMPONENTS.CONNECTIONS                   --
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
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.BE_AADL.Components.Modes;
with Ocarina.BE_AADL.Identifiers;
with Ocarina.BE_AADL.Properties;
with Ocarina.ME_AADL.AADL_Tree.Entities;

package body Ocarina.BE_AADL.Components.Connections is

   use Ocarina.Output;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.BE_AADL.Components.Modes;
   use Ocarina.BE_AADL.Identifiers;
   use Ocarina.BE_AADL.Properties;
   use Ocarina.ME_AADL.AADL_Tree.Entities;

   ----------------------
   -- Print_Connection --
   ----------------------

   procedure Print_Connection (Node : Node_Id) is
      Ident            : constant Node_Id         := Identifier (Node);
      Is_Refined       : constant Boolean         := Is_Refinement (Node);
      Connection_Modes : constant Node_Id         := In_Modes (Node);
      Cat : constant Connection_Type := Get_Category_Of_Connection (Node);
   begin
      Write_Indentation;

      if Present (Ident) then
         Print_Identifier (Ident);
         Write_Space;
         Print_Token (T_Colon);
         Write_Space;
      end if;

      if Is_Refined then
         Print_Tokens ((T_Refined, T_To));
         Write_Space;
      end if;

      case Cat is
         when CT_Port_Connection =>
            Print_Token (T_Port);
         when CT_Data | CT_Data_Delayed =>
            case AADL_Version is
               when AADL_V1 =>
                  Print_Tokens ((T_Data, T_Port));
               when AADL_V2 =>
                  Print_Token (T_Port);
            end case;
         when CT_Event =>
            Print_Tokens ((T_Event, T_Port));
         when CT_Event_Data =>
            Print_Tokens ((T_Event, T_Data, T_Port));
         when CT_Feature =>
            Print_Token (T_Feature);
         when CT_Feature_Group =>
            case AADL_Version is
               when AADL_V1 =>
                  Print_Tokens ((T_Port, T_Group));
               when AADL_V2 =>
                  Print_Tokens ((T_Feature, T_Group));
            end case;
         when CT_Parameter =>
            Print_Token (T_Parameter);
         when CT_Access_Bus =>
            Print_Tokens ((T_Bus, T_Access));
         when CT_Access_Data =>
            Print_Tokens ((T_Data, T_Access));
         when CT_Access_Subprogram =>
            Print_Tokens ((T_Subprogram, T_Access));
         when CT_Access_Subprogram_Group =>
            Print_Tokens ((T_Subprogram, T_Group, T_Access));
         when CT_Access_Virtual_Bus =>
            Print_Tokens ((T_Virtual, T_Bus, T_Access));
         when CT_Access =>
            Print_Token (T_Access);
         when CT_Error =>
            raise Program_Error;
      end case;

      if not Is_Refined then
         Write_Space;
         Print_Entity_Reference (Source (Node));

         Write_Space;
         case AADL_Version is
            when AADL_V1 =>
               if Cat = CT_Data_Delayed then
                  Print_Token (T_Delayed_Connection);
               else
                  Print_Token (T_Direct_Connection);
               end if;
            when AADL_V2 =>
               if Is_Bidirectional (Node) then
                  Print_Token (T_Bidirect_Connection);
               else
                  Print_Token (T_Direct_Connection);
               end if;
         end case;

         Write_Space;
         Print_Entity_Reference (Destination (Node));
      end if;

      Print_Contained_Property_Associations
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));
      Print_In_Modes (Connection_Modes);
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Connection;

end Ocarina.BE_AADL.Components.Connections;
