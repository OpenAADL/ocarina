------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B E _ A A D L . C O M P O N E N T S            --
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
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.BE_AADL.Components.Features;
with Ocarina.BE_AADL.Components.Subcomponents;
with Ocarina.BE_AADL.Components.Connections;
with Ocarina.BE_AADL.Components.Flows;
with Ocarina.BE_AADL.Components.Modes;
with Ocarina.BE_AADL.Components.Subprogram_Calls;
with Ocarina.BE_AADL.Components.Prototypes;
with Ocarina.BE_AADL.Annexes;
with Ocarina.BE_AADL.Identifiers;
with Ocarina.BE_AADL.Properties;

package body Ocarina.BE_AADL.Components is

   use Ocarina.Output;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.BE_AADL.Components.Features;
   use Ocarina.BE_AADL.Components.Subcomponents;
   use Ocarina.BE_AADL.Components.Connections;
   use Ocarina.BE_AADL.Components.Flows;
   use Ocarina.BE_AADL.Components.Modes;
   use Ocarina.BE_AADL.Components.Subprogram_Calls;
   use Ocarina.BE_AADL.Components.Prototypes;

   use Ocarina.BE_AADL.Annexes;
   use Ocarina.BE_AADL.Identifiers;
   use Ocarina.BE_AADL.Properties;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   Bug_Str : constant String := "[BUG is HERE]";

   ------------------------------
   -- Print_Component_Category --
   ------------------------------

   procedure Print_Component_Category (Category : Byte) is
      use Ocarina.ME_AADL;
   begin
      case Component_Category'Val (Category) is
         when CC_Data =>
            Print_Token (T_Data);
         when CC_Subprogram =>
            Print_Token (T_Subprogram);
         when CC_Subprogram_Group =>
            Print_Tokens ((T_Subprogram, T_Group));
         when CC_Thread =>
            Print_Token (T_Thread);
         when CC_Thread_Group =>
            Print_Tokens ((T_Thread, T_Group));
         when CC_Process =>
            Print_Token (T_Process);
         when CC_Memory =>
            Print_Token (T_Memory);
         when CC_Processor =>
            Print_Token (T_Processor);
         when CC_Bus =>
            Print_Token (T_Bus);
         when CC_Device =>
            Print_Token (T_Device);
         when CC_System =>
            Print_Token (T_System);
         when CC_Virtual_Processor =>
            Print_Tokens ((T_Virtual, T_Processor));
         when CC_Virtual_Bus =>
            Print_Tokens ((T_Virtual, T_Bus));
         when CC_Abstract =>
            Print_Token (T_Abstract);

         when others =>
            Write_Line (Bug_Str);
      end case;
   end Print_Component_Category;

   --------------------------
   -- Print_Component_Type --
   --------------------------

   procedure Print_Component_Type (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Component_Type);

      Node_Parent : constant Node_Id := Parent (Node);
      Comp_Name   : constant Node_Id := Identifier (Node);
      List_Node   : Node_Id;

   begin
      Write_Indentation;
      Print_Component_Category (Category (Node));
      Write_Space;
      Print_Identifier (Comp_Name);

      if Present (Node_Parent) then
         Write_Space;
         Print_Token (T_Extends);
         Write_Space;
         Print_Entity_Reference (Node_Parent);
      end if;

      if not Is_Empty (ATN.Prototype_Bindings (Node)) then
         List_Node := First_Node (ATN.Prototype_Bindings (Node));
         Write_Eol;
         Write_Indentation;
         Increment_Indentation;

         Print_Token (T_Left_Parenthesis);
         Write_Space;

         while Present (List_Node) loop
            if List_Node /= First_Node (Prototype_Bindings (Node)) then
               Print_Token (T_Comma);
               Write_Eol;
               Write_Indentation;
            end if;

            Print_Prototype_Bindings (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Write_Space;
         Print_Token (T_Right_Parenthesis);
         Decrement_Indentation;
         Write_Eol;
      end if;

      Write_Eol;

      if not Is_Empty (ATN.Prototypes (Node)) then
         List_Node := First_Node (ATN.Prototypes (Node));
         Write_Indentation;
         Print_Token (T_Prototypes);
         Write_Eol;
         Increment_Indentation;

         while Present (List_Node) loop
            Print_Prototype (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Features (Node)) then
         List_Node := First_Node (ATN.Features (Node));
         Write_Indentation;
         Print_Token (T_Features);
         Write_Eol;
         Increment_Indentation;

         while Present (List_Node) loop
            Print_Feature (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Flows (Node)) then
         List_Node := First_Node (ATN.Flows (Node));
         Write_Indentation;
         Print_Token (T_Flows);
         Write_Eol;
         Increment_Indentation;

         while Present (List_Node) loop
            Print_Flow_Spec (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Modes (Node)) then
         Write_Indentation;
         Print_Token (T_Modes);
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Modes (Node));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Mode =>
                  Print_Mode (List_Node);
               when K_Mode_Transition =>
                  Print_Mode_Transition (List_Node);
               when others =>
                  raise Program_Error;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Properties (Node)) then
         List_Node := First_Node (ATN.Properties (Node));
         Write_Indentation;
         Print_Token (T_Properties);
         Write_Eol;
         Increment_Indentation;

         while Present (List_Node) loop
            Print_Property_Association (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Annexes (Node)) then
         Increment_Indentation;
         List_Node := First_Node (ATN.Annexes (Node));

         while Present (List_Node) loop
            Print_Annex_Subclause (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      Write_Indentation;
      Print_Token (T_End);
      Write_Space;
      Print_Identifier (Comp_Name);
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Component_Type;

   ------------------------------------
   -- Print_Component_Implementation --
   ------------------------------------

   procedure Print_Component_Implementation (Node : Node_Id) is
      Node_Parent : constant Node_Id := Parent (Node);
      Impl_Ident  : constant Node_Id := Identifier (Node);
      List_Node   : Node_Id;

   begin
      Write_Indentation;
      Print_Component_Category (Category (Node));
      Write_Space;
      Print_Token (T_Implementation);
      Write_Space;
      Print_Identifier (Impl_Ident);

      if Present (Node_Parent) then
         Write_Space;
         Print_Token (T_Extends);
         Write_Space;
         Print_Entity_Reference (Node_Parent);
      end if;

      Write_Eol;

      if not Is_Empty (ATN.Prototype_Bindings (Node)) then
         List_Node := First_Node (ATN.Prototype_Bindings (Node));
         Write_Indentation;
         Increment_Indentation;

         Print_Token (T_Left_Parenthesis);
         Write_Space;

         while Present (List_Node) loop
            if List_Node /= First_Node (ATN.Prototype_Bindings (Node)) then
               Print_Token (T_Comma);
               Write_Eol;
               Write_Indentation;
            end if;

            Print_Prototype_Bindings (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Write_Space;
         Print_Token (T_Right_Parenthesis);
         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Refines_Type (Node)) then
         Write_Indentation;
         Print_Tokens ((T_Refines, T_Type));
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Refines_Type (Node));

         while Present (List_Node) loop
            Print_Feature (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Prototypes (Node)) then
         Write_Indentation;
         Print_Token (T_Prototypes);
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Prototypes (Node));

         while Present (List_Node) loop
            Print_Prototype (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Subcomponents (Node)) then
         Write_Indentation;
         Print_Token (T_Subcomponents);
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Subcomponents (Node));

         while Present (List_Node) loop
            Print_Subcomponent (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Calls (Node)) then
         Write_Indentation;
         Print_Token (T_Calls);
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Calls (Node));

         while Present (List_Node) loop
            Print_Subprogram_Call_Sequence (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Connections (Node)) then
         Write_Indentation;
         Print_Token (T_Connections);
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Connections (Node));

         while Present (List_Node) loop
            Print_Connection (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Flows (Node)) then
         Write_Indentation;
         Print_Token (T_Flows);
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Flows (Node));

         while Present (List_Node) loop
            Print_Flow_Implementation (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Modes (Node)) then
         Write_Indentation;
         Print_Token (T_Modes);
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Modes (Node));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Mode =>
                  Print_Mode (List_Node);
               when K_Mode_Transition =>
                  Print_Mode_Transition (List_Node);
               when others =>
                  raise Program_Error;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Properties (Node)) then
         Write_Indentation;
         Print_Token (T_Properties);
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Properties (Node));

         while Present (List_Node) loop
            Print_Property_Association (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Annexes (Node)) then
         Increment_Indentation;
         List_Node := First_Node (ATN.Annexes (Node));

         while Present (List_Node) loop
            Print_Annex_Subclause (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      Write_Indentation;
      Print_Token (T_End);
      Write_Space;
      Print_Identifier (Impl_Ident);
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Component_Implementation;

   ------------------------------
   -- Print_Feature_Group_Type --
   ------------------------------

   procedure Print_Feature_Group_Type (Node : Node_Id) is
      Port_Ident  : constant Node_Id := Identifier (Node);
      Node_Parent : constant Node_Id := Parent (Node);
      Inverse_Ref : constant Node_Id := Inverse_Of (Node);
      List_Node   : Node_Id;

   begin
      Write_Indentation;

      case AADL_Version is
         when AADL_V1 =>
            Print_Tokens ((T_Port, T_Group));
         when AADL_V2 =>
            Print_Tokens ((T_Feature, T_Group));
      end case;

      Write_Space;
      Print_Identifier (Port_Ident);

      if Present (Node_Parent) then
         Write_Space;
         Print_Token (T_Extends);
         Write_Space;
         Print_Entity_Reference (Node_Parent);
      end if;

      Write_Eol;

      if not Is_Empty (Prototype_Bindings (Node)) then
         List_Node := First_Node (Prototype_Bindings (Node));
         Write_Indentation;
         Increment_Indentation;

         Print_Token (T_Left_Parenthesis);
         Write_Space;

         while Present (List_Node) loop
            if List_Node /= First_Node (Prototype_Bindings (Node)) then
               Print_Token (T_Comma);
               Write_Eol;
               Write_Indentation;
            end if;

            Print_Prototype_Bindings (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Write_Space;
         Print_Token (T_Right_Parenthesis);
         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Prototypes (Node)) then
         List_Node := First_Node (ATN.Prototypes (Node));
         Write_Indentation;
         Print_Token (T_Prototypes);
         Write_Eol;
         Increment_Indentation;

         while Present (List_Node) loop
            Print_Prototype (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if Is_Empty (ATN.Features (Node)) and then not Present (Inverse_Ref) then
         if AADL_Version = AADL_V1 then
            Write_Indentation;
            Print_Token (T_Features);
            Write_Eol;
         end if;

      else
         if not Is_Empty (ATN.Features (Node)) then
            Write_Indentation;
            Print_Token (T_Features);
            Write_Eol;
            Increment_Indentation;
            List_Node := First_Node (ATN.Features (Node));

            while Present (List_Node) loop
               if not Is_Implicit_Inverse (List_Node) then
                  --  Implicit features come from 'inverse of'
                  --  statements. Hence they should not be displayed.

                  Print_Feature (List_Node);
               end if;

               List_Node := Next_Node (List_Node);
            end loop;

            Decrement_Indentation;
            Write_Eol;
         end if;

         if Present (Inverse_Ref) then
            Write_Indentation;
            Print_Tokens ((T_Inverse, T_Of));
            Write_Space;
            Print_Entity_Reference (Inverse_Ref);
            Write_Eol;
         end if;
      end if;

      if not Is_Empty (ATN.Properties (Node)) then
         Write_Indentation;
         Print_Token (T_Properties);
         Write_Eol;
         Increment_Indentation;
         List_Node := First_Node (ATN.Properties (Node));

         while Present (List_Node) loop
            Print_Property_Association (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      if not Is_Empty (ATN.Annexes (Node)) then
         Increment_Indentation;
         List_Node := First_Node (ATN.Annexes (Node));

         while Present (List_Node) loop
            Print_Annex_Subclause (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Write_Eol;
      end if;

      Write_Indentation;
      Print_Token (T_End);
      Write_Space;
      Print_Identifier (Port_Ident);
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Feature_Group_Type;

end Ocarina.BE_AADL.Components;
