------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BE_AADL.COMPONENTS.SUBCOMPONENTS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Ocarina.BE_AADL.Components.Modes;
with Ocarina.BE_AADL.Components.Prototypes;
with Ocarina.BE_AADL.Components.Arrays;
with Ocarina.BE_AADL.Properties;
with Ocarina.BE_AADL.Identifiers;

package body Ocarina.BE_AADL.Components.Subcomponents is

   use Ocarina.Output;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.BE_AADL.Properties;
   use Ocarina.BE_AADL.Components.Modes;
   use Ocarina.BE_AADL.Components.Prototypes;
   use Ocarina.BE_AADL.Components.Arrays;
   use Ocarina.BE_AADL.Identifiers;

   ------------------------
   -- Print_Subcomponent --
   ------------------------

   procedure Print_Subcomponent (Node : Node_Id) is

      Class_Ref      : constant Node_Id := Entity_Ref (Node);
      Subcomp_Modes  : constant Node_Id := In_Modes (Node);
      Subcomp_Array  : Node_Id;
      Proto_Bindings : List_Id;
      List_Node      : Node_Id;

   begin
      Subcomp_Array  := Array_Dimensions (Node);
      Proto_Bindings := Prototype_Bindings (Node);

      Write_Indentation;
      Print_Item_Refined_To (Node);
      Write_Space;
      Print_Component_Category (Category (Node));

      if Present (Class_Ref) then
         Write_Space;
         Print_Entity_Reference (Class_Ref);
      end if;

      if not Is_Empty (Proto_Bindings) then
         List_Node := First_Node (Proto_Bindings);

         Write_Space;
         Print_Token (T_Left_Parenthesis);
         Write_Space;

         while Present (List_Node) loop
            if List_Node /= First_Node (Proto_Bindings) then
               Print_Token (T_Comma);
               Write_Space;
            end if;

            Print_Prototype_Bindings (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Write_Space;
         Print_Token (T_Right_Parenthesis);
      end if;

      if Present (Subcomp_Array) then
         Write_Space;
         Print_Array_Dimensions (Subcomp_Array);
      end if;

      Print_Contained_Property_Associations
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

      if Present (Subcomp_Modes) then
         Write_Eol;
         Print_In_Modes (Subcomp_Modes);
      end if;

      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Subcomponent;

end Ocarina.BE_AADL.Components.Subcomponents;
