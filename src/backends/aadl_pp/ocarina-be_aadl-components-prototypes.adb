------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BE_AADL.COMPONENTS.PROTOTYPES                   --
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
with Ocarina.BE_AADL.Identifiers;

package body Ocarina.BE_AADL.Components.Prototypes is

   use Ocarina.Output;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.BE_AADL.Components;
   use Ocarina.BE_AADL.Identifiers;

   ---------------------
   -- Print_Prototype --
   ---------------------

   procedure Print_Prototype (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Prototype);
      Class_Ref : constant Node_Id := Entity_Ref (Node);

   begin
      Write_Indentation;
      Print_Item_Refined_To (Node);
      Write_Space;
      Print_Component_Category (Category (Node));

      if Present (Class_Ref) then
         Write_Space;
         Print_Entity_Reference (Class_Ref);
      end if;

      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Prototype;

   ------------------------------
   -- Print_Prototype_Bindings --
   ------------------------------

   procedure Print_Prototype_Bindings (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Binding_Prototype);
      Class_Ref : constant Node_Id := Entity_Ref (Node);

   begin
      Print_Identifier (Identifier (Node));
      Write_Space;
      Print_Token (T_Association);
      Write_Space;
      Print_Component_Category (Category (Node));

      if Present (Class_Ref) then
         Write_Space;
         Print_Entity_Reference (Class_Ref);
      end if;

   end Print_Prototype_Bindings;

end Ocarina.BE_AADL.Components.Prototypes;
