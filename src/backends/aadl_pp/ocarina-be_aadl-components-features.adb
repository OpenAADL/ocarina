------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B E _ A A D L . C O M P O N E N T S . F E A T U R E S   --
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
with Ocarina.BE_AADL.Properties;
with Ocarina.BE_AADL.Identifiers;
with Ocarina.BE_AADL.Components;
with Ocarina.BE_AADL.Components.Arrays;

package body Ocarina.BE_AADL.Components.Features is

   use Ocarina.Output;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.BE_AADL.Properties;
   use Ocarina.BE_AADL.Identifiers;
   use Ocarina.BE_AADL.Components.Arrays;

   -------------------
   -- Print_Feature --
   -------------------

   procedure Print_Feature (Node : Node_Id) is
      pragma Assert (Present (Node));

   begin
      Write_Indentation;

      case Kind (Node) is
         when K_Parameter =>
            Print_Parameter (Node);
         when K_Port_Spec =>
            Print_Port_Spec (Node);
         when K_Feature_Group_Spec =>
            Print_Feature_Group_Spec (Node);
         when K_Subcomponent_Access =>
            Print_Subcomponent_Access (Node);
         when K_Subprogram_Spec =>
            Print_Subprogram_Spec (Node);
         when others =>
            raise Program_Error;
      end case;

      Print_Contained_Property_Associations
        (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Feature;

   ---------------------
   -- Print_Parameter --
   ---------------------

   procedure Print_Parameter (Node : Node_Id) is
   begin
      Print_Item_Refined_To (Node);
      Write_Space;

      if Is_In (Node) then
         Print_Token (T_In);
         Write_Space;
      end if;

      if Is_Out (Node) then
         Print_Token (T_Out);
         Write_Space;
      end if;

      Print_Token (T_Parameter);
      Write_Space;

      if Entity_Ref (Node) /= No_Node then
         Print_Entity_Reference (Entity_Ref (Node));
      end if;
   end Print_Parameter;

   ------------------------------
   -- Print_Feature_Group_Spec --
   ------------------------------

   procedure Print_Feature_Group_Spec (Node : Node_Id) is
   begin
      Print_Item_Refined_To (Node);
      Write_Space;

      case AADL_Version is
         when AADL_V1 =>
            Print_Tokens ((T_Port, T_Group));
         when AADL_V2 =>
            Print_Tokens ((T_Feature, T_Group));
      end case;
      Write_Space;

      if Inverse_Of (Node) /= No_Node then
         Write_Space;
         Print_Tokens ((T_Inverse, T_Of));
         Write_Space;
         Print_Entity_Reference (Inverse_Of (Node));
      end if;

      if Entity_Ref (Node) /= No_Node then
         Print_Entity_Reference (Entity_Ref (Node));
      end if;
   end Print_Feature_Group_Spec;

   ---------------------
   -- Print_Port_Spec --
   ---------------------

   procedure Print_Port_Spec (Node : Node_Id) is
      Class_Ref : constant Node_Id := Entity_Ref (Node);

   begin
      Print_Item_Refined_To (Node);
      Write_Space;

      if Is_In (Node) then
         Print_Token (T_In);
         Write_Space;
      end if;

      if Is_Out (Node) then
         Print_Token (T_Out);
         Write_Space;
      end if;

      if Is_Event (Node) then
         Print_Token (T_Event);
         Write_Space;
      end if;

      if Is_Data (Node) or else Is_Feature (Node) then
         if Is_Data (Node) then
            Print_Token (T_Data);
            Write_Space;
            Print_Token (T_Port);
         elsif Is_Feature (Node) then
            Print_Token (T_Feature);
         end if;

         if Present (Class_Ref) then
            Write_Space;
            Print_Entity_Reference (Class_Ref);
         end if;
      else
         Print_Token (T_Port);
      end if;

      if Present (Array_Dimensions (Node)) then
         Write_Space;
         Print_Array_Dimensions (Array_Dimensions (Node));
      end if;
   end Print_Port_Spec;

   -------------------------------
   -- Print_Subcomponent_Access --
   -------------------------------

   procedure Print_Subcomponent_Access (Node : Node_Id) is
      Class_Ref : constant Node_Id := Entity_Ref (Node);

   begin
      Print_Item_Refined_To (Node);
      Write_Space;

      if Is_Provided (Node) then
         Print_Token (T_Provides);
      else
         Print_Token (T_Requires);
      end if;

      Write_Space;
      Print_Component_Category (Subcomponent_Category (Node));
      Write_Space;
      Print_Token (T_Access);

      if Present (Class_Ref) then
         Write_Space;
         Print_Entity_Reference (Class_Ref);
      end if;
   end Print_Subcomponent_Access;

   ---------------------------
   -- Print_Subprogram_Spec --
   ---------------------------

   procedure Print_Subprogram_Spec (Node : Node_Id) is
      Subprog_Ref : constant Node_Id := Entity_Ref (Node);

   begin
      Print_Item_Refined_To (Node);
      Write_Space;

      if Is_Server (Node) then
         Print_Token (T_Server);
         Write_Space;
      end if;

      Print_Token (T_Subprogram);

      if Present (Subprog_Ref) then
         Write_Space;
         Print_Entity_Reference (Subprog_Ref);
      end if;
   end Print_Subprogram_Spec;

end Ocarina.BE_AADL.Components.Features;
