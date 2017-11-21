------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . I N S T A N C E S . M E S S A G E S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Errors;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;

package body Ocarina.Instances.Messages is

   use Errors;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Entities;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   ---------------------------
   -- Display_No_Entity_Ref --
   ---------------------------

   procedure Display_No_Entity_Ref (Node : Node_Id) is
   begin
      pragma Assert (Present (Node));
      Error_Loc (1)  := Loc (Node);
      Error_Name (1) := Get_Name_Of_Entity (Node);
      DW ("%is not associated with any entity");
   end Display_No_Entity_Ref;

   ----------------------------------------
   -- Display_Entity_Is_A_Component_Type --
   ----------------------------------------

   procedure Display_Entity_Is_A_Component_Type (Node : Node_Id) is
   begin
      pragma Assert (Present (Node));
      Error_Loc (1)  := ATN.Loc (Node);
      Error_Name (1) := Get_Name_Of_Entity (Node);
      DW ("%references a component type");
   end Display_Entity_Is_A_Component_Type;

   ---------------------------------
   -- Display_Instantiation_Error --
   ---------------------------------

   procedure Display_Instantiation_Error
     (Node  : Node_Id;
      Fatal : Boolean := True)
   is
      pragma Assert (Present (Node));
   begin
      Error_Loc (1)  := Loc (Node);
      Error_Name (1) := Get_Name_Of_Entity (Node);
      if Fatal then
         DE ("%cannot be properly instantiated");
      else
         DW ("%cannot be properly instantiated");
      end if;
      Exit_On_Error (Fatal, "Cannot instantiate full model, exit now");
   end Display_Instantiation_Error;

   procedure Display_Type_Instantiation_Error
     (Node  : Node_Id;
      Fatal : Boolean := True)
   is
      pragma Assert (Present (Node));
      Error_Msg : constant String :=
        "% (feature) cannot be properly instantiated: requires full type";
   begin
      Error_Loc (1)  := Loc (Node);
      Error_Name (1) := Get_Name_Of_Entity (Node);
      if Fatal then
         DE (Error_Msg);
      else
         DW (Error_Msg);
      end if;
      Exit_On_Error (Fatal, "Cannot instantiate full model, exit now");
   end Display_Type_Instantiation_Error;

end Ocarina.Instances.Messages;
