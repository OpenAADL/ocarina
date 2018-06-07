------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . I N S T A N C E S . A N N E X E S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2010-2018 ESA & ISAE.                    --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

package body Ocarina.Instances.Annexes is

   use Ocarina.ME_AADL.AADL_Instances.Nodes;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AIU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIE renames Ocarina.ME_AADL.AADL_Instances.Entities;

   -------------------
   -- Apply_Annexes --
   -------------------

   function Apply_Annexes
     (Instance_Root : Node_Id;
      Instance      : Node_Id;
      Annex_List    : List_Id;
      Override_Mode : Boolean)
      return Boolean
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert (Present (Instance));

      Annex_Subclause   : Node_Id;
      Success : Boolean := True;
   begin
      if not ATU.Is_Empty (Annex_List) then
         Annex_Subclause := ATN.First_Node (Annex_List);

         while Present (Annex_Subclause) loop
            Success := Add_Annex_Instance
              (Instance_Root, Instance, Annex_Subclause, Override_Mode)
              and then Success;

            Annex_Subclause := ATN.Next_Node (Annex_Subclause);
         end loop;
      end if;

      return Success;
   end Apply_Annexes;

   ------------------------
   -- Add_Annex_Instance --
   ------------------------

   function Add_Annex_Instance
     (Instance_Root   : Node_Id;
      Entity_Instance : Node_Id;
      Annex_Subclause : Node_Id;
      Override_Mode   : Boolean)
      return Boolean
   is
      pragma Assert
        (Kind (Entity_Instance) = K_Component_Instance
           or else Kind (Entity_Instance) = K_Feature_Group_Spec_Instance);

      pragma Unreferenced (Instance_Root);

      Annex_Instance : Node_Id;
   begin
      Annex_Instance := AIU.New_Node
        (K_Annex_Instance, ATN.Loc (Annex_Subclause));

      AIN.Set_Identifier (Annex_Instance,
                          AIE.Duplicate_Identifier
                            (ATN.Identifier (Annex_Subclause)));

      AIN.Set_Corresponding_Annex (Annex_Instance,
                                   ATN.Corresponding_Annex (Annex_Subclause));

      if Override_Mode then
         --  Append the node to the BEGINNING of the annex list of the
         --  instance so that annexes that are declared for inheriting
         --  components override the annexes that are declared for the
         --  parent component.

         AIU.Push_Node_To_List
           (Annex_Instance,
            AIN.Annexes (Entity_Instance));
      else
         AIU.Append_Node_To_List
           (Annex_Instance,
            AIN.Annexes (Entity_Instance));
      end if;

      return True;
   end Add_Annex_Instance;

end Ocarina.Instances.Annexes;
