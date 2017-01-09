------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . A N A L Y Z E R . A A D L _ E M A             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

with Ocarina.Analyzer.AADL_EMA.Naming_Rules;
with Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL_EMA.EMA_Tree.Nutils;
with Ocarina.Analyzer.AADL_EMA.Finder;

package body Ocarina.Analyzer.AADL_EMA is

   use Ocarina.Analyzer.AADL_EMA.Naming_Rules;
   use Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
   use Ocarina.Analyzer.AADL_EMA.Finder;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ANU renames Ocarina.ME_AADL_EMA.EMA_Tree.Nutils;
   package EMATN renames Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;

   Language : constant String := "emv2";

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      ANU.Init;
      Ocarina.Analyzer.Register_Analyzer (Language, Analyze_Model'Access);
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      ANU.Reset;
   end Reset;

   -------------------
   -- Analyze_Model --
   -------------------

   function Analyze_Model (Root : Node_Id) return Boolean is
      use ATN;

      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);

      Success : Boolean := True;
      EMA_Root : Node_Id;
      Roots : Node_List;
      Packages : Node_List;
      Package_Spec : Node_Id;
      Pass_To_Next : Boolean := False;
   begin
      Get_EMA_Annexes_List
      (Root,
       Roots.First,
       Roots.Last,
       Packages.First,
       Packages.Last);

      EMA_Root := Roots.First;
      Package_Spec := Packages.First;
      while Present (EMA_Root) loop
         if EMATN.Kind (EMA_Root) = EMATN.K_Annex_Subclause then
            Success := Success and then
                 Check_Names_In_Subclause (Root, EMA_Root, Package_Spec);
            Pass_To_Next := True;
         elsif EMATN.Kind (EMA_Root) = EMATN.K_Annex_Library then
            Success := Success and then
                 Check_Names_In_Library (Root, EMA_Root, Package_Spec);
            Pass_To_Next := True;
         else
            Pass_To_Next := False;
         end if;

         if Pass_To_Next and then Present (Package_Spec)
         then
            Package_Spec := ATN.Next_Entity (Package_Spec);
         end if;

         EMA_Root := EMATN.Next_Node (EMA_Root);
      end loop;

      return Success;
   end Analyze_Model;

end Ocarina.Analyzer.AADL_EMA;
