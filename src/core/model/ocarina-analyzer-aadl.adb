------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . A N A L Y Z E R . A A D L                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2018 ESA & ISAE.        --
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

with Ocarina.Analyzer.AADL.Legality_Rules;
with Ocarina.Analyzer.AADL.Names;
with Ocarina.Analyzer.AADL.Links;
with Ocarina.Analyzer.AADL.Semantics;
with Ocarina.Analyzer.AADL.Naming_Rules;
with Ocarina.Analyzer.AADL.Annexes;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Debug;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.Printers;

with Ocarina.Options;

package body Ocarina.Analyzer.AADL is

   use Ocarina.Analyzer.AADL.Legality_Rules;
   use Ocarina.Analyzer.AADL.Names;
   use Ocarina.Analyzer.AADL.Links;
   use Ocarina.Analyzer.AADL.Semantics;
   use Ocarina.Analyzer.AADL.Annexes;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.ME_AADL.Printers;
   use Ocarina.Options;

   Language : constant String := "aadl";

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Ocarina.Analyzer.Register_Analyzer (Language, Analyze_Model'Access);
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
   end Reset;

   -------------------
   -- Analyze_Model --
   -------------------

   function Analyze_Model (Root : Node_Id) return Boolean is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      Success : Boolean := True;

   begin
      Ocarina.Analyzer.AADL.Naming_Rules.Initialize;

      --  Check legality rules (properties, types...)

      Success := Check_Legality_Rules (Root);

      --  Link the identifiers and designators to their corresponding
      --  AADL components and namespaces.

      Success :=
        Success
        and then Check_Names_In_Namespaces (Root)
        and then Link_Declarations_Of_Namespaces (Root);

      --  Check the semantics of the AADL specification in order to
      --  display the maximum amount of error messages.

      Success := Success and then Check_Semantics_In_Namespaces (Root);

      --  Link the identifiers and designators to their corresponding
      --  AADL subcomponents, subclauses or port groups.

      Success :=
        Success
        and then Check_Names_In_Components_And_Feature_Groups (Root)
        and then Link_Subclauses_In_Components_And_Feature_Groups (Root);

      --  Check the semantics of subclauses and port groups

      Success := Success and then Check_Semantics_In_Components (Root);

      --  Link the identifiers and designators to their corresponding
      --  AADL property types and constants and check the semantics of
      --  the properties.

      Success :=
        Success
        and then Link_Properties_Of_AADL_Description (Root)
        and then Check_Semantics_Of_Properties (Root);

      Success := Success
        and then Find_Analyze_Annexes (Root);

      if Success
        and then Get_Current_Action = Analyze_Model
        and then Debug_Mode
      then
         Print_AADL_Tree
           (Root,
            Ocarina.ME_AADL.AADL_Tree.Debug.W_Node_Id'Access);
      end if;

      return Success;
   end Analyze_Model;

   ----------------------------
   -- Have_Common_Statements --
   ----------------------------

   function Have_Common_Statements
     (Node_1 : Node_Id;
      Node_2 : Node_Id) return Boolean
   is
      List_1      : List_Id;
      List_2      : List_Id;
      List_Item_1 : Node_Id;
      List_Item_2 : Node_Id;
      Name_Id_1   : Name_Id;
      Name_Id_2   : Name_Id;
   begin
      if No (Node_1) and then No (Node_2) then
         return True;
      elsif Safe_XOR (No (Node_1), No (Node_2)) then
         --  The absence of an 'in mode' (or 'in binding') statement
         --  in only one of the two nodes means that the property or
         --  the flow covers the remaing modes (or bindings). Hence no
         --  common modes (or bindings) are present.

         return False;
      end if;

      case Kind (Node_1) is
         when K_In_Modes =>
            List_1 := Modes (Node_1);

         when K_In_Binding =>
            List_1 := Binding (Node_1);

         when others =>
            raise Program_Error;
      end case;

      case Kind (Node_2) is
         when K_In_Modes =>
            List_2 := Modes (Node_2);

         when K_In_Binding =>
            List_2 := Binding (Node_2);

         when others =>
            raise Program_Error;
      end case;

      if Is_Empty (List_1) and then Is_Empty (List_2) then
         return True;

      elsif Safe_XOR (Is_Empty (List_1), Is_Empty (List_2)) then
         --  Means 'in modes (none)' or 'in bindings (none)'. Hence no
         --  common statements are present.

         return False;

      else
         List_Item_1 := First_Node (List_1);
         List_Item_2 := First_Node (List_2);

         while Present (List_Item_1) loop
            List_Item_2 := First_Node (List_2);

            while Present (List_Item_2) loop
               if Kind (List_Item_1) = Kind (List_Item_2) then
                  if Kind (List_Item_1) = K_Entity_Reference then
                     Name_Id_1 := Get_Name_Of_Entity_Reference (List_Item_1);
                     Name_Id_2 := Get_Name_Of_Entity_Reference (List_Item_2);

                     if Name_Id_1 = Name_Id_2 then
                        return True;
                        --  XXX We only consider identifers or modes,
                        --  not classifier references, etc. Hence this
                        --  test is incomplete. The tree structure
                        --  should be modified to avoid so that we can
                        --  use the same accessor for all the node
                        --  kinds
                     end if;
                  end if;
               end if;

               List_Item_2 := Next_Node (List_Item_2);
            end loop;

            List_Item_1 := Next_Node (List_Item_1);
         end loop;

         return False;
      end if;
   end Have_Common_Statements;

end Ocarina.Analyzer.AADL;
