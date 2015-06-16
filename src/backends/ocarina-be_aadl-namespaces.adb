------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B E _ A A D L . N A M E S P A C E S            --
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
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.Property_Sets;
with Ocarina.BE_AADL.Annexes;
with Ocarina.BE_AADL.Components;
with Ocarina.BE_AADL.Identifiers;
with Ocarina.BE_AADL.Properties;

package body Ocarina.BE_AADL.Namespaces is

   use Ocarina.Output;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.Property_Sets;
   use Ocarina.BE_AADL.Annexes;
   use Ocarina.BE_AADL.Components;
   use Ocarina.BE_AADL.Properties;
   use Ocarina.BE_AADL.Identifiers;

   procedure Print_Import_Declaration (Node : Node_Id);
   procedure Print_Alias_Declaration (Node : Node_Id);

   ------------------------------------
   -- Print_Constrained_Property_Set --
   ------------------------------------

   procedure Print_Constrained_Property_Set
     (Node      : Node_Id;
      Criterion : Node_Id)
   is
      Ident     : constant Node_Id := Identifier (Node);
      List_Node : Node_Id;

      Someting_To_Print : Boolean := False;
   begin
      --  We do not print standard property sets.

      if not Is_User_Defined (Ident) then
         return;
      end if;

      --  First of all see whether the constraint let us some
      --  declarations to print.

      if not Is_Empty (Declarations (Node)) then
         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop

            Someting_To_Print :=
              Is_Printable (List_Node, Criterion) or else Someting_To_Print;

            exit when Someting_To_Print;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if not Someting_To_Print then
         return;
      end if;

      Print_Tokens ((T_Property, T_Set));
      Write_Space;
      Print_Identifier (Ident);
      Write_Space;
      Print_Token (T_Is);
      Write_Eol;

      if not Is_Empty (Imports_List (Node)) then
         List_Node := First_Node (Imports_List (Node));

         while Present (List_Node) loop
            if List_Node /= First_Node (Imports_List (Node)) then
               Write_Eol;
            end if;

            Print_Import_Declaration (List_Node);

            List_Node := Next_Node (List_Node);
         end loop;

         Write_Eol;
         Write_Eol;
      end if;

      if not Is_Empty (Declarations (Node)) then
         List_Node := First_Node (Declarations (Node));
         Increment_Indentation;

         while Present (List_Node) loop
            if Is_Printable (List_Node, Criterion) then
               case Kind (List_Node) is
                  when K_Property_Definition_Declaration =>
                     Print_Property_Definition_Declaration (List_Node);

                  when K_Property_Type_Declaration =>
                     Print_Property_Type_Declaration (List_Node);

                  when K_Constant_Property_Declaration =>
                     Print_Constant_Property (List_Node);

                  when others =>
                     Node_Not_Handled (List_Node);
               end case;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
      end if;

      Write_Indentation;
      Print_Token (T_End);
      Write_Space;
      Print_Identifier (Ident);
      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Constrained_Property_Set;

   ------------------------
   -- Print_Property_Set --
   ------------------------

   procedure Print_Property_Set (Node : Node_Id) is
      procedure Internal_Print_Property_Set is
         new Print_Constrained_Property_Set
        (Always_Printable);
   begin
      Internal_Print_Property_Set (Node, No_Node);
   end Print_Property_Set;

   -------------------------------
   -- Print_Constrained_Package --
   -------------------------------

   procedure Print_Constrained_Package (Node : Node_Id; Criterion : Node_Id) is
      pragma Assert (Kind (Node) = K_Package_Specification);

      Pack_Identifier   : constant Node_Id := Identifier (Node);
      List_Node         : Node_Id;
      Someting_To_Print : Boolean          := False;
      Has_Public        : Boolean          := False;
      Has_Private       : Boolean          := False;
   begin
      --  First of all see whether the constraint let us some
      --  declarations to print.

      if not Is_Empty (Declarations (Node)) then
         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            Someting_To_Print :=
              Is_Printable (List_Node, Criterion) or else Someting_To_Print;

            Has_Public  := Has_Public or else not Is_Private (List_Node);
            Has_Private := Has_Private or else Is_Private (List_Node);

            List_Node := Next_Node (List_Node);
         end loop;
      else
         raise Program_Error;
      end if;

      if not Someting_To_Print then
         return;
      end if;

      Increment_Indentation;
      Print_Token (T_Package);
      Write_Space;
      Print_Identifier (Pack_Identifier);
      Write_Eol;

      --  Public part

      if Has_Public then
         Print_Token (T_Public);
         Write_Eol;

         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            if not Is_Private (List_Node)
              and then Is_Printable (List_Node, Criterion)
            then
               case Kind (List_Node) is
                  when K_Component_Type =>
                     Print_Component_Type (List_Node);

                  when K_Component_Implementation =>
                     Print_Component_Implementation (List_Node);

                  when K_Feature_Group_Type =>
                     Print_Feature_Group_Type (List_Node);

                  when K_Name_Visibility_Declaration =>
                     Print_Name_Visibility_Declaration (List_Node);

                  when others =>
                     raise Program_Error;
               end case;
            end if;

            Write_Eol;
            List_Node := Next_Node (List_Node);
         end loop;

         if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node))
           and then AADL_Version = AADL_V1
         then
            declare
               Number_Of_Properties : Integer := 0;
            begin
               List_Node :=
                 First_Node
                   (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

               while Present (List_Node) loop
                  if not Is_Private (List_Node)
                    and then Is_Printable (List_Node, Criterion)
                  then
                     Number_Of_Properties := Number_Of_Properties + 1;
                  end if;

                  List_Node := Next_Node (List_Node);
               end loop;

               if Number_Of_Properties > 0 then
                  Print_Token (T_Properties);
                  Write_Eol;
               end if;
            end;

            List_Node :=
              First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

            while Present (List_Node) loop
               if not Is_Private (List_Node)
                 and then Is_Printable (List_Node, Criterion)
               then
                  Print_Property_Association (List_Node);
               end if;

               List_Node := Next_Node (List_Node);
            end loop;
         end if;
      end if;

      --  Annex subclauses

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Annexes (Node)) then
         Increment_Indentation;
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Annexes (Node));

         while Present (List_Node) loop
            Print_Annex_Subclause (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
         Write_Eol;
      end if;

      --  private part

      if Has_Private then
         Print_Token (T_Private);
         Write_Eol;

         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            if Is_Private (List_Node)
              and then Is_Printable (List_Node, Criterion)
            then
               case Kind (List_Node) is
                  when K_Component_Type =>
                     Print_Component_Type (List_Node);

                  when K_Component_Implementation =>
                     Print_Component_Implementation (List_Node);

                  when K_Feature_Group_Type =>
                     Print_Feature_Group_Type (List_Node);

                  when K_Name_Visibility_Declaration =>
                     Print_Name_Visibility_Declaration (List_Node);
                  when others =>
                     raise Program_Error;
               end case;

               Write_Eol;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;

         if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node))
           and then AADL_Version = AADL_V1
         then
            declare
               Number_Of_Properties : Integer := 0;
            begin
               List_Node :=
                 First_Node
                   (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

               while Present (List_Node) loop
                  if Is_Private (List_Node)
                    and then Is_Printable (List_Node, Criterion)
                  then
                     Number_Of_Properties := Number_Of_Properties + 1;
                  end if;

                  List_Node := Next_Node (List_Node);
               end loop;

               if Number_Of_Properties > 0 then
                  Print_Token (T_Properties);
                  Write_Eol;
               end if;
            end;

            List_Node :=
              First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

            while Present (List_Node) loop
               if Is_Private (List_Node)
                 and then Is_Printable (List_Node, Criterion)
               then
                  Print_Property_Association (List_Node);
               end if;

               List_Node := Next_Node (List_Node);
            end loop;
         end if;
      end if;

      if AADL_Version = AADL_V2 then
         if not Is_Empty
             (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node))
         then
            List_Node :=
              First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Node));

            while Present (List_Node) loop
               if Is_Printable (List_Node, Criterion) then
                  Print_Property_Association (List_Node);
               end if;

               List_Node := Next_Node (List_Node);
            end loop;
         end if;
      end if;

      Print_Token (T_End);
      Write_Space;
      Print_Identifier (Pack_Identifier);
      Print_Token (T_Semicolon);
      Write_Eol;
      Decrement_Indentation;
   end Print_Constrained_Package;

   -------------------
   -- Print_Package --
   -------------------

   procedure Print_Package (Node : Node_Id) is
      procedure Internal_Print_Package is new Print_Constrained_Package
        (Always_Printable);
   begin
      Internal_Print_Package (Node, No_Node);
   end Print_Package;

   ------------------------------------------
   -- Print_Constrained_AADL_Specification --
   ------------------------------------------

   procedure Print_Constrained_AADL_Specification
     (Node      : Node_Id;
      Criterion : Node_Id)
   is
      pragma Assert (Kind (Node) = K_AADL_Specification);

      --  Some internal procedures

      procedure Internal_Print_Package is new Print_Constrained_Package
        (Is_Printable);
      procedure Internal_Print_Property_Set is
         new Print_Constrained_Property_Set
        (Is_Printable);

      List_Node : Node_Id;
   begin
      if not Is_Empty (Declarations (Node)) then
         List_Node := First_Node (Declarations (Node));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Component_Type =>
                  if Is_Printable (List_Node, Criterion) then
                     Print_Component_Type (List_Node);
                  end if;

               when K_Component_Implementation =>
                  if Is_Printable (List_Node, Criterion) then
                     Print_Component_Implementation (List_Node);
                  end if;

               when K_Feature_Group_Type =>
                  if Is_Printable (List_Node, Criterion) then
                     Print_Feature_Group_Type (List_Node);
                  end if;

               when K_Package_Specification =>
                  Internal_Print_Package (List_Node, Criterion);

               when K_Property_Set =>
                  Internal_Print_Property_Set (List_Node, Criterion);

               when others =>
                  raise Program_Error;
            end case;

            Write_Eol;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;
   end Print_Constrained_AADL_Specification;

   ------------------------------
   -- Print_AADL_Specification --
   ------------------------------

   procedure Print_AADL_Specification (Node : Node_Id) is
      procedure Internal_Print_AADL_Specification is
         new Print_Constrained_AADL_Specification
        (Always_Printable);
   begin
      Internal_Print_AADL_Specification (Node, No_Node);
   end Print_AADL_Specification;

   ---------------------------------------
   -- Print_Name_Visibility_Declaration --
   ---------------------------------------

   procedure Print_Name_Visibility_Declaration (Node : Node_Id) is
      List_Node : Node_Id;

   begin
      List_Node := First_Node (List_Items (Node));

      while Present (List_Node) loop
         Write_Indentation;
         case Kind (List_Node) is
            when K_Import_Declaration =>
               Print_Import_Declaration (List_Node);

            when K_Alias_Declaration =>
               Print_Alias_Declaration (List_Node);
            when others =>
               raise Program_Error;
         end case;

         Write_Eol;
         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Name_Visibility_Declaration;

   ------------------------------
   -- Print_Import_Declaration --
   ------------------------------

   procedure Print_Import_Declaration (Node : Node_Id) is
      List_Node : Node_Id;
      Name_Node : Node_Id;

   begin
      List_Node := First_Node (List_Items (Node));

      Print_Token (T_With);
      Write_Space;

      while Present (List_Node) loop
         if List_Node /= First_Node (List_Items (Node)) then
            Print_Token (T_Comma);
            Write_Space;
         end if;

         case Kind (List_Node) is
            when K_Identifier =>
               Print_Identifier (List_Node);

            when K_Package_Name =>
               Name_Node :=
                 First_Node
                   (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers (List_Node));
               while Present (Name_Node) loop
                  if Name_Node /=
                    First_Node
                      (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers (List_Node))
                  then
                     Print_Token (T_Colon_Colon);
                  end if;

                  Print_Identifier (Name_Node);
                  Name_Node := Next_Node (Name_Node);
               end loop;

            when others =>
               raise Program_Error;
         end case;

         List_Node := Next_Node (List_Node);
      end loop;

      Print_Token (T_Semicolon);
   end Print_Import_Declaration;

   -----------------------------
   -- Print_Alias_Declaration --
   -----------------------------

   procedure Print_Alias_Declaration (Node : Node_Id) is
      List_Node  : Node_Id;
      Entity_Cat : Ocarina.ME_AADL.Entity_Category;
      Is_All     : Boolean := False;

   begin
      Entity_Cat :=
        Ocarina.ME_AADL.Entity_Category'Val
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Entity_Category (Node));

      Is_All := Ocarina.ME_AADL.AADL_Tree.Nodes.Is_All (Node);

      case Entity_Cat is
         when EC_Package =>
            if Is_All then
               Print_Token (T_Renames);
            else
               Print_Identifier (Identifier (Node));
               Write_Space;
               Print_Tokens ((T_Renames, T_Package));
            end if;

            Write_Space;

            List_Node :=
              First_Node
                (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers
                   (Package_Name (Node)));
            while Present (List_Node) loop
               if List_Node /=
                 First_Node
                   (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers
                      (Package_Name (Node)))
               then
                  Print_Token (T_Colon_Colon);
               end if;

               Print_Identifier (List_Node);

               List_Node := Next_Node (List_Node);
            end loop;

            if Is_All then
               Print_Token (T_Colon_Colon);
               Print_Token (T_All);
            end if;

         when EC_Component =>
            if Identifier (Node) /= No_Node then
               Print_Identifier (Identifier (Node));
               Write_Space;
            end if;

            Print_Token (T_Renames);
            Write_Space;
            Print_Component_Category (Category (Node));
            Write_Space;
            Print_Entity_Reference (Reference (Node));

         when EC_Feature_Group_Type =>
            if Identifier (Node) /= No_Node then
               Print_Identifier (Identifier (Node));
               Write_Space;
            end if;

            Print_Tokens ((T_Renames, T_Feature, T_Group));
            Write_Space;
            Print_Entity_Reference (Reference (Node));

         when others =>
            raise Program_Error;
      end case;

      Print_Token (T_Semicolon);

   end Print_Alias_Declaration;

end Ocarina.BE_AADL.Namespaces;
