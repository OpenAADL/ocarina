------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BACKENDS.ADA_TREE.GENERATOR.SPARK                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ocarina.Namet;  use Ocarina.Namet;
with Ocarina.Output; use Ocarina.Output;

with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Generator;
package body Ocarina.Backends.Ada_Tree.Generator.Spark is

   use Ocarina.Backends.Ada_Tree.Generator;
   use Ocarina.Backends.Ada_Tree.Nodes;
   use Ocarina.Backends.Ada_Tree.Nutils;

   procedure Write (T : Token_Type);

   -----------
   -- Write --
   -----------

   procedure Write (T : Token_Type) is
   begin
      Write_Name (Token_Image (T));
   end Write;

   ----------------------------------
   -- Generate_Inherit_Annotations --
   ----------------------------------

   procedure Generate_Inherit_Annotations (N : Node_Id) is
      P  : Node_Id;
      LS : List_Id := No_List;
      LB : List_Id := No_List;
   begin

      --  Initialize withed package lists for a package specification
      if Kind (N) = K_Package_Specification then
         LS := Withed_Packages (N);
         LB :=
           Withed_Packages (Package_Implementation (Package_Declaration (N)));
      --  Initialize withed package lists for a subprogram specification
      elsif Kind (N) = K_Subprogram_Specification then
         LS := Withed_Packages (N);
         if Subprogram_Implementation (Main_Subprogram_Unit (N)) /=
           No_Node
         then
            LB :=
              Withed_Packages
                (Subprogram_Implementation (Main_Subprogram_Unit (N)));
         end if;
      --  Initialize withed package lists for a subprogram implementation
      elsif Kind (N) = K_Subprogram_Implementation then
         LS := Withed_Packages (N);
         if Subprogram_Specification (Main_Subprogram_Unit (N)) /= No_Node then
            LB :=
              Withed_Packages
                (Subprogram_Specification (Main_Subprogram_Unit (N)));
         end if;
      end if;

      --  inherit clauses for package specification
      if not Is_Empty (LS) then
         Increment_Indentation;
         Write (Tok_Annotation);
         Write_Space;
         Write (Tok_Inherit);
         Write_Space;
         P := First_Node (LS);
         loop
            Generate (Defining_Identifier (P));
            P := Next_Node (P);
            exit when No (P);
            Write (Tok_Comma);
            Write_Eol;
            Write (Tok_Annotation);
            Write_Indentation (7);
         end loop;
      end if;

      --  inherit clauses for package implementation
      if Is_Empty (Withed_Packages (N)) and then not Is_Empty (LB) then
         Increment_Indentation;
         Write (Tok_Annotation);
         Write_Space;
         Write (Tok_Inherit);
         Write_Space;
      elsif not Is_Empty (LS) and then not Is_Empty (LB) then
         Write (Tok_Comma);
         Write_Eol;
         Write (Tok_Annotation);
         Write_Indentation (7);
      end if;
      if not Is_Empty (LB) then
         P := First_Node (LB);
         loop
            Generate (Defining_Identifier (P));
            P := Next_Node (P);
            exit when No (P);
            Write (Tok_Comma);
            Write_Eol;
            Write (Tok_Annotation);
            Write_Indentation (7);
         end loop;
      end if;
      if not Is_Empty (LS) or else not Is_Empty (LB) then
         Write (Tok_Semicolon);
         Write_Eol;
         Decrement_Indentation;
      end if;
   end Generate_Inherit_Annotations;

   -----------------------------
   -- Generate_Own_Annotation --
   -----------------------------

   procedure Generate_Own_Annotation (N : Node_Id) is
   begin

      case Own_Mode (N) is
         when Mode_In =>
            null;

         when Mode_Out =>
            Write (Tok_Out);
            Write_Space;

         when Mode_Inout =>
            Write (Tok_In);
            Write_Space;
            Write (Tok_Out);
            Write_Space;
      end case;

      if Is_Protected (N) then
         Write (Tok_Protected);
         Write_Space;
      end if;
      Generate (Variable (N));
   end Generate_Own_Annotation;

   ----------------------------------
   -- Generate_Own_Annotation_List --
   ----------------------------------

   procedure Generate_Own_Annotation_List (L : List_Id) is
      P : Node_Id;
   begin
      P := First_Node (L);
      Write (Tok_Annotation);
      Write_Space;
      Write (Tok_Own);
      loop
         Write_Indentation;
         Write_Space;
         Generate (P);
         P := Next_Node (P);
         exit when No (P);
         Write (Tok_Semicolon);
         Write_Eol;
         Write (Tok_Annotation);
      end loop;
      Write (Tok_Semicolon);
      Write_Eol;
   end Generate_Own_Annotation_List;

end Ocarina.Backends.Ada_Tree.Generator.Spark;
