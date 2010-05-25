------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.ASN1_TREE.GENERATOR                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2010, European Space Agency (ESA).              --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Outfiles; use Outfiles;
with Namet;    use Namet;
with Output;   use Output;

with Ocarina.Backends.ASN1_Values;
with Ocarina.Backends.ASN1_Tree.Nodes;
with Ocarina.Backends.ASN1_Tree.Nutils;
with Ocarina.Backends.Messages;

package body Ocarina.Backends.ASN1_Tree.Generator is

   use Ocarina.Backends.ASN1_Values;
   use Ocarina.Backends.ASN1_Tree.Nodes;
   use Ocarina.Backends.ASN1_Tree.Nutils;
   use Ocarina.Backends.Messages;

   procedure Write (T : Token_Type);
   procedure Write_Line (T : Token_Type);

   pragma Unreferenced (Write_Line);

   procedure Generate_ASN1_File (N : Node_Id);
   procedure Generate_Module (N : Node_Id);
   procedure Generate_Type_Definition (N : Node_Id);
   procedure Generate_Enumerated (N : Node_Id);
   procedure Generate_Enumerated_Value (N : Node_Id);
   procedure Generate_Sequence (N : Node_Id);
   procedure Generate_Sequence_Member (N : Node_Id);
   procedure Generate_Choice (N : Node_Id);
   procedure Generate_Choice_Member (N : Node_Id);
   procedure Generate_Defining_Identifier (N : Node_Id);

   -----------
   -- Write --
   -----------

   procedure Write (T : Token_Type) is
   begin
      Write_Name (Token_Image (T));
   end Write;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (T : Token_Type) is
   begin
      Write (T);
      Write_Eol;
   end Write_Line;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is
         when K_ASN1_File =>
            Generate_ASN1_File (N);

         when K_ASN1_Module =>
            Generate_Module (N);

         when K_Type_Definition =>
            Generate_Type_Definition (N);

         when K_Enumerated =>
            Generate_Enumerated (N);

         when K_Enumerated_Value =>
            Generate_Enumerated_Value (N);

         when K_Sequence =>
            Generate_Sequence (N);

         when K_Sequence_Member =>
            Generate_Sequence_Member (N);

         when K_Choice =>
            Generate_Choice (N);

         when K_Choice_Member =>
            Generate_Choice_Member (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when others =>
            Display_Error ("other element in generator", Fatal => False);
            null;
      end case;
   end Generate;

   ------------------------
   -- Generate_ASN1_File --
   ------------------------

   procedure Generate_ASN1_File (N : Node_Id) is
      Fd : File_Descriptor;
   begin
      if No (N) then
         return;
      end if;
      Get_Name_String (Name (Defining_Identifier (N)));
      Fd := Create_File (Name_Buffer (1 .. Name_Len) & ".asn1", Text);
      Set_Output (Fd);

      Generate (Module_Node (N));

      Release_Output (Fd);
   end Generate_ASN1_File;

   ---------------------
   -- Generate_Module --
   ---------------------

   procedure Generate_Module (N : Node_Id) is
      P : Node_Id;
   begin
      Write_Name (Name (N));
      Write_Space;
      Write_Str ("DEFINITIONS AUTOMATIC TAGS ::= BEGIN");
      Write_Eol;
      if not Is_Empty (Definitions (N)) then
         P := First_Node (Definitions (N));
         while Present (P) loop
            Generate (P);
            P := Next_Node (P);
         end loop;
      end if;
      Write_Line ("END");
   end Generate_Module;

   ------------------------------
   -- Generate_Type_Definition --
   ------------------------------

   procedure Generate_Type_Definition (N : Node_Id) is
   begin
      Write_Name (Name (N));
      Write_Space;
      Write_Str (" ::= ");
      Generate (Declaration (N));
   end Generate_Type_Definition;

   -------------------------
   -- Generate_Enumerated --
   -------------------------

   procedure Generate_Enumerated (N : Node_Id) is
      P : Node_Id;
   begin
      Write_Str (" ENUMERATED {");
      Write_Eol;
      Increment_Indentation;
      Write_Indentation;
      if not Is_Empty (Values (N)) then
         P := First_Node (Values (N));
         while Present (P) loop
            Generate (P);
            P := Next_Node (P);
            if P /= No_Node then
               Write_Char (',');
               Write_Eol;
               Write_Indentation;
            end if;
         end loop;
      end if;
      Write_Eol;
      Write_Indentation;
      Write_Line ("}");
      Decrement_Indentation;
      Write_Indentation;
   end Generate_Enumerated;

   -------------------------------
   -- Generate_Enumerated_Value --
   -------------------------------

   procedure Generate_Enumerated_Value (N : Node_Id) is
   begin
      Write_Name (Name (N));
      if Value (N) /= No_Value then
         Write_Str (" (");
         Write_Str (Image (Value (N)));
         Write_Char (')');
      end if;
   end Generate_Enumerated_Value;

   -----------------------
   -- Generate_Sequence --
   -----------------------

   procedure Generate_Sequence (N : Node_Id) is
      P : Node_Id;
   begin
      Write_Line (" SEQUENCE {");
      Increment_Indentation;
      if not Is_Empty (Values (N)) then
         P := First_Node (Values (N));
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            P := Next_Node (P);
            if P /= No_Node then
               Write_Char (',');
               Write_Eol;
            end if;
         end loop;
      end if;
      Write_Eol;
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("}");
   end Generate_Sequence;

   ------------------------------
   -- Generate_Sequence_Member --
   ------------------------------

   procedure Generate_Sequence_Member (N : Node_Id) is
   begin
      Write_Name (Member_Name (N));
      Write_Space;
      Generate (Member_Type (N));
   end Generate_Sequence_Member;

   ---------------------
   -- Generate_Choice --
   ---------------------

   procedure Generate_Choice (N : Node_Id) is
      P : Node_Id;
   begin
      Write_Line (" CHOICE {");
      Increment_Indentation;
      Write_Indentation (-1);
      if not Is_Empty (Values (N)) then
         P := First_Node (Values (N));
         while Present (P) loop
            Generate (P);
            P := Next_Node (P);
            if P /= No_Node then
               Write_Char (',');
               Write_Eol;
               Write_Indentation;
            end if;
         end loop;
      end if;
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("}");
   end Generate_Choice;

   ------------------------------
   -- Generate_Choice_Member --
   ------------------------------

   procedure Generate_Choice_Member (N : Node_Id) is
   begin
      Write_Name (Member_Name (N));
      Write_Space;
      Generate (Member_Type (N));
   end Generate_Choice_Member;

   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------

   procedure Generate_Defining_Identifier (N : Node_Id) is
   begin
      Write_Name (Name (N));
   end Generate_Defining_Identifier;

end Ocarina.Backends.ASN1_Tree.Generator;
