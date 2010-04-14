------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . X M L _ T R E E . G E N E R A T O R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2008-2009, GET-Telecom Paris.                --
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

with Namet;  use Namet;
with Output; use Output;
with Utils;  use Utils;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.XML_Values;
with Ocarina.Backends.Messages;

package body Ocarina.Backends.XML_Tree.Generator is

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.XML_Tree.Nodes;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.XML_Values;
   use Ocarina.Backends.Messages;

   procedure Generate_XML_Comment (N : Node_Id);
   procedure Generate_HI_Distributed_Application (N : Node_Id);
   procedure Generate_HI_Node (N : Node_Id);
   procedure Generate_Defining_Identifier (N : Node_Id);
   procedure Generate_HI_Unit (N : Node_Id);
   procedure Generate_Assignement (N : Node_Id);
   procedure Generate_Literal (N : Node_Id);
   procedure Generate_XML_Node (N : Node_Id);
   procedure Generate_XML_File (N : Node_Id);

   procedure Write (T : Token_Type);
   procedure Write_Line (T : Token_Type);

   function Get_File_Name (N : Node_Id) return Name_Id;
   --  Generate a file name from the package node given as parameter

   procedure Release_Output (Fd : File_Descriptor);
   --  Releases the output by closing the opened files

   function Set_Output (N : Node_Id) return File_Descriptor;
   --  Adjust the output depending on the command line options and
   --  return a file descriptor in order to be able to close it.

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (N : Node_Id) return Name_Id is
      Suffix : constant String := ".xml";
   begin
      --  The File name corresponding is the lowerd name of N

      Get_Name_String
        (Conventional_Base_Name
         (Name
          (Defining_Identifier
           (N))));

      --  Adding file suffix

      Add_Str_To_Name_Buffer (Suffix);

      return Name_Find;
   end Get_File_Name;

   ----------------
   -- Set_Output --
   ----------------

   function Set_Output (N : Node_Id) return File_Descriptor is
   begin
      if not Print_On_Stdout then
         declare
            File_Name : constant Name_Id
              := Get_File_Name (N);
            Fd : File_Descriptor;
         begin
            Get_Name_String (File_Name);

            --  Create a new file and overwrites existing file with
            --  the same name

            Fd := Create_File (Name_Buffer (1 .. Name_Len), Text);

            if Fd = Invalid_FD then
               raise Program_Error;
            end if;

            --  Setting the output

            Set_Output (Fd);
            return Fd;
         end;
      end if;

      return Invalid_FD;
   end Set_Output;

   --------------------
   -- Release_Output --
   --------------------

   procedure Release_Output (Fd : File_Descriptor) is
   begin
      if not Print_On_Stdout and then Fd /= Invalid_FD then
         Set_Standard_Output;
         Close (Fd);
      end if;
   end Release_Output;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is

         when K_XML_Comment =>
            Generate_XML_Comment (N);

         when K_HI_Distributed_Application =>
            Generate_HI_Distributed_Application (N);

         when K_HI_Unit =>
            Generate_HI_Unit (N);

         when K_XML_File =>
            Generate_XML_File (N);

         when K_HI_Node =>
            Generate_HI_Node (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when K_XML_Node =>
            Generate_XML_Node (N);

         when K_Literal =>
            Generate_Literal (N);

         when K_Assignement =>
            Generate_Assignement (N);

         when others =>
            Display_Error ("other element in generator", Fatal => False);
            null;
      end case;
   end Generate;

   --------------------------
   -- Generate_XML_Comment --
   --------------------------

   procedure Generate_XML_Comment (N : Node_Id) is
      --  This procedure does the following :

      --  * It generates an XML comment basing on the name of node N

      --  * If the name it too long, and depending on the location of
      --    the comment in the source code, the procedure splits the
      --    comment into more than a line.

      --  The comment is assumed to be a sequence of caracters,
      --  beginning and ending with a NON-SPACE caracter.

      --  A word is :

      --  a space character, or else a sequence of non space
      --  characters located between two spaces.

      --  The maximum length of a line, in colums
      Max_Line_Length : constant Natural := 78;

      function Are_There_More_Words return Boolean;
      --  This function returns True if there are words in the buffer

      function Next_Word_Length return Natural;
      --  This function returns the size of the next word to be
      --  got. It returns zero when the buffer is empty.

      function Get_Next_Word return String;
      --  This function extracts the next word from the buffer

      --------------------------
      -- Are_There_More_Words --
      --------------------------

      function Are_There_More_Words return Boolean is
      begin
         return (Name_Len /= 0);
      end Are_There_More_Words;

      ----------------------
      -- Next_Word_Length --
      ----------------------

      function Next_Word_Length return Natural is
         L : Natural;
      begin
         if not Are_There_More_Words then
            L := 0;
         elsif Name_Buffer (1) = ' ' then
            L := 1;
         else
            L := 0;
            while L + 1 <= Name_Len and then  Name_Buffer (L + 1) /= ' ' loop
               L := L + 1;
            end loop;
         end if;
         return L;
      end Next_Word_Length;

      -------------------
      -- Get_Next_Word --
      -------------------

      function Get_Next_Word return String is
         L : constant Natural := Next_Word_Length;
      begin
         if L = 0 then
            return "";
         else
            declare
               Next_Word : constant String := Name_Buffer (1 .. L);
            begin
               if Name_Len = L then
                  Name_Len := 0;
               else
                  Set_Str_To_Name_Buffer (Name_Buffer (L + 1 .. Name_Len));
               end if;
               return Next_Word;
            end;
         end if;
      end Get_Next_Word;

      First_Line : Boolean := True;
      Used_Columns : Natural;
   begin
      Get_Name_String (Name (Defining_Identifier (N)));

      while Are_There_More_Words loop
         Used_Columns := N_Space;
         if First_Line then
            First_Line := False;
         else
            Write_Indentation;
         end if;

         --  We consume 4 colums

         Used_Columns := Used_Columns + 2;
         Write_Str ("/*");

         Used_Columns := Used_Columns + Next_Word_Length;
         Write_Str (Get_Next_Word);

         while Are_There_More_Words
           and then (Used_Columns + Next_Word_Length < Max_Line_Length)
         loop
            Used_Columns := Used_Columns + Next_Word_Length;
            Write_Str (Get_Next_Word);
         end loop;
         Write_Str ("*/");

         if Are_There_More_Words then
            Write_Eol;
         end if;
      end loop;
      Write_Eol;
   end Generate_XML_Comment;

   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------

   procedure Generate_Defining_Identifier (N : Node_Id) is
   begin
      Write_Name (Name (N));
   end Generate_Defining_Identifier;

   -----------------------------------------
   -- Generate_HI_Distributed_Application --
   -----------------------------------------

   procedure Generate_HI_Distributed_Application (N : Node_Id) is
      P                     : Node_Id := First_Node (HI_Nodes (N));
      Application_Directory : Name_Id;
   begin
      --  Create the application directory (a lower case string)
      Get_Name_String (Name (N));
      Application_Directory := To_Lower (Name_Find);

      Create_Directory (Application_Directory);

      --  Process the application nodes

      Enter_Directory (Application_Directory);

      while Present (P) loop
         Generate (P);
         P := Next_Node (P);
      end loop;

      Leave_Directory;
   end Generate_HI_Distributed_Application;

   ----------------------
   -- Generate_HI_Node --
   ----------------------

   procedure Generate_HI_Node (N : Node_Id) is
      U                   : Node_Id := First_Node (Units (N));
   begin
      while Present (U) loop
         Generate (U);
         U := Next_Node (U);
      end loop;
   end Generate_HI_Node;

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

   ----------------------
   -- Generate_HI_Unit --
   ----------------------

   procedure Generate_HI_Unit (N : Node_Id) is
   begin
      Generate (XML_File (N));
   end Generate_HI_Unit;

   ----------------------
   -- Generate_Literal --
   ----------------------

   procedure Generate_Literal (N : Node_Id) is
   begin
      Write_Str (Image (Value (N)));
   end Generate_Literal;

   --------------------------
   -- Generate_Assignement --
   --------------------------

   procedure Generate_Assignement (N : Node_Id) is
   begin
      Generate (Left_Expression (N));
      Write (Tok_Equal);
      Write_Char ('"');
      Generate (Right_Expression (N));
      Write_Char ('"');
   end Generate_Assignement;

   -----------------------
   -- Generate_XML_Node --
   -----------------------

   procedure Generate_XML_Node (N : Node_Id) is
      P : Node_Id;
   begin
      if Name (Defining_Identifier (N)) = No_Name then
         P := First_Node (Subitems (N));
         while Present (P) loop
            Write_Indentation (-1);
            Generate (P);
            P := Next_Node (P);
         end loop;
         return;
      end if;

      Write (Tok_Less);
      Generate (Defining_Identifier (N));

      if Items (N) /= No_List then
         P := First_Node (Items (N));
         while Present (P) loop
            Write_Space;
            Generate (P);
            P := Next_Node (P);
         end loop;
      end if;

      if Is_Empty (Subitems (N)) and then not Present (Node_Value (N)) then
         Write (Tok_Slash);
         Write (Tok_Greater);
         Write_Eol;
      else
         if Present (Node_Value (N)) then
            Write (Tok_Greater);
            Generate (Node_Value (N));
         else
            Write (Tok_Greater);
            Write_Eol;
            Increment_Indentation;
            P := First_Node (Subitems (N));
            while Present (P) loop
               Write_Indentation (-1);
               Generate (P);
               P := Next_Node (P);
            end loop;
            Decrement_Indentation;
            Write_Indentation (-1);
         end if;
         Write (Tok_Less);
         Write (Tok_Slash);
         Generate (Defining_Identifier (N));
         Write_Line (Tok_Greater);
      end if;
   end Generate_XML_Node;

   -----------------------
   -- Generate_XML_File --
   -----------------------

   procedure Generate_XML_File (N : Node_Id) is
      Fd : File_Descriptor;
   begin
      if No (N) then
         return;
      end if;
      Fd := Set_Output (N);

      Generate (Root_Node (N));

      Release_Output (Fd);
   end Generate_XML_File;

end Ocarina.Backends.XML_Tree.Generator;
