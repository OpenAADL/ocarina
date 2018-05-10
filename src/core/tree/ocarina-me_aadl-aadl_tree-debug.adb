------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . M E _ A A D L . A A D L _ T R E E . D E B U G       --
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

with GNAT.Table;

with Charset;   use Charset;
with Locations; use Locations;
with Ocarina.Namet;     use Ocarina.Namet;
with Utils;     use Utils;

with Ocarina.Annotations; use Ocarina.Annotations;

package body Ocarina.ME_AADL.AADL_Tree.Debug is

   -----------
   -- Image --
   -----------

   function Image (N : Node_Kind) return String is
      S : String := Node_Kind'Image (N);
   begin
      To_Lower (S);
      for I in S'Range loop
         if S (I) = '_' then
            S (I) := ' ';
         end if;
      end loop;
      return S (3 .. S'Last);
   end Image;

   function Image (N : Name_Id) return String is
   begin
      if N = No_Name then
         return No_Str;
      else
         return Get_Name_String (N);
      end if;
   end Image;

   function Image (N : Node_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : List_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Value_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Boolean) return String is
   begin
      return Boolean'Image (N);
   end Image;

   function Image (N : Byte) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Int) return String is
      S : constant String := Int'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   ---------------
   -- W_Boolean --
   ---------------

   procedure W_Boolean (N : Boolean) is
   begin
      Write_Str (N'Img);
   end W_Boolean;

   ------------
   -- W_Byte --
   ------------

   procedure W_Byte (N : Byte) is
   begin
      Write_Int (Int (N));
   end W_Byte;

   -----------------
   -- W_Full_Tree --
   -----------------

   procedure W_Full_Tree (N : Node_Id) is
      D : Node_Id := First_Node (List_Id (N));
   begin
      N_Indents := 0;
      while Present (D) loop
         W_Node_Id (D);
         D := Next_Node (D);
      end loop;
   end W_Full_Tree;

   ---------------
   -- W_Indents --
   ---------------

   procedure W_Indents is
   begin
      for I in 1 .. N_Indents loop
         Write_Str (" ");
      end loop;
   end W_Indents;

   ---------------
   -- W_List_Id --
   ---------------

   procedure W_List_Id (L : List_Id) is
      E : Node_Id;
   begin
      if L = No_List then
         return;
      end if;

      E := First_Node (L);
      while E /= No_Node loop
         W_Node_Id (E);
         E := Next_Node (E);
      end loop;
   end W_List_Id;

   ----------------------
   -- W_Node_Attribute --
   ----------------------

   procedure W_Node_Attribute
     (A : String;
      K : String;
      V : String;
      N : Int := 0)
   is
   begin
      --  XXX Why skipping these cases ?
      if A = "Next_Node"
        or else A = "Next_Entity"
        or else A = "Homonym"
        or else A = "Name"
        or else A = "Visible"
        or else A = "Next_Identifier"
      then
         return;
      end if;
      N_Indents := N_Indents + 1;
      W_Indents;
      Write_Str (A);
      Write_Char (' ');
      Write_Str (K);
      Write_Char (' ');
      if K = "Name_Id" then
         Write_Line (Quoted (V));
      else
         Write_Line (V);
      end if;

      --  We skip these cases to avoid infinite loop when dumping a tree.

      if A /= "Corresponding_Entity"
        and then A /= "Corresponding_Declaration"
        and then A /= "Corresponding_Annex"
        and then A /= "Container_Component"
        and then A /= "Item"
        and then A /= "Extra_Item"
        and then A /= "Declaration"
        and then A /= "Entity"
        and then A /= "Entity_Scope"
        and then A /= "Inversed_Entity"
        and then A /= "Namespace"
        and then A /= "Namespace_Path"
        and then A /= "Namespace_Identifier"
        and then A /= "Parent_Component"
        and then A /= "Parent_Entity"
        and then A /= "Parent_Sequence"
        and then A /= "Parent_Subcomponent"
        and then A /= "Potential_Scope"
        and then A /= "Property_Scope"
        and then A /= "Reference"
        and then A /= "Scope_Entity"
        and then A /= "Value_Container"
        and then A /= "Annotation"
        and then A /= "First_Homonym_In_Namespace"
        and then A /= "Annotation_Node"
        and then A /= "Annotation_Info"
        and then A /= "Instances"
        and then A /= "Parent"
      then
         if K = "Node_Id" then
            W_Node_Id (Node_Id (N));
         elsif K = "List_Id" then
            W_List_Id (List_Id (N));
         end if;
      end if;

      N_Indents := N_Indents - 1;
   end W_Node_Attribute;

   -------------------
   -- W_Node_Header --
   -------------------

   procedure W_Node_Header (N : Node_Id) is
   begin
      W_Indents;
      Write_Int (Int (N));
      Write_Char (' ');
      Write_Str (Image (Kind (N)));
      Write_Char (' ');
      Write_Line (Image (Loc (N)));
   end W_Node_Header;

   ---------------
   -- W_Node_Id --
   ---------------

   procedure W_Node_Id (N : Node_Id) is
   begin
      if N = No_Node then
         return;
      end if;
      W_Node (N);
   end W_Node_Id;

   ---------
   -- wni --
   ---------

   procedure wni (N : Node_Id) is
      I : constant Integer := N_Indents;
   begin
      N_Indents := 1;
      W_Node_Id (N);
      N_Indents := I;
   end wni;

   ----------------------
   -- Dump_Annotations --
   ----------------------

   procedure Dump_Annotations (N : Node_Id; Recursive : Boolean) is
      package Dumped_Nodes is new GNAT.Table (Node_Id, Nat, 1, 10, 50);
      --  This is the list of the already dumped "nodes"

      function Is_Dumped (E : Node_Id) return Boolean;
      --  See whether E has already been ckecked or not

      procedure Set_Dumped (E : Node_Id);
      --  Append E to the dumped node table

      ---------------
      -- Is_Dumped --
      ---------------

      function Is_Dumped (E : Node_Id) return Boolean is
         use Dumped_Nodes;
      begin
         for J in First .. Last loop
            if Table (J) = E then
               return True;
            end if;
         end loop;

         return False;
      end Is_Dumped;

      ----------------
      -- Set_Dumped --
      ----------------

      procedure Set_Dumped (E : Node_Id) is
         use Dumped_Nodes;
      begin
         Append (E);
      end Set_Dumped;

      procedure Internal_Dump_Annotations (N : Node_Id);

      -------------------------------
      -- Internal_Dump_Annotations --
      -------------------------------

      procedure Internal_Dump_Annotations (N : Node_Id) is
         A : Annotation_Id;
      begin
         Set_Dumped (N);

         W_Node_Header (N);

         A := First_Annotation (N);

         while A /= No_Annotation loop
            W_Indents;
            Write_Line ("Name : '" & Image (Annotation_Name (A)) & "'");

            W_Indents;
            Write_Line ("Node :" & Node_Id'Image (Annotation_Node (A)));

            if Present (Annotation_Node (A))
              and then Recursive
              and then not Is_Dumped (Annotation_Node (A))
            then
               N_Indents := N_Indents + 1;
               Internal_Dump_Annotations (Annotation_Node (A));
               N_Indents := N_Indents - 1;
            end if;

            A := Next_Annotation (A);
         end loop;

      end Internal_Dump_Annotations;

      I : constant Integer := N_Indents;
   begin
      N_Indents := 1;
      Internal_Dump_Annotations (N);
      N_Indents := I;

      --  Deallocate the dumped nodes list

      Dumped_Nodes.Free;
   end Dump_Annotations;

end Ocarina.ME_AADL.AADL_Tree.Debug;
