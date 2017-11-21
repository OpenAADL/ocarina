------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  O C A R I N A . A N N O T A T I O N S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2007-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Annotations is

   Prefix : constant String := "annotation";

   type Annotation_Record is record
      Node : Node_Id;
      Name : Name_Id;
      Info : Node_Id;
      Next : Annotation_Id;
   end record;

   package Annotation_Table is new GNAT.Table
     (Annotation_Record,
      Annotation_Id,
      1,
      10,
      10);
   use Annotation_Table;

   function Internal_Name (N : Node_Id) return Name_Id;
   --  Use name table to get a unique name id for N

   procedure Append_To (N : Node_Id; A : Annotation_Record);
   --  Get to the end of the annotation list and add A. Create this
   --  list when none already exists.

   -------------------
   -- Internal_Name --
   -------------------

   function Internal_Name (N : Node_Id) return Name_Id is
   begin
      Set_Str_To_Name_Buffer (Prefix);
      Add_Str_To_Name_Buffer (N'Img);
      return Name_Find;
   end Internal_Name;

   ---------------
   -- Append_To --
   ---------------

   procedure Append_To (N : Node_Id; A : Annotation_Record) is
      Index : Annotation_Id := First_Annotation (N);

   begin
      Increment_Last;
      Table (Last) := A;

      --  No list available

      if Index = 0 then
         Set_Name_Table_Info (Internal_Name (N), Int (Last));

      else
         --  Reach end of list then append

         while Table (Index).Next /= 0 loop
            Index := Table (Index).Next;
         end loop;
         Table (Index).Next := Last;
      end if;
   end Append_To;

   ----------------------
   -- First_Annotation --
   ----------------------

   function First_Annotation (N : Node_Id) return Annotation_Id is
   begin
      return Annotation_Id (Get_Name_Table_Info (Internal_Name (N)));
   end First_Annotation;

   ----------------------
   -- Annotation_Index --
   ----------------------

   function Annotation_Index (N : Node_Id; A : Node_Id) return Annotation_Id is
      Index : Annotation_Id := First_Annotation (N);

   begin
      while Index /= 0 loop
         exit when Table (Index).Node = A;
         Index := Table (Index).Next;
      end loop;

      return Index;
   end Annotation_Index;

   ----------------------
   -- Annotation_Index --
   ----------------------

   function Annotation_Index (N : Node_Id; A : Name_Id) return Annotation_Id is
      Index : Annotation_Id := First_Annotation (N);

   begin
      while Index /= 0 loop
         exit when Table (Index).Name = A;
         Index := Table (Index).Next;
      end loop;

      return Index;
   end Annotation_Index;

   --------------
   -- Annotate --
   --------------

   procedure Annotate (N : Node_Id; A : Node_Id; I : Node_Id := No_Node) is
      Index : constant Annotation_Id := Annotation_Index (N, A);

   begin
      if Index /= 0 then
         Table (Index).Info := I;

      else
         Append_To (N, (Node => A, Name => No_Name, Info => I, Next => 0));
      end if;
   end Annotate;

   --------------
   -- Annotate --
   --------------

   procedure Annotate (N : Node_Id; A : Name_Id; I : Node_Id := No_Node) is
      Index : constant Annotation_Id := Annotation_Index (N, A);

   begin
      if Index /= 0 then
         Table (Index).Info := I;

      else
         Append_To (N, (Node => No_Node, Name => A, Info => I, Next => 0));
      end if;
   end Annotate;

   ---------------------
   -- Annotation_Info --
   ---------------------

   function Annotation_Info (N : Node_Id; A : Node_Id) return Node_Id is
      Index : constant Annotation_Id := Annotation_Index (N, A);

   begin
      if Index = 0 then
         raise Program_Error with "Annotation item not found";
      end if;

      return Table (Index).Info;
   end Annotation_Info;

   ---------------------
   -- Annotation_Info --
   ---------------------

   function Annotation_Info (N : Node_Id; A : Name_Id) return Node_Id is
      Index : constant Annotation_Id := Annotation_Index (N, A);

   begin
      if Index = 0 then
         raise Program_Error with "Annotation item not found";
      end if;

      return Table (Index).Info;
   end Annotation_Info;

   ---------------------
   -- Annotation_Node --
   ---------------------

   function Annotation_Node (I : Annotation_Id) return Node_Id is
   begin
      return Table (I).Node;
   end Annotation_Node;

   ---------------------
   -- Annotation_Name --
   ---------------------

   function Annotation_Name (I : Annotation_Id) return Name_Id is
   begin
      return Table (I).Name;
   end Annotation_Name;

   ---------------------
   -- Next_Annotation --
   ---------------------

   function Next_Annotation (I : Annotation_Id) return Annotation_Id is
   begin
      return Table (I).Next;
   end Next_Annotation;

end Ocarina.Annotations;
