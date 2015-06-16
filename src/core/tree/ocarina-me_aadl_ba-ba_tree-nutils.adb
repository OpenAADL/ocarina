------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . M E _ A A D L _ B A . B A _ T R E E . N U T I L S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

with Ocarina.Namet;

package body Ocarina.ME_AADL_BA.BA_Tree.Nutils is

   use Ocarina.Namet;

   -------------------
   -- First_Homonym --
   -------------------

   function First_Homonym (N : Node_Id) return Node_Id is
      HN : constant Name_Id := Name (N);
   begin
      return Node_Id (Get_Name_Table_Info (HN));
   end First_Homonym;

   -----------------------
   -- Set_First_Homonym --
   -----------------------

   procedure Set_First_Homonym (N : Node_Id; V : Node_Id) is
   begin
      Set_Name_Table_Info (Name (N), Int (V));
   end Set_First_Homonym;

   -----------------------
   -- Push_Node_To_List --
   -----------------------

   procedure Push_Node_To_List (E : Node_Id; L : List_Id) is
      First_L : constant Node_Id := First_Node (L);
      Last_E  : Node_Id;  --  the last element of E
      Next_E  : Node_Id;
   begin
      Set_First_Node (L, E);

      Last_E := E;
      loop
         Next_E := Next_Node (Last_E);
         exit when No (Next_E);
         Last_E := Next_E;
      end loop;

      if No (First_L) then
         --  list is empty

         Set_Last_Node (L, Last_E);
      else
         Set_Next_Node (Last_E, First_L);
      end if;
   end Push_Node_To_List;

   --------------------------
   -- Replace_Node_To_List --
   --------------------------

   procedure Replace_Node_To_List
     (List     : List_Id;
      Old_Node : Node_Id;
      New_Node : Node_Id)
   is
      Node : Node_Id;
      Next : Node_Id := No_Node;
   begin
      if Old_Node = First_Node (List) then
         if Present (Next_Node (First_Node (List))) then
            Set_Next_Node (New_Node, Next_Node (First_Node (List)));
         end if;
         Set_First_Node (List, New_Node);
      else
         Node := First_Node (List);
         Next := Next_Node (Node);

         while Present (Next) loop
            if Next = Old_Node then
               Set_Next_Node (Node, New_Node);
               Set_Next_Node (New_Node, Next_Node (Next));
            end if;

            Node := Next_Node (Node);
            Next := Next_Node (Node);
         end loop;
      end if;
   end Replace_Node_To_List;

   -------------------------
   -- Append_List_To_List --
   -------------------------

   procedure Append_List_To_List (S : List_Id; D : in out List_Id) is
   begin
      if Present (D) then
         Append_Node_To_List (First_Node (S), D);
      else
         --  This is highly dangerous. Append should be a copy
         --  operation.

         D := S;
      end if;
   end Append_List_To_List;

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;
   begin
      Last := Last_Node (L);
      if No (Last) then
         Set_First_Node (L, E);
      else
         Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         Set_Last_Node (L, Last);
         Last := Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   ------------------------------
   -- Append_Node_To_Node_List --
   ------------------------------

   procedure Append_Node_To_Node_List
     (Node :        Node_Id;
      List : in out Node_List;
      Once :        Boolean := True)
   is
      pragma Assert (Present (Node));

      Current : Node_Id;
   begin
      if List.First = No_Node then
         List.First := Node;
         List.Last  := Node;
         Set_Next_Entity (Node, No_Node);

      else
         Current := List.First;
         while Present (Current) loop
            --  Do not append when Node already there in List

            if Once and then Current = Node then
               return;
            end if;
            Current := Next_Entity (Current);
         end loop;

         Set_Next_Entity (List.Last, Node);
         Set_Next_Entity (Node, No_Node);
         List.Last := Node;
      end if;
   end Append_Node_To_Node_List;

   ----------------------------
   -- Remove_Nodes_From_List --
   ----------------------------

   procedure Remove_Nodes_From_List (List : in out Node_List) is
      Node : Node_Id;
      Next : Node_Id;
   begin
      if Present (List.First) then
         Node := List.First;
         while Present (Node) loop
            Next := Next_Entity (Node);
            Set_Next_Entity (Node, No_Node);
            Node := Next;
         end loop;
         List.First := No_Node;
         List.Last  := No_Node;
      end if;
   end Remove_Nodes_From_List;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List_Id) return Natural is
      N : Node_Id;
      C : Natural := 0;
   begin
      if not Is_Empty (L) then
         N := First_Node (L);

         while Present (N) loop
            C := C + 1;
            N := Next_Node (N);
         end loop;
      end if;

      return C;
   end Length;

   ---------------------
   -- Make_Identifier --
   ---------------------

   function Make_Identifier
     (Loc          : Location;
      Name         : Name_Id;
      Display_Name : Name_Id;
      Entity       : Node_Id) return Node_Id
   is
      Node : constant Node_Id := New_Node (K_Identifier, Loc);
   begin
      Set_Name (Node, Name);
      Set_Display_Name (Node, Display_Name);
      Set_Corresponding_Entity (Node, Entity);

      return Node;
   end Make_Identifier;

   --------------
   -- New_List --
   --------------

   function New_List (Kind : Node_Kind; Loc : Location) return List_Id is
   begin
      return List_Id (New_Node (Kind, Loc));
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N                 := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      Set_Loc (N, Loc);

      return N;
   end New_Node;

   -----------------
   -- Reset_Nodes --
   -----------------

   procedure Reset_Nodes is
   begin
      Entries.Init;
   end Reset_Nodes;

   --------------------------------
   -- Remove_Last_Node_From_List --
   --------------------------------

   function Remove_Last_Node_From_List (L : List_Id) return Node_Id is
      Previous : Node_Id;
      Current  : Node_Id;
      Next     : Node_Id;
   begin
      if No (L) then           --  invalid list
         return No_Node;
      end if;

      Previous := First_Node (L);

      if No (Previous) then    --  list is empty
         return No_Node;
      end if;

      Current := Next_Node (Previous);

      if No (Current) then     --  list contains only one element
         Set_First_Node (L, No_Node);   --  erase L first node
         Set_Last_Node (L, No_Node);
         return Previous;
      end if;

      loop
         Next := Next_Node (Current);
         exit when No (Next);
         Previous := Current;
         Current  := Next;
      end loop;

      Set_Next_Node (Previous, No_Node);
      Set_Last_Node (L, Previous);

      return Current;
   end Remove_Last_Node_From_List;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;
   begin
      C := First_Node (L);

      if C = E then
         Set_First_Node (L, Next_Node (E));

         if Last_Node (L) = E then
            Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if Next_Node (C) = E then
               Set_Next_Node (C, Next_Node (E));

               if Last_Node (L) = E then
                  Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

end Ocarina.ME_AADL_BA.BA_Tree.Nutils;
