------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . M E _ R E A L . R E A L _ T R E E . N U T I L S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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

with Utils;

package body Ocarina.ME_REAL.REAL_Tree.Nutils is

   Global_Variables : List_Id;
   --  Variables visible from the current scope

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      REAL_Root := No_Node;
      AADL_Model_Root := No_Node;
      Owner_Node := No_Node;
      Ocarina.ME_REAL.REAL_Tree.Nutils.Domain :=
        Ocarina.Instances.REAL_Checker.Queries.Empty_Set;
      Environment := No_List;
      Global_Variables := New_List (K_List_Id, No_Location);
      Node_List.Init (Library_Theorems);
      Node_List.Init (To_Run_Theorem_List);
      Is_Domain := False;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Node_List.Free (Library_Theorems);
      Node_List.Free (To_Run_Theorem_List);
      Init;
   end Reset;

   ---------------------------
   -- Find_Global_Variables --
   ---------------------------

   function Find_Global_Variable (Key : Name_Id) return Node_Id
   is
   begin
      return Find_Node_By_Name (Key, Global_Variables);
   end Find_Global_Variable;

   -----------------------------------------
   -- Append_Variable_To_Global_Variables --
   -----------------------------------------

   procedure Append_Variable_To_Global_Variables (Node : Node_Id)
   is
   begin
      Append_Node_To_List (Node, Global_Variables);
   end Append_Variable_To_Global_Variables;

   ---------------------------
   -- Find_Declared_Theorem --
   ---------------------------

   function Find_Declared_Theorem (Theorem_Name : Name_Id) return Node_Id
   is
      use Node_List;
      use Utils;
   begin
      for I in Node_List.First .. Node_List.Last (Library_Theorems) loop
         if To_Lower (Name (Identifier (Library_Theorems.Table (I).Node))) =
           To_Lower (Theorem_Name) then
            return Library_Theorems.Table (I).Node;
         end if;
      end loop;

      return No_Node;
   end Find_Declared_Theorem;

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

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   --------------
   -- New_List --
   --------------

   function New_List (Kind : Node_Kind; Loc : Location) return List_Id
   is
   begin
      return List_Id (New_Node (Kind, Loc));
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id
   is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      Set_Loc  (N, Loc);
      return N;
   end New_Node;

   --------------------------
   -- Replace_Node_To_List --
   --------------------------

   procedure Replace_Node_To_List
     (List     : List_Id;
      Old_Node : Node_Id;
      New_Node : Node_Id)
   is
      Node  : Node_Id;
      Next  : Node_Id := No_Node;
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

   -----------------------
   -- Find_Node_By_Name --
   -----------------------

   function Find_Node_By_Name (Key : Name_Id; Target : List_Id) return Node_Id
   is
      N  : Node_Id;
   begin
      if Is_Empty (Target) then
         return No_Node;
      end if;

      N := First_Node (Target);

      while Present (N) loop
         if Name (Identifier (N)) = Key then
            return N;
         end if;
         N := Next_Node (N);
      end loop;

      return No_Node;
   end Find_Node_By_Name;

end Ocarina.ME_REAL.REAL_Tree.Nutils;
