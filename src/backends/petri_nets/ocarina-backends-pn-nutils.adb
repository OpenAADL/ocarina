------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . P N . N U T I L S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

package body Ocarina.Backends.PN.Nutils is

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Types.Node_Id; L : Types.List_Id) is
      use OPN;

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

   ---------------------------
   --  Push_Node_Into_List  --
   ---------------------------

   procedure Push_Node_Into_List (E : Types.Node_Id; L : Types.List_Id) is
      use OPN;

      First : Node_Id;
   begin
      if Is_Empty (L) then
         Append_Node_To_List (E, L);
      else
         First := First_Node (L);
         Set_First_Node (L, E);
         Set_Next_Node (E, First);

         if Next_Node (First) = No_Node then
            Set_Last_Node (L, First);
         end if;
      end if;
   end Push_Node_Into_List;

   ---------------------------
   -- Delete_Node_From_List --
   ---------------------------

   procedure Delete_Node_From_List (E : Types.Node_Id; L : Types.List_Id) is
      use OPN;

      Node_Iter : Node_Id;
   begin
      if E = First_Node (L) then
         Set_First_Node (L, Next_Node (E));
         if Next_Node (E) = No_Node then
            Set_Last_Node (L, No_Node);
         end if;
      else
         Node_Iter := First_Node (L);
         while Present (Node_Iter) loop
            if Next_Node (Node_Iter) = E then
               if Next_Node (E) /= No_Node then
                  Set_Next_Node (Node_Iter, Next_Node (E));
               else
                  Set_Next_Node (Node_Iter, No_Node);
                  Set_Last_Node (L, Node_Iter);
               end if;
            end if;

            Node_Iter := Next_Node (Node_Iter);
         end loop;
      end if;
   end Delete_Node_From_List;

   --------------
   -- New_List --
   --------------

   function New_List (Kind : OPN.Node_Kind) return Types.List_Id is
   begin
      return List_Id (New_Node (Kind));
   end New_List;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : Types.List_Id) return Boolean is
      use OPN;

   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   --------------
   -- New_Node --
   --------------

   function New_Node (Kind : OPN.Node_Kind) return Types.Node_Id is
      use OPN;

      N : Node_Id;
   begin
      Entries.Increment_Last;
      N                 := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);

      return N;
   end New_Node;

   ---------------------
   -- Make_Identifier --
   ---------------------

   function Make_Identifier
     (Pn_Entity  : Types.Node_Id;
      Ident_Name : Types.Name_Id) return Types.Node_Id
   is
      use OPN;

      Identify : constant Node_Id := New_Node (K_Identifier);
   begin
      Set_Corresponding_Entity (Identify, Pn_Entity);
      Set_Name (Identify, Ident_Name);
      Set_Ocarina_Node (Identify, No_Node);
      return Identify;
   end Make_Identifier;

end Ocarina.Backends.PN.Nutils;
