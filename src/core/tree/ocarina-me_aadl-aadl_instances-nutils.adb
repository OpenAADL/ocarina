------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.ME_AADL.AADL_INSTANCES.NUTILS                   --
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

with Ocarina.Namet;
with Utils;

with Ocarina.ME_AADL.AADL_Instances.Entities;

package body Ocarina.ME_AADL.AADL_Instances.Nutils is

   use Ocarina.Namet;
   use Utils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

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

   -------------------
   -- To_Node_Array --
   -------------------

   function To_Node_Array (L : List_Id) return Node_Array is
      N : Node_Id;
      J : Natural := 1;
      Result : Node_Array (1 .. Length (L));
   begin
      if not Is_Empty (L) then
         N := First_Node (L);

         while Present (N) loop
            Result (J) := N;
            J := J + 1;
            N := Next_Node (N);
         end loop;
      end if;

      return Result;
   end To_Node_Array;

   --------------------
   -- Make_Container --
   --------------------

   function Make_Node_Container
     (Item       : Node_Id;
      Extra_Item : Node_Id := No_Node) return Node_Id
   is
      Container : constant Node_Id := New_Node (K_Node_Container, Loc (Item));
   begin
      Set_Item (Container, Item);
      Set_Extra_Item (Container, Extra_Item);

      return Container;
   end Make_Node_Container;

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

   -----------------------------------
   -- Compute_Full_Name_Of_Instance --
   -----------------------------------

   function Compute_Full_Name_Of_Instance
     (Instance         : Node_Id;
      Display_Name     : Boolean := False;
      Keep_Root_System : Boolean := True) return Name_Id
   is
      pragma Assert
        (Kind (Instance) = K_Component_Instance
         or else Kind (Instance) = K_Subcomponent_Instance
         or else Kind (Instance) = K_Namespace_Instance
         or else Kind (Instance) = K_Connection_Instance
         or else Kind (Instance) = K_Port_Spec_Instance
         or else Kind (Instance) = K_Parameter_Instance
         or else Kind (Instance) = K_Call_Sequence_Instance
         or else Kind (Instance) = K_Call_Instance);

      Parent_Name : Name_Id := No_Name;
      Entity_Name : Name_Id := No_Name;
      Full_Name   : Name_Id := No_Name;
      L           : List_Id;
      N           : Node_Id;
   begin
      --  A full name is the concatenation of the names of the
      --  subdeclarations (subcomponents, connections, etc.) from the
      --  top level system. The name of this system is also part of
      --  the full name.

      case Kind (Instance) is
         when K_Component_Instance =>
            if Get_Category_Of_Component (Instance) = CC_Subprogram then
               L := Split_Name (Namespace (Instance));

               Set_Str_To_Name_Buffer ("");

               if not Is_Empty (L) then
                  N := First_Node (L);

                  while Present (N) loop
                     if Display_Name then
                        Get_Name_String_And_Append
                          (Ocarina.ME_AADL.AADL_Instances.Nodes.Display_Name
                             (N));
                     else
                        Get_Name_String_And_Append (Name (N));
                     end if;

                     Add_Char_To_Name_Buffer ('_');

                     N := Next_Node (N);
                  end loop;
               end if;

               Get_Name_String_And_Append
                 (Get_Name_Of_Entity (Instance, Display_Name));
               Full_Name := Name_Find;

            elsif No (Parent_Subcomponent (Instance)) then
               --  These two cases we return only the name of the
               --  entity:

               --  1 - If we cannot go upper in the instance tree, we
               --  get the instance name.

               --  2 - If we deal with a subprogram.

               --  FIXME: This needs more effort since a subprogram
               --  name should be Namespace1_Namespace2_..._Spg.

               Full_Name := Get_Name_Of_Entity (Instance, Display_Name);

            elsif Get_Category_Of_Component
                (Parent_Component (Parent_Subcomponent (Instance))) =
              CC_System
              and then not Keep_Root_System
            then
               --  If there is a corresponding subcomponent but its
               --  parent is a system, we get the name of the
               --  subcomponent unless the user wanted to go upper in
               --  the instance tree

               Full_Name :=
                 Get_Name_Of_Entity
                   (Parent_Subcomponent (Instance),
                    Display_Name);
            else
               --  General case, we go upper in the instance tree

               Full_Name :=
                 Compute_Full_Name_Of_Instance
                   (Parent_Subcomponent (Instance),
                    Display_Name,
                    Keep_Root_System);
            end if;

         when K_Call_Instance =>
            Parent_Name :=
              Compute_Full_Name_Of_Instance
                (Parent_Sequence (Instance),
                 Display_Name,
                 Keep_Root_System);
            Get_Name_String (Parent_Name);
            Add_Str_To_Name_Buffer ("_");
            Get_Name_String_And_Append
              (Get_Name_Of_Entity (Instance, Display_Name));
            Full_Name := Name_Find;

         when others =>
            Parent_Name :=
              Compute_Full_Name_Of_Instance
                (Parent_Component (Instance),
                 Display_Name,
                 Keep_Root_System);
            Get_Name_String (Parent_Name);
            Entity_Name := Get_Name_Of_Entity (Instance, Display_Name);

            if Entity_Name /= No_Name then
               Add_Str_To_Name_Buffer ("_");
               Get_Name_String_And_Append (Entity_Name);
            end if;

            Full_Name := Name_Find;
      end case;

      return Full_Name;
   end Compute_Full_Name_Of_Instance;

   ----------------
   -- Split_Name --
   ----------------

   function Split_Name (N : Node_Id) return List_Id is
      Name_List : List_Id;
      D_Name    : Name_Id;
      L_Name    : Name_Id;
   begin
      if No (Identifier (N))
        or else Display_Name (Identifier (N)) = No_Name
      then
         Name_List := No_List;
      else
         Get_Name_String (Display_Name (Identifier (N)));

         --  To get uniform case handling of the separation between
         --  names and the end of the full name

         Add_Char_To_Name_Buffer (':');

         declare
            Package_Name : constant String :=
              Name_Buffer (Name_Buffer'First .. Name_Len);
            Lower_Index, Upper_Index : Natural := Package_Name'First;
            Identifier               : Node_Id;
         begin
            Name_List := New_List (K_List_Id, No_Location);

            while Upper_Index <= Package_Name'Last loop

               --  There are two kinds of package names:
               --  1 - "Name1::Name2::.." names for which a list containing
               --      all the names is built
               --  2 - "One_Single_Name" names for which a single element
               --      list is built

               if Name_Buffer (Upper_Index) = ':' then
                  Set_Str_To_Name_Buffer
                    (Package_Name (Lower_Index .. Upper_Index - 1));
                  D_Name := Name_Find;
                  L_Name := To_Lower (D_Name);

                  Identifier :=
                    Make_Identifier (No_Location, L_Name, D_Name, No_Node);
                  Append_Node_To_List (Identifier, Name_List);

                  --  skip the second ':'

                  Upper_Index := Upper_Index + 1;

                  --  Point to the beginning of the next name

                  Lower_Index := Upper_Index + 1;
               end if;

               Upper_Index := Upper_Index + 1;
            end loop;
         end;
      end if;

      return Name_List;
   end Split_Name;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (Original : Node_Id) return Node_Id is
      Copy : Node_Id;
   begin
      if Kind (Original) = K_Identifier then
         Copy := New_Node (K_Identifier, AIN.Loc (Original));
         Set_Name (Copy, AIN.Name (Original));
         Set_Display_Name (Copy, AIN.Display_Name (Original));
         Set_Corresponding_Entity (Copy, AIN.Corresponding_Entity (Original));
         return Copy;
      end if;

      return No_Node;
   end Copy_Node;

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

   -----------------------
   -- Find_Name_In_List --
   -----------------------

   function Find_Name_In_List
     (Name_Node : Name_Id;
      List      : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      if not Is_Empty (List) then
         N := First_Node (List);

         while Present (N) loop
            if Identifier (N) /= No_Node
              and then Name (Identifier (N)) = Name_Node
            then
               return N;
            end if;
            N := Next_Node (N);
         end loop;
      end if;
      return No_Node;
   end Find_Name_In_List;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Abstract;
   end Is_Abstract;

   -------------
   -- Is_Data --
   -------------

   function Is_Data (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Data;
   end Is_Data;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Subprogram;
   end Is_Subprogram;

   ----------------
   -- Is_Process --
   ----------------

   function Is_Process (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Process;
   end Is_Process;

   ---------------
   -- Is_Device --
   ---------------

   function Is_Device (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Device;
   end Is_Device;

   --------------------------
   -- Is_Process_Or_Device --
   --------------------------

   function Is_Process_Or_Device (C : Node_Id) return Boolean is
   begin
      return Is_Process (C) or else Is_Device (C);
   end Is_Process_Or_Device;

   ---------------
   -- Is_Thread --
   ---------------

   function Is_Thread (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Thread;
   end Is_Thread;

   ---------------
   -- Is_Memory --
   ---------------

   function Is_Memory (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Memory;
   end Is_Memory;

   ---------------
   -- Is_System --
   ---------------

   function Is_System (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_System;
   end Is_System;

   ------------------
   -- Is_Processor --
   ------------------

   function Is_Processor (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Processor;
   end Is_Processor;

   --------------------------
   -- Is_Virtual_Processor --
   --------------------------

   function Is_Virtual_Processor (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Virtual_Processor;
   end Is_Virtual_Processor;

   --------------------------
   -- Is_Subprogram_Access --
   --------------------------

   function Is_Subprogram_Access (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Subcomponent_Access_Instance
        and then
          Get_Category_Of_Component (Corresponding_Instance (C)) =
          CC_Subprogram;
   end Is_Subprogram_Access;

   ------------
   -- Is_Bus --
   ------------

   function Is_Bus (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Bus;
   end Is_Bus;

   --------------------
   -- Is_Virtual_Bus --
   --------------------

   function Is_Virtual_Bus (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Component_Instance
        and then Get_Category_Of_Component (C) = CC_Virtual_Bus;
   end Is_Virtual_Bus;

   -------------
   -- Is_Port --
   -------------

   function Is_Port (C : Node_Id) return Boolean is
   begin
      return Kind (C) = K_Port_Spec_Instance;
   end Is_Port;

end Ocarina.ME_AADL.AADL_Instances.Nutils;
