------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . R E A L _ E X P A N D E R . F L O W _ A N A L Y S I S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2018 ESA & ISAE.        --
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

with Locations; use Locations;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Instances.Queries;
with GNAT.Dynamic_Tables;
with Ocarina.Namet;

package body Ocarina.REAL_Expander.Flow_Analysis is
   use Ocarina.Namet;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL;
   use Ocarina.Instances.Queries;

   type Flow_Element is record
      Node : Node_Id;
   end record;

   type Single_Flow is record
      List : List_Id;
   end record;

   package Flow_List is new GNAT.Dynamic_Tables
     (Table_Component_Type => Flow_Element,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,  -- # of elements
      Table_Increment      => 50); -- % increase

   package Flow is new GNAT.Dynamic_Tables
     (Table_Component_Type => Single_Flow,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,  -- # of elements
      Table_Increment      => 50); -- % increase

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;

   Flow_Lists       : Flow.Instance;
   End_To_End_Flows : Flow.Instance;

   procedure Build_Flows (Branch : List_Id; E : Node_Id);
   --  From a node E, explore the following flow
   --  * Whenever it finds a cycle or a sink, remove Branch from
   --    the global Flow_List, and add it to End_To_End_Flows
   --  * Whenever it finds a separation, create a copy of the branch
   --    and add it to the global Flow_List

   function Detect_Cycle (Branch : List_Id; E : Node_Id) return Boolean;
   --  Return true whenever E is already present in Branch

   function Bound_Bus (Src : Node_Id; Dst : Node_Id) return Node_Id;
   --  Return a bus if one is bound to the connection defined by
   --  Src -> Dst, return No_Node otherwise.

   -------------------
   -- Explore_Flows --
   -------------------

   procedure Explore_Flows is
      use AIN;
      use Ocarina.ME_AADL.AADL_Instances.Nutils;

      C                  : Node_Id;
      L                  : List_Id;
      Confirmed_Flow_Src : Flow_List.Instance;
   begin
      Flow.Init (End_To_End_Flows);
      Flow.Init (Flow_Lists);
      Flow_List.Init (Confirmed_Flow_Src);

      --  First we must find the sources.

      --  Source definition is a thread with at least one
      --  output and no input.

      --  FIXME :
      --  * check definition => should be subprograms instead of threads
      --                     => devices ?
      --  * source granularity
      --  * add data access

      for N in 1 .. Entries.Last loop
         if Kind (N) = K_Component_Instance
           and then Get_Category_Of_Component (N) = CC_Thread
         then
            declare
               F             : Node_Id;
               Found_Input   : Boolean := False;
               Temp_Flow_Src : Flow_List.Instance;
            begin
               Flow_List.Init (Temp_Flow_Src);

               if not Is_Empty (Features (N)) then
                  F := First_Node (Features (N));
                  while Present (F) loop

                     --  Search for predecessor threads

                     C := First_Node (AIN.Sources (F));
                     while Present (C) and then not Found_Input loop
                        Found_Input :=
                          (Kind (Item (C)) = K_Port_Spec_Instance);
                        C := Next_Node (C);
                     end loop;

                     --  Search for successor threads
                     --  (potential flows)

                     C := First_Node (AIN.Destinations (F));
                     while Present (C) loop
                        if Kind (Item (C)) = K_Port_Spec_Instance then
                           declare
                              FE : Flow_Element;
                           begin
                              FE.Node := F;
                              Flow_List.Append (Temp_Flow_Src, FE);
                           end;
                        end if;
                        C := Next_Node (C);
                     end loop;

                     exit when Found_Input;
                     F := Next_Node (F);
                  end loop;

                  --  If the thread is a source,
                  --  we create the corresponding flow list
                  --  and add the component in the Confirmed_Sources list

                  if not Found_Input then
                     declare
                        use Flow_List;

                        FE : Flow_Element;
                     begin
                        for I in First .. Last (Temp_Flow_Src) loop
                           FE.Node := Temp_Flow_Src.Table (I).Node;
                           Append (Confirmed_Flow_Src, FE);
                        end loop;
                     end;
                  end if;
               end if;

               Flow_List.Free (Temp_Flow_Src);
            end;
         end if;
      end loop;

      --  Then we explore each flow list

      declare
         use Flow_List;

         SF : Single_Flow;
         Id : Node_Id;
      begin
         for I in First .. Last (Confirmed_Flow_Src) loop
            L       := New_List (K_List_Id, No_Location);
            SF.List := L;
            Flow.Append (Flow_Lists, SF);
            Id :=
              Make_Identifier
                (No_Location,
                 Name (Identifier (Confirmed_Flow_Src.Table (I).Node)),
                 Name (Identifier (Confirmed_Flow_Src.Table (I).Node)),
                 Confirmed_Flow_Src.Table (I).Node);
            Build_Flows (Branch => L, E => Id);
         end loop;
      end;
   end Explore_Flows;

   -----------------
   -- Build_Flows --
   -----------------

   procedure Build_Flows (Branch : List_Id; E : Node_Id) is
      use Ocarina.ME_AADL.AADL_Instances.Nodes;
      use Ocarina.ME_AADL.AADL_Instances.Nutils;
      use Flow;

      pragma Assert
        (Kind (Corresponding_Entity (E)) = K_Port_Spec_Instance
         or else Kind (Corresponding_Entity (E)) = K_Parameter_Instance);

      Successors : Flow_List.Instance;
      S          : Node_Id;
      Id         : Node_Id;
      Bus        : Node_Id;
      NB_Output  : Natural := 0;
      SF         : Single_Flow;
      FE         : Flow_Element;
   begin
      Flow_List.Init (Successors);

      --  If E is already present in Branch, then we just found a cycle.
      --  No need to continue analysis of this branch, since this node has
      --  already been analyzed.

      if Detect_Cycle (Branch, Corresponding_Entity (E)) then

         --  Remove branch from the list of branch to be explored

         for I in First .. Last (Flow_Lists) loop
            if Flow_Lists.Table (I).List = Branch then
               SF.List := No_List;
               Set_Item (Flow_Lists, I, SF);
               exit;
            end if;
         end loop;

         --  Add branch to the list of fully explored branches

         SF.List := Branch;
         Append (End_To_End_Flows, SF);

         return;
      end if;

      --  FIXME :
      --  send flow data to parent components

      --  A first parsing of the node's features will give us
      --  all eligible successors

      if not Is_Empty (Destinations (Corresponding_Entity (E))) then
         S := First_Node (Destinations (Corresponding_Entity (E)));
         while Present (S) loop
            if Kind (Item (S)) = K_Parameter_Instance then
               Id :=
                 Make_Identifier
                   (No_Location,
                    Name (Identifier (Item (S))),
                    Name (Identifier (Item (S))),
                    Item (S));
               FE.Node := Id;
               Flow_List.Append (Successors, FE);
               NB_Output := NB_Output + 1;
            end if;

            S := Next_Node (S);
         end loop;

         if NB_Output = 0 then
            S := First_Node (Destinations (Corresponding_Entity (E)));
            while Present (S) loop
               if Kind (Item (S)) = K_Port_Spec_Instance then
                  Id :=
                    Make_Identifier
                      (No_Location,
                       Name (Identifier (Item (S))),
                       Name (Identifier (Item (S))),
                       Item (S));
                  FE.Node := Id;
                  Flow_List.Append (Successors, FE);
                  NB_Output := NB_Output + 1;
               end if;
               S := Next_Node (S);
            end loop;
         end if;
      end if;

      Id :=
        Make_Identifier
          (No_Location,
           Name (E),
           Name (E),
           Corresponding_Entity (E));
      Append_Node_To_List (Id, Branch);

      --  In leaf component (ie terminal subprograms),
      --  we must 'guess' the following node of the flow path.
      --  Our policy is to consider that any leaving flow
      --  is dependant of the incoming flows.

      if NB_Output = 0
        and then not Is_Empty
          (Features (Parent_Component (Corresponding_Entity (E))))
      then

         --  We search all potential successors
         declare
            F : Node_Id :=
              First_Node
                (Features (Parent_Component (Corresponding_Entity (E))));
         begin
            --  We add all accessed subcomponents to the flow

            while Present (F) loop
               if Kind (F) = K_Subcomponent_Access_Instance then
                  Id :=
                    Make_Identifier
                      (No_Location,
                       Name (Identifier (F)),
                       Name (Identifier (F)),
                       F);
                  Append_Node_To_List (Id, Branch);
               end if;
               F := Next_Node (F);
            end loop;

            --  We build a list of successors
            --  begining by subprograms parameters,
            --  and only then exploring out features

            F :=
              First_Node
                (Features (Parent_Component (Corresponding_Entity (E))));
            while Present (F) loop
               if Kind (F) = K_Parameter_Instance
                 and then F /= Corresponding_Entity (E)
                 and then Is_Out (F)
               then
                  Id :=
                    Make_Identifier
                      (No_Location,
                       Name (Identifier (F)),
                       Name (Identifier (F)),
                       F);
                  FE.Node := Id;
                  Flow_List.Append (Successors, FE);
                  NB_Output := NB_Output + 1;
               end if;
               F := Next_Node (F);
            end loop;

            if NB_Output = 0
              and then
                Get_Category_Of_Component
                  (Parent_Component (Corresponding_Entity (E))) =
                CC_Thread
            then
               F :=
                 First_Node
                   (Features (Parent_Component (Corresponding_Entity (E))));
               while Present (F) loop
                  if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then
                     Id :=
                       Make_Identifier
                         (No_Location,
                          Name (Identifier (F)),
                          Name (Identifier (F)),
                          F);
                     FE.Node := Id;
                     Flow_List.Append (Successors, FE);
                     NB_Output := NB_Output + 1;
                  end if;
                  F := Next_Node (F);
               end loop;
            end if;

            if NB_Output = 0 then

               --  Remove branch from the list of branch to be explored
               for I in First .. Last (Flow_Lists) loop
                  if Flow_Lists.Table (I).List = Branch then
                     SF.List := No_List;
                     Set_Item (Flow_Lists, I, SF);
                     exit;
                  end if;
               end loop;

               --  Add branch to the list of fully explored branches
               SF.List := Branch;
               Append (End_To_End_Flows, SF);
               return;
            end if;
         end;
      end if;

      --  We continue the exploration

      if NB_Output = 1 then
         --  Check if the connection from E to S is bound to a bus

         if Kind (Corresponding_Entity (Successors.Table (First).Node)) =
           K_Port_Spec_Instance
         then
            Bus :=
              Bound_Bus
                (Corresponding_Entity (E),
                 Corresponding_Entity (Successors.Table (First).Node));
            if Present (Bus) then
               Id :=
                 Make_Identifier
                   (No_Location,
                    Name (Identifier (Bus)),
                    Name (Identifier (Bus)),
                    Bus);
               Append_Node_To_List (Id, Branch);
            end if;
         end if;

         Build_Flows (Branch => Branch, E => Successors.Table (First).Node);
         Flow_List.Free (Successors);
         return;
      end if;

      --  If NB_Output = N, with N > 0
      --  We duplicate N times Branch and add those clones to
      --  the Flows_List

      declare
         TFL : List_Id;
      begin
         Build_Flows (Branch => Branch, E => Successors.Table (First).Node);

         for I in (First + 1) .. NB_Output loop
            TFL := New_List (K_List_Id, No_Location);

            --  create a clone of the branch after the first one

            S := First_Node (Branch);
            while Present (S) loop
               Id :=
                 Make_Identifier
                   (No_Location,
                    Name (S),
                    Name (S),
                    Corresponding_Entity (S));
               Append_Node_To_List (Id, TFL);
               S := Next_Node (S);
            end loop;

            SF.List := TFL;
            Append (Flow_Lists, SF);

            --  Check if the connection from E to S is bound to a bus

            if Kind (Successors.Table (I).Node) = K_Port_Spec_Instance then

               Bus :=
                 Bound_Bus
                   (Corresponding_Entity (E),
                    Corresponding_Entity (Successors.Table (I).Node));
               if Present (Bus) then
                  Id :=
                    Make_Identifier
                      (No_Location,
                       Name (Identifier (Bus)),
                       Name (Identifier (Bus)),
                       Bus);
                  Append_Node_To_List (Id, TFL);
               end if;
            end if;

            Build_Flows (Branch => TFL, E => Successors.Table (I).Node);
         end loop;
         Flow_List.Free (Successors);
      end;
   end Build_Flows;

   -----------------
   -- Store_Flows --
   -----------------

   procedure Store_Flows (Instance : Node_Id) is
      use Ocarina.ME_AADL.AADL_Instances.Nodes;
      use Ocarina.ME_AADL.AADL_Instances.Nutils;
      use Flow;

      R : constant Node_Id := Root_System (Instance);
      P : Node_Id;

   begin
      if ATNU.Is_Empty (Flows (R)) then
         Set_Flows (R, ATNU.New_List (ATN.K_List_Id, No_Location));
      end if;

      for I in First .. Last (End_To_End_Flows) loop
         if not Is_Empty (End_To_End_Flows.Table (I).List) then
            declare
               L : constant List_Id := End_To_End_Flows.Table (I).List;
            begin
               P := ATNU.New_Node (ATN.K_End_To_End_Flow_Spec, No_Location);
               ATN.Set_Source_Flow (P, First_Node (L));
               ATN.Set_Sink_Flow (P, Last_Node (L));
               ATN.Set_Connections (P, L);
               ATNU.Append_Node_To_List (P, Flows (R));
            end;
         end if;

      end loop;
   end Store_Flows;

   -----------
   -- Reset --
   -----------

   procedure Reset (Instance : Node_Id) is
      use Ocarina.ME_AADL.AADL_Instances.Nodes;
      R : Node_Id;

   begin
      if Present (Instance) then
         R := Root_System (Instance);
         if Present (R) then
            Set_Flows (R, ATNU.New_List (ATN.K_List_Id, No_Location));
         end if;
      end if;
   end Reset;

   ------------------
   -- Detect_Cycle --
   ------------------

   function Detect_Cycle (Branch : List_Id; E : Node_Id) return Boolean is
      use Ocarina.ME_AADL.AADL_Instances.Nodes;

      N : Node_Id := First_Node (Branch);

   begin
      while Present (N) loop
         if Corresponding_Entity (N) = E then
            return True;
         end if;
         N := Next_Node (N);
      end loop;

      return False;
   end Detect_Cycle;

   -----------------
   -- Bound_Bus --
   -----------------

   function Bound_Bus (Src : Node_Id; Dst : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Instances.Nodes;
      use Ocarina.ME_AADL.AADL_Instances.Nutils;
      --      use Namet;

      Owner       : Node_Id;
      S           : Node_Id;
      Str : constant Name_Id := Get_String_Name ("actual_connection_binding");
      Src_Process : Node_Id;
      Dst_Process : Node_Id;

   begin
      if Kind (Src) /= K_Port_Spec_Instance
        or else Kind (Dst) /= K_Port_Spec_Instance
      then
         return No_Node;
      end if;

      if Get_Category_Of_Component (Parent_Component (Src)) /= CC_Process
        or else
          Get_Category_Of_Component (Parent_Component (Dst)) /=
          CC_Process
      then
         --  A bus can only be bound to an inter-process connection
         return No_Node;
      end if;

      Src_Process := Parent_Subcomponent (Parent_Component (Src));
      Dst_Process := Parent_Subcomponent (Parent_Component (Dst));

      Owner := Parent_Component (Parent_Subcomponent (Parent_Component (Src)));

      if not Is_Empty (Connections (Owner)) then
         S := First_Node (Connections (Owner));
         while Present (S) loop
            if Kind (S) = K_Connection_Instance
              and then Is_Defined_Property (S, Str)
            then
               if Item (First_Node (Path (Source (S)))) = Src_Process
                 and then
                   Item (Next_Node (First_Node (Path (Source (S))))) =
                   Src
                 and then
                   Item (First_Node (Path (Destination (S)))) =
                   Dst_Process
                 and then
                   Item (Next_Node (First_Node (Path (Destination (S))))) =
                   Dst
               then
                  return Get_Reference_Property (S, Str);
               end if;
            end if;

            S := Next_Node (S);
         end loop;
      end if;

      return No_Node;
   end Bound_Bus;

end Ocarina.REAL_Expander.Flow_Analysis;
