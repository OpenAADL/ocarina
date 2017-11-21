------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . T R A N S F O . O P T I M                 --
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

with Ocarina.Transfo.Optim.Eval;
with Ocarina.Transfo.Fusions;
with Ocarina.Transfo.Move;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Instances;
with Ocarina.REAL_Values;
with Ocarina.Instances.REAL_Finder;
with Ocarina.Instances.REAL_Checker.Queries;
with Ocarina.Analyzer.AADL;
with GNAT.Heap_Sort_G;
with GNAT.OS_Lib;
with Ocarina.Backends;

with Ocarina.ME_AADL.AADL_Instances.Debug;
use Ocarina.ME_AADL.AADL_Instances.Debug;
with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Transfo.Optim is
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Transfo.Optim.Eval;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.Instances.REAL_Finder;
   use Ocarina.REAL_Values;
   use Set;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   Current_Solution_Set : Solution_Set;

   Cost_Limit : constant Float := -5.0;
   --  minimum distance to deadline, in ms

   Non_Optim_Str : Name_Id;

   Iteration_Number : Natural := 0;

   procedure Break_All_Threads
     (Instance_Root :     Node_Id;
      Success       : out Boolean);
   pragma Unreferenced (Break_All_Threads);
   --  Launched at init, split threads that contain
   --  more than one call sequence

   procedure Build_Actual_Merged_System
     (Instance_Root : in out Node_Id;
      Owner_Process : in out Node_Id;
      Solution      :        Solution_Set);
   --  Build the system, fusionning all threads presents
   --  in the solution

   procedure Build_Actual_Moved_System
     (Instance_Root : in out Node_Id;
      Solution      :        Solution_Set;
      Process       :        Node_Id);
   --  pragma Unreferenced (Build_Actual_Moved_System);
   --  Build the system, moving all thread from solution into
   --  process

   function Find_Best_Move
     (System, Dest_Process : Node_Id) return Solution_Set;
   --  Find a single yet optimal move

   function Is_In (E : Node_Id; Set : Solution_Set) return Boolean;
   --  Return true if the element E is in the solution set Set

   procedure Clean_All_Process_Features (Instance_Root : in out Node_Id);
   --  Clean all useless process features
   --  Used when move terminated before cleaning

   function Find_Subcomponent_By_Name
     (Component         : Node_Id;
      Subcomponent_Name : Name_Id) return Node_Id;

   ----------
   -- Init --
   ----------

   procedure Init (AADL_Instance : Node_Id) is
      use Ocarina.Backends;
   begin
      Init (Current_Solution_Set);
      Ocarina.Transfo.Fusions.Init;
      Ocarina.Transfo.Optim.Eval.Init (AADL_Instance);
      Set_Current_Backend_Name ("aadl");

      Non_Optim_Str := Get_String_Name ("transformations::no_optimization");
   end Init;

   -----------------------
   -- Break_All_Threads --
   -----------------------

   procedure Break_All_Threads
     (Instance_Root :     Node_Id;
      Success       : out Boolean)
   is
      pragma Unreferenced (Instance_Root);
   begin
      Success := True;
      --  FIXME TODO
   end Break_All_Threads;

   --------------------------------
   -- Build_Actual_Merged_System --
   --------------------------------

   procedure Build_Actual_Merged_System
     (Instance_Root : in out Node_Id;
      Owner_Process : in out Node_Id;
      Solution      :        Solution_Set)
   is
      use Ocarina.Transfo.Fusions;
      use Ocarina.Instances;
      use Ocarina.Analyzer.AADL;
      use Ocarina.Backends;

      package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

      T1, T2    : Node_Id;
      T_Merged  : Node_Id;
      Success   : Boolean;
      AADL_Tree : constant Node_Id :=
        ATN.Parent
          (ATN.Namespace
             (AIN.Corresponding_Declaration (Root_System (Instance_Root))));
   begin
      T1 := Solution.Table (First).Thread_Node;

      for I in First + 1 .. Last (Solution) loop
         T2 := Solution.Table (I).Thread_Node;

         W_Line
           ("**  merged : " &
            Get_Name_String (AIN.Name (AIN.Identifier (T1))) &
            " & " &
            Get_Name_String (AIN.Name (AIN.Identifier (T2))));

         Fusion_Threads
           (AADL_Tree,
            AIN.Name (AIN.Identifier (Owner_Process)),
            AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (T1))),
            AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (T2))),
            T_Merged,
            Success);
         if not Success then
            raise Program_Error with "thread fusion failed";
         end if;

         Generate_Code (AADL_Tree);

         Ocarina.Transfo.Fusions.Reset;

         Ocarina.Analyzer.AADL.Reset;

         if Analyze_Model (AADL_Tree) then
            Instance_Root := Instantiate_Model (AADL_Tree);
            if No (Instance_Root) then
               raise Program_Error
                 with "Cannot instantiate the " & "fusioned AADL model";
            end if;
         else
            raise Program_Error
              with "Cannot analyze the " & "fusioned AADL model";
         end if;

         T1            := AIN.First_Node (ATN.Instances (T_Merged));
         Owner_Process :=
           AIN.Parent_Subcomponent
             (AIN.Parent_Component (AIN.Parent_Subcomponent (T1)));
      end loop;

      Ocarina.Instances.REAL_Checker.Queries.Init (Instance_Root);
   end Build_Actual_Merged_System;

   -------------------------------
   -- Build_Actual_Moved_System --
   -------------------------------

   procedure Build_Actual_Moved_System
     (Instance_Root : in out Node_Id;
      Solution      :        Solution_Set;
      Process       :        Node_Id)
   is
      use Ocarina.Instances;
      use Ocarina.Analyzer.AADL;
      use Ocarina.Transfo.Move;
      use Ocarina.Backends;

      package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

      AADL_Tree : constant Node_Id :=
        ATN.Parent
          (ATN.Namespace
             (AIN.Corresponding_Declaration (Root_System (Instance_Root))));

      T : Node_Id;
      P : Node_Id;
      M : Boolean;
   begin

      for I in First .. Last (Solution) loop
         T := Parent_Subcomponent (Solution.Table (I).Thread_Node);
         P := Parent_Subcomponent (Parent_Component (T));

         W_Line
           ("move " &
            Get_Name_String (Name (Identifier (T))) &
            " from " &
            Get_Name_String (Name (Identifier (P))) &
            " to " &
            Get_Name_String
              (Name (Identifier (Parent_Subcomponent (Process)))));

         Move_Thread
           (Name (Identifier (T)),
            Name (Identifier (P)),
            Name (Identifier (Parent_Subcomponent (Process))));

         Generate_Code (AADL_Tree);

         Ocarina.Transfo.Move.Reset (AADL_Tree);
         Ocarina.Analyzer.AADL.Reset;

         if Analyze_Model (AADL_Tree) then
            Instance_Root := Instantiate_Model (AADL_Tree);
            if No (Instance_Root) then
               raise Program_Error
                 with "Cannot instantiate the " & "moved AADL model";
            end if;
         else
            W_Line ("Cannot analyze the " & "moved AADL model");
            W_Line ("");
            W_Line
              ("STEP => iteration number : " &
               Int'Image (Int (Iteration_Number)));
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         M :=
           Clean_Obsolete_Features
             (Root_System (Instance_Root),
              Name (Identifier (P)));
         M :=
           Clean_Obsolete_Features
             (Root_System (Instance_Root),
              Name (Identifier (Parent_Subcomponent (Process))))
           or else M;

         if M then
            Ocarina.Transfo.Move.Reset (AADL_Tree);
            Ocarina.Analyzer.AADL.Reset;

            if Analyze_Model (AADL_Tree) then
               Instance_Root := Instantiate_Model (AADL_Tree);
               if No (Instance_Root) then
                  raise Program_Error
                    with "Cannot instantiate the " & "moved AADL model";
               end if;
            else
               W_Line ("Cannot analyze the " & "moved AADL model");
               W_Line ("");
               W_Line
                 ("STEP => iteration number : " &
                  Int'Image (Int (Iteration_Number)));
               GNAT.OS_Lib.OS_Exit (1);
            end if;
         end if;

      end loop;
      Ocarina.Instances.REAL_Checker.Queries.Init (Instance_Root);
   end Build_Actual_Moved_System;

   ------------------
   -- Copy_Threads --
   ------------------

   function Copy_Threads (Process : Node_Id) return Solution_Set is
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Instances.Entities;

      Process_Subcomponent : Node_Id := First_Node (Subcomponents (Process));
      Unsorted_List        : Solution_Set;
      N                    : Node_Id;
      TU                   : Thread_Unit;
   begin
      --  Build the non-sorted list

      Init (Unsorted_List);

      while Present (Process_Subcomponent) loop

         N := Corresponding_Instance (Process_Subcomponent);
         if Kind (N) = K_Component_Instance
           and then Get_Category_Of_Component (N) = CC_Thread
         then

            TU.Thread_Node := N;
            Append (Unsorted_List, TU);
         end if;

         Process_Subcomponent := Next_Node (Process_Subcomponent);
      end loop;

      return Unsorted_List;
   end Copy_Threads;

   ------------------
   -- Sort_Threads --
   ------------------

   function Sort_Threads (Process : Node_Id) return Solution_Set is
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Instances.Entities;

      pragma Assert
        (Kind (Process) = K_Component_Instance
         and then Get_Category_Of_Component (Process) = CC_Process);

      Array_Max_Size : constant Natural := Length (Subcomponents (Process));

      type Node_Table is array (0 .. Array_Max_Size) of Node_Id;

      Threads : Node_Table;

      procedure Internal_Move (From, To : Natural);
      --  Perform a move on the Threads node_table

      function Internal_Compare (Left, Right : Natural) return Boolean;
      --  Compare two indexes of the Threads node_table

      function True_Connections_Number (Node : Node_Id) return Natural;
      --  Compute teh number of connections to and from
      --  threads from the same process

      package Node_Sort is new GNAT.Heap_Sort_G
        (Move => Internal_Move,
         Lt   => Internal_Compare);

      -----------------------------
      -- True_Connections_Number --
      -----------------------------

      function True_Connections_Number (Node : Node_Id) return Natural is
         Feat          : constant List_Id := Features (Node);
         N, N2, Parent : Node_Id;
         Val_Left      : Natural          := 0;
      begin
         if not Is_Empty (Feat) then
            N := First_Node (Feat);
            while Present (N) loop

               if Kind (N) = K_Port_Spec_Instance then

                  if Is_In (N) then

                     N2 := First_Node (Sources (N));
                     while Present (N2) loop

                        --  Check if the source feature is in the same
                        --  process

                        Parent := Item (N2);
                        if Kind (Parent) = K_Port_Spec_Instance then
                           Parent := Parent_Component (Parent);
                           if Get_Category_Of_Component (Parent) =
                             CC_Thread
                           then
                              Val_Left := Val_Left + 1;
                           end if;
                        end if;

                        N2 := Next_Node (N2);
                     end loop;
                  end if;

                  if Is_Out (N) then
                     N2 := First_Node (Destinations (N));
                     while Present (N2) loop

                        --  Check if the destination feature is in the same
                        --  process

                        Parent := Item (N2);
                        if Kind (Parent) = K_Port_Spec_Instance then
                           Parent := Parent_Component (Parent);
                           if Get_Category_Of_Component (Parent) =
                             CC_Thread
                           then
                              Val_Left := Val_Left + 1;
                           end if;
                        end if;

                        N2 := Next_Node (N2);
                     end loop;
                  end if;
               end if;

               N := Next_Node (N);
            end loop;
         end if;

         return Val_Left;
      end True_Connections_Number;

      -------------------
      -- Internal_Move --
      -------------------

      procedure Internal_Move (From, To : Natural) is
      begin
         Threads (To) := Threads (From);
      end Internal_Move;

      ----------------------
      -- Internal_Compare --
      ----------------------

      function Internal_Compare (Left, Right : Natural) return Boolean is
         Val_Left  : Natural;
         Val_Right : Natural;
      begin
         --  FIXME :
         --  Currently only look at the number of connections
         --  Should call a REAL theorem

         Val_Left := True_Connections_Number (Threads (Left));

         Val_Right := True_Connections_Number (Threads (Right));

         return (Val_Left > Val_Right);
      end Internal_Compare;

      Process_Subcomponent : Node_Id := First_Node (Subcomponents (Process));
      Sorted_List          : Solution_Set;
      N                    : Node_Id;
      It                   : Natural := 0;
      TU                   : Thread_Unit;
   begin
      --  Build the original, non-sorted list

      while Present (Process_Subcomponent) loop

         N := Corresponding_Instance (Process_Subcomponent);
         if Kind (N) = K_Component_Instance
           and then Get_Category_Of_Component (N) = CC_Thread
         then
            It           := It + 1;
            Threads (It) := N;
         end if;

         Process_Subcomponent := Next_Node (Process_Subcomponent);
      end loop;

      --  Proceed to sort

      Node_Sort.Sort (It);

      --  Create the corresponding sorted list

      Init (Sorted_List);
      for I in 1 .. It loop
         TU.Thread_Node := Threads (I);
         Append (Sorted_List, TU);
      end loop;

      return Sorted_List;
   end Sort_Threads;

   --------------------------------
   -- Clean_All_Process_Features --
   --------------------------------

   procedure Clean_All_Process_Features (Instance_Root : in out Node_Id) is
      use Ocarina.Transfo.Move;
      use Ocarina.ME_AADL.AADL_Instances.Entities;
      use Ocarina.ME_AADL;
      use Ocarina.Instances;
      use Ocarina.Analyzer.AADL;
      use Ocarina.Backends;

      package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

      System    : constant Node_Id := AIN.Root_System (Instance_Root);
      AADL_Tree : constant Node_Id :=
        ATN.Parent (ATN.Namespace (AIN.Corresponding_Declaration (System)));
      S         : Node_Id := First_Node (Subcomponents (System));
      N         : Node_Id;
      Local_Mod : Boolean;
      Modified  : Boolean := False;
   begin
      while Present (S) loop
         N := Corresponding_Instance (S);
         if Kind (N) = K_Component_Instance
           and then Get_Category_Of_Component (N) = CC_Process
         then

            Local_Mod :=
              Clean_Obsolete_Features (System, Name (Identifier (S)));
            Modified := Modified or else Local_Mod;
         end if;

         S := Next_Node (S);
      end loop;

      if not Modified then
         return;
      end if;

      Generate_Code (AADL_Tree);

      Ocarina.Analyzer.AADL.Reset;
      if Analyze_Model (AADL_Tree) then
         Instance_Root := Instantiate_Model (AADL_Tree);
         if No (Instance_Root) then
            W_Line ("Cannot instantiate the " & "cleaned AADL model");
            W_Line ("");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      else
         W_Line ("Cannot analyze the " & "cleaned AADL model");
         W_Line ("");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Clean_All_Process_Features;

   --------------------
   -- Find_Best_Move --
   --------------------

   function Find_Best_Move
     (System, Dest_Process : Node_Id) return Solution_Set
   is
      Processes              : Node_Id := First_Node (Subcomponents (System));
      Process                : Node_Id;
      Threads                : Node_Id;
      Thread                 : Node_Id;
      Non_Optim              : Value_Id;
      Optimize               : Boolean;
      Candidate              : Node_Id := No_Node;
      Candidate_Value        : Float   := 0.0;
      Best_Value             : Float   := 0.0;
      Candidate_Solution_Set : Solution_Set;
      TU                     : Thread_Unit;
      Success                : Boolean;
   begin
      while Present (Processes) loop
         Process := Corresponding_Instance (Processes);

         Non_Optim := Get_Property_Value (Non_Optim_Str, Process);
         Optimize  :=
           ((Non_Optim = No_Value)
            or else (not Get_Value_Type (Non_Optim).BVal));
         if Process /= Dest_Process and then Optimize then

            if not Is_Empty (Subcomponents (Process)) then
               Threads := First_Node (Subcomponents (Process));
            else
               Threads := No_Node;
            end if;
            while Present (Threads) loop
               Thread := Corresponding_Instance (Threads);

               --  We build an heterogenous set

               TU.Thread_Node := Thread;
               Init (Candidate_Solution_Set);
               Append (Candidate_Solution_Set, TU);
               TU.Thread_Node := Dest_Process;
               Append (Candidate_Solution_Set, TU);

               Compute_Relative_Move_Value
                 (Candidate_Solution_Set,
                  Candidate_Value,
                  Success);
               if not Success then
                  raise Program_Error;
               end if;

               if Candidate_Value > Best_Value then
                  Candidate  := Thread;
                  Best_Value := Candidate_Value;
               end if;

               Free (Candidate_Solution_Set);
               Threads := Next_Node (Threads);
            end loop;

         end if;

         Processes := Next_Node (Processes);
      end loop;

      Init (Candidate_Solution_Set);
      if Present (Candidate) then
         TU.Thread_Node := Candidate;
         Append (Candidate_Solution_Set, TU);
      end if;

      return Candidate_Solution_Set;
   end Find_Best_Move;

   -------------------------------
   -- Find_Subcomponent_By_Name --
   -------------------------------

   function Find_Subcomponent_By_Name
     (Component         : Node_Id;
      Subcomponent_Name : Name_Id) return Node_Id
   is
      pragma Assert (AIN.Kind (Component) = K_Component_Instance);

      N : Node_Id := First_Node (Subcomponents (Component));

   begin
      while Present (N) loop
         if Name (Identifier (N)) = Subcomponent_Name then
            return N;
         end if;
         N := AIN.Next_Node (N);
      end loop;

      return No_Node;
   end Find_Subcomponent_By_Name;

   ----------------------
   -- Greedy_Heuristic --
   ----------------------

   procedure Greedy_Heuristic
     (Instance_Root : in out Node_Id;
      Success       :    out Boolean)
   is
      use Ocarina.ME_AADL.AADL_Instances.Entities;
      use Ocarina.ME_AADL;

      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);

      System                 : Node_Id;
      Subcomp                : Node_Id;
      Best_Candidate         : Node_Id;
      System_Value           : Float;
      Process                : Node_Id;
      Non_Optim              : Value_Id;
      Optimize               : Boolean;
      Process_Name           : Name_Id;
      Found                  : Boolean;
      Candidate_Solution_Set : Solution_Set;
   begin
      Success := True;

      Clean_All_Process_Features (Instance_Root);
      System  := Root_System (Instance_Root);
      Subcomp := First_Node (Subcomponents (System));

      Compute_System_Value (System_Value, Success);
      if not Success then
         return;
      end if;
      Compute_System_Cost (System_Value, Success);
      if not Success then
         return;
      end if;

      while Present (Subcomp) loop
         Process_Name := Name (Identifier (Subcomp));
         Process      := Corresponding_Instance (Subcomp);

         Non_Optim := Get_Property_Value (Non_Optim_Str, Process);
         Optimize  :=
           (((Non_Optim = No_Value)
             or else (not Get_Value_Type (Non_Optim).BVal))
            and then (Get_Category_Of_Component (Process) = CC_Process));

         if Optimize then

            W_Line ("");
            W_Line ("process " & Get_Name_String (Process_Name));

            declare
               Thread, T, T_Subcomp : Node_Id;
               Thread_List          : List_Id;
               Continue             : Boolean;
               Candidate_Value      : Float;
               Local_Value          : Float;
               Local_Cost           : Float;
               Local_Best_Value     : Float;
               Sorted_Threads_List  : Solution_Set;
               TU                   : Thread_Unit;
               Pre_Found            : Boolean;
            begin
               while True loop
                  --  Reload the system, since the model has been
                  --  reinstantiated

                  System := Root_System (Instance_Root);

                  --  Since fusion exchange the process defintion
                  --  but does not change teh related subcomponent
                  --  name, we can find back teh current process
                  --  after the first iteration

                  Subcomp := Find_Subcomponent_By_Name (System, Process_Name);
                  if No (Process) then
                     raise Program_Error;
                  end if;
                  Process := Corresponding_Instance (Subcomp);

                  Thread_List         := Subcomponents (Process);
                  Sorted_Threads_List := Sort_Threads (Process);
                  Found               := False;
                  Candidate_Value     := 0.0;

                  for I in Set.First .. Last (Sorted_Threads_List) loop
                     Thread := Sorted_Threads_List.Table (I).Thread_Node;

                     --  We try a thread as first element of the set of
                     --  fusionable threads

                     TU.Thread_Node := Thread;
                     Init (Candidate_Solution_Set);
                     Append (Candidate_Solution_Set, TU);
                     Local_Best_Value := 0.0;

                     --  Look for optimal candidates for fusion with the
                     --  previous thread

                     Pre_Found := False;
                     Continue  := True;
                     while Continue loop

                        Best_Candidate := No_Node;

                        T_Subcomp := First_Node (Thread_List);
                        while Present (T_Subcomp) loop
                           T := Corresponding_Instance (T_Subcomp);

                           if T /= Thread
                             and then not Is_In (T, Candidate_Solution_Set)
                           then
                              TU.Thread_Node := T;
                              Append (Candidate_Solution_Set, TU);

                              --  1/ evaluate

                              Compute_Relative_Cost
                                (Candidate_Solution_Set,
                                 Local_Cost,
                                 Success);
                              if Success and then Local_Cost < Cost_Limit then
                                 Compute_Relative_Value
                                   (Candidate_Solution_Set,
                                    Local_Value,
                                    Success);
                              end if;
                              if not Success then
                                 return;
                              end if;

                              --  2/ Tag the current thread as candidate if
                              --  it has a better value than the previous
                              --  ones.

                              if Local_Value > Local_Best_Value
                                and then Local_Cost < Cost_Limit
                              then
                                 Best_Candidate   := T;
                                 Local_Best_Value := Local_Value;
                              end if;
                              Decrement_Last (Candidate_Solution_Set);

                              Iteration_Number := Iteration_Number + 1;
                           end if;

                           T_Subcomp := Next_Node (T_Subcomp);
                        end loop;

                        Continue := Present (Best_Candidate);

                        if Continue then
                           TU.Thread_Node := Best_Candidate;
                           Append (Candidate_Solution_Set, TU);
                           Pre_Found := True;
                        end if;
                     end loop;

                     --  FIXME
                     --  More precise evaluation is not possible without
                     --  actual fusion.

                     if Local_Best_Value > Candidate_Value
                       and then Pre_Found
                     then
                        Copy (Candidate_Solution_Set, Current_Solution_Set);
                        Candidate_Value := Local_Best_Value;
                        Found           := True;
                        exit;
                     end if;
                  end loop;

                  Free (Sorted_Threads_List);

                  if Found then
                     Build_Actual_Merged_System
                       (Instance_Root,
                        Subcomp,
                        Current_Solution_Set);
                     declare
                        Move_Set : Solution_Set;
                     begin
                        System  := Root_System (Instance_Root);
                        Subcomp :=
                          Find_Subcomponent_By_Name (System, Process_Name);
                        if No (Process) then
                           raise Program_Error;
                        end if;
                        Process := Corresponding_Instance (Subcomp);

                        Move_Set := Find_Best_Move (System, Process);
                        if Last (Move_Set) > 0 then
                           Build_Actual_Moved_System
                             (Instance_Root,
                              Move_Set,
                              Process);
                           Free (Move_Set);
                        end if;
                     end;

                     --  FIXME
                     --  Binary analysis
                     --  Update timing properties

                     Free (Sorted_Threads_List);
                     Free (Candidate_Solution_Set);
                  else
                     Free (Sorted_Threads_List);
                     Free (Candidate_Solution_Set);
                     exit;
                  end if;
               end loop;
            end;
         end if;

         --  This is ok because the subcomponent has been replaced in
         --  the system subcomponent list, hence its place is the same
         --  as the original subcomponent

         Subcomp := Next_Node (Subcomp);
      end loop;

      W_Line
        ("END => iteration number : " & Int'Image (Int (Iteration_Number)));
   end Greedy_Heuristic;

   ----------------------------------
   -- Exhaustive_Space_Exploration --
   ----------------------------------

   procedure Exhaustive_Space_Exploration
     (Instance_Root : in out Node_Id;
      Success       :    out Boolean)
   is
      use Ocarina.ME_AADL.AADL_Instances.Entities;
      use Ocarina.ME_AADL;

      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);

      System         : Node_Id;
      Subcomp        : Node_Id;
      Best_Candidate : Node_Id;
      System_Value   : Float;
      Process        : Node_Id;

      Non_Optim_Str : constant Name_Id :=
        Get_String_Name ("transformations::no_optimization");
      Non_Optim              : Value_Id;
      Optimize               : Boolean;
      Process_Name           : Name_Id;
      Found                  : Boolean;
      Candidate_Solution_Set : Solution_Set;
   begin
      Success := True;

      Clean_All_Process_Features (Instance_Root);
      System  := Root_System (Instance_Root);
      Subcomp := First_Node (Subcomponents (System));

      Compute_System_Value (System_Value, Success);
      if not Success then
         return;
      end if;
      Compute_System_Cost (System_Value, Success);
      if not Success then
         return;
      end if;

      while Present (Subcomp) loop
         Process_Name := Name (Identifier (Subcomp));
         Process      := Corresponding_Instance (Subcomp);

         Non_Optim := Get_Property_Value (Non_Optim_Str, Process);
         Optimize  :=
           (((Non_Optim = No_Value)
             or else (not Get_Value_Type (Non_Optim).BVal))
            and then (Get_Category_Of_Component (Process) = CC_Process));

         if Optimize then

            W_Line ("");
            W_Line ("process " & Get_Name_String (Process_Name));

            declare
               Thread, T, T_Subcomp  : Node_Id;
               Thread_List           : List_Id;
               Continue              : Boolean;
               Candidate_Value       : Float;
               Local_Value           : Float;
               Local_Cost            : Float;
               Local_Best_Value      : Float;
               Unsorted_Threads_List : Solution_Set;
               TU                    : Thread_Unit;
               Pre_Found             : Boolean;
            begin
               while True loop
                  --  Reload the system, since the model has been
                  --  reinstantiated

                  System := Root_System (Instance_Root);

                  --  Since fusion exchange the process defintion
                  --  but does not change teh related subcomponent
                  --  name, we can find back teh current process
                  --  after the first iteration

                  Subcomp := Find_Subcomponent_By_Name (System, Process_Name);
                  if No (Process) then
                     raise Program_Error;
                  end if;
                  Process := Corresponding_Instance (Subcomp);
                  Found   := False;

                  Thread_List           := Subcomponents (Process);
                  Unsorted_Threads_List := Copy_Threads (Process);
                  Candidate_Value       := 0.0;
                  for I in Set.First .. Last (Unsorted_Threads_List) loop
                     Thread := Unsorted_Threads_List.Table (I).Thread_Node;
                     Local_Best_Value := 0.0;

                     --  We try a thread as first element of the set of
                     --  fusionable threads

                     TU.Thread_Node := Thread;
                     Init (Candidate_Solution_Set);
                     Append (Candidate_Solution_Set, TU);

                     --  Look for optimal candidates for fusion with the
                     --  previous thread

                     Pre_Found := False;
                     Continue  := True;
                     while Continue loop
                        Best_Candidate := No_Node;
                        T_Subcomp      := First_Node (Thread_List);

                        while Present (T_Subcomp) loop
                           T := Corresponding_Instance (T_Subcomp);

                           if T /= Thread
                             and then not Is_In (T, Candidate_Solution_Set)
                           then
                              TU.Thread_Node := T;
                              Append (Candidate_Solution_Set, TU);

                              --  1/ evaluate

                              Compute_Relative_Cost
                                (Candidate_Solution_Set,
                                 Local_Cost,
                                 Success);
                              if Success and then Local_Cost < Cost_Limit then
                                 Compute_Relative_Value
                                   (Candidate_Solution_Set,
                                    Local_Value,
                                    Success);
                              end if;
                              if not Success then
                                 return;
                              end if;

                              --  2/ Tag the current thread as candidate if
                              --  it has a better value than the previous
                              --  ones.

                              if Local_Value > Local_Best_Value
                                and then Local_Cost < Cost_Limit
                              then
                                 Best_Candidate   := T;
                                 Local_Best_Value := Local_Value;
                              end if;

                              Iteration_Number := Iteration_Number + 1;
                              Decrement_Last (Candidate_Solution_Set);
                           end if;

                           T_Subcomp := Next_Node (T_Subcomp);
                        end loop;

                        Continue := Present (Best_Candidate);

                        if Continue then
                           TU.Thread_Node := Best_Candidate;
                           Append (Candidate_Solution_Set, TU);
                           Pre_Found := True;
                        end if;

                     end loop;

                     --  FIXME
                     --  More precise evaluation is not possible without
                     --  actual fusion.

                     if Local_Best_Value > Candidate_Value
                       and then Pre_Found
                     then
                        Copy (Candidate_Solution_Set, Current_Solution_Set);
                        Candidate_Value := Local_Best_Value;
                        Found           := True;
                     end if;
                  end loop;

                  if Found then
                     Build_Actual_Merged_System
                       (Instance_Root,
                        Subcomp,
                        Current_Solution_Set);

                     declare
                        Move_Set : Solution_Set;
                     begin
                        System  := Root_System (Instance_Root);
                        Subcomp :=
                          Find_Subcomponent_By_Name (System, Process_Name);
                        if No (Process) then
                           raise Program_Error;
                        end if;
                        Process := Corresponding_Instance (Subcomp);

                        Move_Set := Find_Best_Move (System, Process);
                        if Last (Move_Set) > 0 then
                           Build_Actual_Moved_System
                             (Instance_Root,
                              Move_Set,
                              Process);
                           Free (Move_Set);
                        end if;
                     end;

                     --  FIXME
                     --  Binary analysis
                     --  Update timing properties

                     Free (Candidate_Solution_Set);
                  else
                     Free (Unsorted_Threads_List);
                     Free (Candidate_Solution_Set);
                     exit;
                  end if;

                  Free (Unsorted_Threads_List);
                  Free (Candidate_Solution_Set);
               end loop;
            end;
         end if;

         --  This is ok because the subcomponent has been replaced in
         --  the system subcomponent list, hence its place is the same
         --  as the original subcomponent

         Subcomp := Next_Node (Subcomp);
      end loop;

      W_Line
        ("END => iteration number : " & Int'Image (Int (Iteration_Number)));
   end Exhaustive_Space_Exploration;

   -----------
   -- Is_In --
   -----------

   function Is_In (E : Node_Id; Set : Solution_Set) return Boolean is
   begin
      for N in First .. Last (Set) loop
         if Set.Table (N).Thread_Node = E then
            return True;
         end if;
      end loop;

      return False;
   end Is_In;

end Ocarina.Transfo.Optim;
