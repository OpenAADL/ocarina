------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . T R A N S F O . F U S I O N S . S C H E D U L E R     --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Errors;
with Ocarina.Namet;
with GNAT.Dynamic_Tables;
with Ada.Text_IO;
with Ocarina.Transfo.Fusions;

package body Ocarina.Transfo.Fusions.Scheduler is

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Errors;
   use Ocarina.Namet;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package OTF renames Ocarina.Transfo.Fusions;

   type Node_Ref is record
      Node : Node_Id;
   end record;

   package CS_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Node_Ref,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,  -- # of elements
      Table_Increment      => 50); -- % increase

   package Mode_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Node_Ref,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,  -- # of elements
      Table_Increment      => 50); -- % increase

   Quantum       : Natural := 0;
   Hyperperiod   : Natural;
   Initial_Mode  : Node_Id;
   Thread        : Node_Id;
   Mode_Schedule : Mode_Table.Instance;

   function Get_Single_Mode (Call_Sequence : Node_Id) return Node_Id;
   --  Returns the call sequence mode, if any
   --  if their is more than one mode, raises an error
   --  if their is zero mode, returns no_node

   function Sort_Call_Sequences return CS_Table.Instance;
   --  Returns a list of the thread's call sequences sorted by priority

   function Compute_GCD (V1, V2 : Int) return Int;
   --  Computes Greatest Common Denominator of the two parameters

   procedure Get_Thread_Quantum (Thr : Node_Id; GCD, LCM : out Natural);
   --  Returns the Greatest Common Denominator and the Lowest Common
   --  Multiple of the thread's call sequences periods _or_ the thread
   --  period as specified with the 'period' parameter. Puts 0 in GCD in
   --  case  of failure to compute.

   -----------------
   -- Compute_GCD --
   -----------------

   function Compute_GCD (V1, V2 : Int) return Int is
   begin
      if V2 = 0 then
         return V1;
      else
         return Compute_GCD (V2, V1 mod V2);
      end if;
   end Compute_GCD;

   -----------------
   -- Get_Quantum --
   -----------------

   function Get_Quantum return Natural is
   begin
      return Quantum;
   end Get_Quantum;

   -------------------------
   -- Sort_Call_Sequences --
   -------------------------

   function Sort_Call_Sequences return CS_Table.Instance is
      procedure Insert
        (Call_Seq  :        Node_Id;
         List      : in out CS_Table.Instance;
         Real_Size : in out Natural);
      --  Insert a call sequence element regarding to the period order

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Call_Seq  :        Node_Id;
         List      : in out CS_Table.Instance;
         Real_Size : in out Natural)
      is
         It  : Natural := CS_Table.First;
         Per : Natural;
         CS  : Node_Id;
         CS2 : Node_Id;
      begin
         Per := Get_Call_Sequence_Priority (Call_Seq);

         while It <= Real_Size loop
            exit when Get_Call_Sequence_Priority (List.Table (It).Node) > Per;
            It := It + 1;
         end loop;

         CS                   := List.Table (It).Node;
         List.Table (It).Node := Call_Seq;
         It                   := It + 1;

         while It <= Real_Size loop
            CS2                  := List.Table (It).Node;
            List.Table (It).Node := CS;
            CS                   := CS2;
            It                   := It + 1;
         end loop;
         List.Table (It).Node := CS;

         Real_Size := Real_Size + 1;
      end Insert;

      S       : Natural;
      C       : Node_Id;
      CS_List : CS_Table.Instance;
      Ref     : Node_Ref;
   begin
      if Is_Empty (Calls (Thread)) then
         raise Program_Error;
      end if;

      CS_Table.Init (CS_List);

      --  First we build a table of the right size, with empty cells

      C := First_Node (Calls (Thread));
      while Present (C) loop
         if Present (Get_Single_Mode (C)) then
            Ref.Node := No_Node;
            CS_Table.Append (CS_List, Ref);
         end if;
         C := Next_Node (C);
      end loop;

      --  Then we make an ordered insert

      S := 0;
      C := First_Node (Calls (Thread));
      while Present (C) loop
         if Present (Get_Single_Mode (C)) then
            Insert (C, CS_List, S);
         end if;
         C := Next_Node (C);
      end loop;

      return CS_List;
   end Sort_Call_Sequences;

   ------------------------------
   -- Get_Call_Sequence_Period --
   ------------------------------

   function Get_Call_Sequence_Period
     (Call_Sequence : Node_Id) return Natural
   is
      pragma Assert (Kind (Call_Sequence) = K_Subprogram_Call_Sequence);

      Per     : Natural;
      Wrapper : Node_Id;

   begin
      if Is_Empty (Subprogram_Calls (Call_Sequence)) then
         raise Program_Error;
      end if;

      Wrapper :=
        Entity (Entity_Ref (First_Node (Subprogram_Calls (Call_Sequence))));

      if Kind (Wrapper) = K_Subcomponent_Access then
         Wrapper := Entity (Entity_Ref (Wrapper));
      end if;

      if OTF.Is_Defined_Property (Wrapper, CS_Period) then
         Per := Natural (OTF.Get_Integer_Property (Wrapper, CS_Period));
      else
         return 0;
      end if;

      return Per;
   end Get_Call_Sequence_Period;

   --------------------------------
   -- Get_Call_Sequence_Priority --
   --------------------------------

   function Get_Call_Sequence_Priority
     (Call_Sequence : Node_Id) return Natural
   is
      pragma Assert (Kind (Call_Sequence) = K_Subprogram_Call_Sequence);

      Per     : Natural;
      Wrapper : Node_Id;
      Data    : Node_Id;

   begin
      if Is_Empty (Subprogram_Calls (Call_Sequence)) then
         raise Program_Error;
      end if;

      Wrapper :=
        Entity (Entity_Ref (First_Node (Subprogram_Calls (Call_Sequence))));

      if Kind (Wrapper) = K_Subcomponent_Access then
         Data := Container_Component (Wrapper);

         if OTF.Is_Defined_Property (Data, Raw_Priority) then
            Per := Natural (OTF.Get_Integer_Property (Data, Raw_Priority));
         else
            DE ("Data " &
               Get_Name_String (Name (Identifier (Data))) &
               " must be protected if it offer a subprogram access");
            raise Program_Error;
         end if;

      elsif OTF.Is_Defined_Property (Wrapper, Transfo_Priority) then
         Per := Natural (OTF.Get_Integer_Property (Wrapper, Transfo_Priority));

      else
         Per := 0;
      end if;

      return Per;
   end Get_Call_Sequence_Priority;

   ------------------------
   -- Get_Thread_Quantum --
   ------------------------

   procedure Get_Thread_Quantum (Thr : Node_Id; GCD, LCM : out Natural) is
      C       : Node_Id;
      Tmp_GCD : Int := 0;
      Tmp_LCM : Int := 0;
      Current : Int := -1;

   begin
      LCM := 0;
      if Is_Empty (Calls (Thr)) then
         raise Program_Error;
      end if;

      C := First_Node (Calls (Thr));
      while Present (C) loop
         Current := Int (Get_Call_Sequence_Period (C));

         --  We consider only call sequences with periods

         if Tmp_GCD > 0 then
            if Current > 0 then
               Tmp_GCD := Compute_GCD (Current, Tmp_LCM);

               --  Since GCD (a, b) * LCM (a, b) = a * b
               --  LCM (a, b) = (a * b) / GCD (a, b)
               Tmp_LCM := (Current * Tmp_LCM) / Tmp_GCD;
            end if;
         elsif Current > 0 then
            Tmp_GCD := Current;
            Tmp_LCM := Current;
         end if;

         C := Next_Node (C);
      end loop;

      if Tmp_GCD = 0 then
         if OTF.Is_Defined_Property (Thr, Period) then
            GCD := Natural (OTF.Get_Integer_Property (Thr, Period));
            LCM := GCD;
            return;
         else
            GCD := 0;
            LCM := 0;
            return;
         end if;
      end if;

      GCD := Natural (Tmp_GCD);
      LCM := Natural (Tmp_LCM);
   end Get_Thread_Quantum;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (New_Thread, Old_Thread_1, Old_Thread_2 : Node_Id) is
      Th1_GCD : Natural;
      Th2_GCD : Natural;
      Th1_LCM : Natural;
      Th2_LCM : Natural;

   begin
      Initial_Mode := No_Node;

      Get_Thread_Quantum (Old_Thread_1, Th1_GCD, Th1_LCM);
      Get_Thread_Quantum (Old_Thread_2, Th2_GCD, Th2_LCM);

      if Th1_GCD = 0 then
         if Th2_GCD = 0 then

            --  No need for a scheduler

            DE ("No need for a scheduler when no threads are periodic");
            raise Program_Error;
         else
            Quantum     := Th2_GCD;
            Hyperperiod := Th2_LCM;
         end if;
      else
         if Th2_GCD = 0 then
            Quantum     := Th1_GCD;
            Hyperperiod := Th1_LCM;
         else
            if Th1_LCM > Th2_LCM then
               Quantum := Natural (Compute_GCD (Int (Th1_GCD), Int (Th2_GCD)));
            else
               Quantum := Natural (Compute_GCD (Int (Th2_GCD), Int (Th1_GCD)));
            end if;

            Hyperperiod := (Th1_LCM * Th2_LCM) / Quantum;
         end if;
      end if;

      Thread := New_Thread;
      Mode_Table.Init (Mode_Schedule);
   end Initialize;

   -----------------------
   -- Generate_Schedule --
   -----------------------

   procedure Generate_Schedule is
      use CS_Table;

      Iter      : Natural := 0;
      Sorted_CS : CS_Table.Instance;
      CS        : Node_Id;
      Ptr       : Natural := First;
      CS_Per    : Natural;
   --  Per       : Natural;
   begin
      Sorted_CS := Sort_Call_Sequences;

      --  check the list :
      Ptr := First;
      while Ptr <= Last (Sorted_CS) loop
         Ptr := Ptr + 1;
      end loop;

      while Iter < Hyperperiod loop
         Ptr := First;
         while Ptr <= Last (Sorted_CS) loop
            CS     := Sorted_CS.Table (Ptr).Node;
            CS_Per := Get_Call_Sequence_Period (CS);

            if CS_Per /= 0 then
               --  We call the call sequence each time that the current
               --  time is a multiple of the period

               if Iter mod CS_Per = 0 then
                  declare
                     Ref : Node_Ref;
                  begin
                     Ref.Node := Get_Single_Mode (CS);
                     Mode_Table.Append (Mode_Schedule, Ref);
                  end;
               end if;
            end if;

            Ptr := Ptr + 1;
         end loop;

         Iter := Iter + Quantum;
      end loop;

      Initial_Mode := Sorted_CS.Table (First).Node;

      CS_Table.Free (Sorted_CS);
   end Generate_Schedule;

   -----------------------
   -- Find_Initial_Mode --
   -----------------------

   function Find_Initial_Mode return Node_Id is
   begin
      if No (Initial_Mode) then
         DE ("generate_scheduler must be call before find_initial_mode");
         raise Program_Error;
      end if;

      return Get_Single_Mode (Initial_Mode);
   end Find_Initial_Mode;

   --------------------
   -- Print_Schedule --
   --------------------

   procedure Print_Schedule (Package_Name, Thread_Inst_Name : Name_Id) is
      use Ada.Text_IO;
      use Mode_Table;

      Scheduler : constant Name_Id := Get_String_Name ("PolyORB_HI.Scheduler");
      Thread_Name : constant Name_Id :=
        Get_String_Name
          (Get_Name_String (Package_Name) &
           "_" &
           Get_Name_String (Thread_Inst_Name));
      Sched_Base : constant Name_Id :=
        Get_String_Name
          (Get_Name_String (Thread_Name) & "_scheduler_instance");
      Sched_File : constant String  := Get_Name_String (Sched_Base) & ".ads";
      Pkg_Name   : constant Name_Id := Sched_Base;
      Type_Name  : constant Name_Id :=
        Get_String_Name ("Sched_" & Get_Name_String (Thread_Name) & "_T");
      Mode_Type : constant Name_Id :=
        Get_String_Name (Get_Name_String (Thread_Name) & "_Mode_Type");
      Inst_Pkg : constant Name_Id :=
        Get_String_Name (Get_Name_String (Thread_Name) & "_Mode");
      Function_name : constant Name_Id := Get_String_Name ("Change_Mode");

      FD : File_Type;
      M  : Node_Id;
      It : Natural;
   begin
      --  Create a new file for the scheduler instance

      Create (File => FD, Name => Sched_File);

      Put_Line (FD, "with " & Get_Name_String (Scheduler) & ";");
      Put_Line (FD, "with PolyORB_HI_Generated.Activity;");
      Put_Line (FD, "");
      Put_Line (FD, "package " & Get_Name_String (Pkg_Name) & " is");
      Put_Line (FD, "   use PolyORB_HI_Generated.Activity;");
      Put_Line (FD, "");

      --  FIXME
      --  Should use primitives from the Ada backend
      --  Current form is code redundency

      Put_Line
        (FD,
         "   type " &
         Get_Name_String (Type_Name) &
         " is array (Integer range <>) of");
      Put_Line (FD, "     " & Get_Name_String (Mode_Type) & ";");
      Put_Line (FD, "");
      Put_Line
        (FD,
         "   package " &
         Get_Name_String (Inst_Pkg) &
         " is new " &
         Get_Name_String (Scheduler));

      Put_Line (FD, "     (" & Get_Name_String (Mode_Type) & ",");
      Put_Line (FD, "      " & Get_Name_String (Type_Name) & ",");

      --  Mode_Schedule : Mode_Table.Instance;
      It := First;

      while It <= Last (Mode_Schedule) loop
         M := Mode_Schedule.Table (It).Node;

         if It = First then
            Put (FD, "      (" & Get_Name_String (Name (Identifier (M))));
         else
            Put (FD, "       " & Get_Name_String (Name (Identifier (M))));
         end if;

         if It /= Last (Mode_Schedule) then
            Put_Line (FD, ",");
         else
            Put_Line (FD, "),");
         end if;

         It := It + 1;
      end loop;

      Put_Line (FD, "     " & Int'Image (Int (It - First)) & ",");
      Put_Line (FD, "      " & Get_Name_String (Function_name) & ");");
      Put_Line (FD, "");
      Put_Line (FD, "end  " & Get_Name_String (Pkg_Name) & ";");
      Close (FD);
   end Print_Schedule;

   ---------------------
   -- Get_Single_Mode --
   ---------------------

   function Get_Single_Mode (Call_Sequence : Node_Id) return Node_Id is
      pragma Assert (Kind (Call_Sequence) = K_Subprogram_Call_Sequence);

      M : Node_Id;
   begin
      if No (In_Modes (Call_Sequence))
        or else Is_Empty (Modes (In_Modes (Call_Sequence)))
      then
         return No_Node;
      else
         M := First_Node (Modes (In_Modes (Call_Sequence)));
         if Present (Next_Node (M)) then
            DE ("More than one mode in " &
               Get_Name_String (Name (Identifier (Call_Sequence))));
            raise Program_Error;
         else
            return ATN.Entity (M);
         end if;
      end if;
   end Get_Single_Mode;

   ----------
   -- Free --
   ----------

   procedure Free is
   begin
      Mode_Table.Free (Mode_Schedule);
      Initial_Mode := No_Node;
   end Free;

end Ocarina.Transfo.Fusions.Scheduler;
