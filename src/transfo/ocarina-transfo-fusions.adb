------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              O C A R I N A . T R A N S F O . F U S I O N S               --
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

with Ocarina.Transfo;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Debug;

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Transfo.Fusions.Scheduler;
with Errors;
with Ocarina.Namet;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Dynamic_Tables;
with Ocarina.Builder.AADL.Components;
with Ocarina.Builder.AADL.Components.Subprogram_Calls;
with Ocarina.Builder.AADL.Components.Subcomponents;
with Ocarina.Builder.AADL.Components.Connections;
with Ocarina.Builder.AADL.Components.Features;
with Ocarina.AADL_Values;

with Ocarina.Builder.AADL.Namespaces;
with Ocarina.Builder.AADL.Properties;
with Ocarina.Analyzer.AADL.Queries;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.Analyzer.AADL.Naming_Rules;
with Ocarina.Analyzer.AADL.Semantics;
with Locations;
with Charset;
with Utils;

with Ocarina.Instances.Queries;
with Ocarina.Backends.Messages;

package body Ocarina.Transfo.Fusions is
   use Ocarina.Transfo;

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Debug;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.Backends.Messages;

   use Errors;
   use Ocarina.Namet;
   use Locations;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   package Node_Counters is new GNAT.Dynamic_Tables
     (Table_Component_Type => Node_Counter,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,  -- # of elements
      Table_Increment      => 50); -- % increase

   package Node_Mapping is new GNAT.Dynamic_Tables
     (Table_Component_Type => Node_Association,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,  -- # of elements
      Table_Increment      => 50); -- % increase

   package Node_Priorities is new GNAT.Dynamic_Tables
     (Table_Component_Type => Node_Priority,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,  -- # of elements
      Table_Increment      => 50); -- % increase

   function Is_In_Call_Sequence
     (Call          : Name_Id;
      Call_Sequence : Node_Id) return Boolean;
   --  Return true if their is a suprogram call of identifier "Call" in the
   --  parameter-given call sequence

   function Is_Call_Sequence_Sporadic
     (Call_Seq, Thread : Node_Id) return Boolean;
   --  Return true if the call sequence depends of at least on event or
   --  event data port

   function Get_Entrypoint_Sporadic_Port
     (Call_Seq, Thread : Node_Id) return Node_Id;
   --  return the port which is teh entrypoint of call sequence call_seq,
   --  return no_node if call_seq has no entrypoint defined

   procedure Compute_Call_Sequences
     (Th_Inst_1, Th_Inst_2     : Node_Id;
      New_Thread_Impl          : Node_Id;
      Th1_Mapping, Th2_Mapping : Node_Mapping.Instance);
   --  Copy all call sequences in the new thread, creating new modes
   --  and assigning properties

   procedure Set_Flat_Entity_Reference (Node : Node_Id; Name : Name_Id);
   --  Set the entity reference in the specific case where the path
   --  is a single element

   function Declare_Wrapper_Subprogram (Owner_Thread : Node_Id) return Node_Id;
   --  Create a wrapper (declaration and implementation) for the
   --  thread passed though parameters.
   --  Return the component implementation,
   --  return no_node whenever declaration fail

   procedure Declare_Wrapper_Subprogram (Owner_Thread : Node_Id);
   pragma Unreferenced (Declare_Wrapper_Subprogram);
   --  Create a wrapper (declaration and implementation) for the
   --  thread passed though parameters.
   --  raise a program_error whenever declaration fail

   function Duplicate_Subcomponent
     (Old_Subcomp, Owner : Node_Id;
      Suffix             : String := "") return Node_Id;
   --  Duplicate the subcomponent

   function Duplicate_Process (Old_Proc : Node_Id) return Node_Id;
   --  Duplicate the process, minus connections and subcomponents

   procedure Clean_Type_Declaration (Old_Decl, Namespace : Node_Id);
   --  Delete the declaration of the old threads iff they are
   --  not referenced anymore
   procedure Clean_Implementation_Declaration (Old_Impl, Namespace : Node_Id);
   --  Delete the implementation of the old threads iff they are
   --  not referenced anymore
   procedure Clean_Declaration (Old_Decl, Namespace : Node_Id);
   --  Delete the (type/implementation) declaration of the old threads
   --  iff they are not referenced anymore

   procedure Exchange_Processes_In_System
     (System, Old_Proc_Subcomponent, New_Proc :     Node_Id;
      Success                                 : out Boolean);
   --  In the component system, replace the subcomponent
   --  Old_Proc_Subcomponent by a subcomponent containing New_Proc

   procedure Clean;
   --  Free all global variables

   procedure Copy_Feature_Core
     (Feature, New_Feature, Owner_Component : Node_Id;
      Suffix                                : String := "");
   --  Copy the information from a feature generic object to a new one

   function Find_New_Feature_Node
     (Old_Feature_Name : Name_Id;
      Parent           : Node_Id;
      Mapping          : Node_Mapping.Instance) return Node_Id;
   --  Find a new feature node using the old name as a key

   procedure Create_Connection
     (Src_Path, Dst_Path    : List_Id;
      Old_Cnx, Wrapper_impl : Node_Id;
      CT                    : Ocarina.ME_AADL.Connection_Type;
      New_Name              : Name_Id := No_Name);
   --  Create the local connection from src_path to dst_path,
   --  and add it to the wrapper

   procedure Create_Connection_End_Path
     (Path    : in out List_Id;
      Token_1 :        Name_Id;
      Token_2 :        Name_Id := No_Name);
   --  Create the list that contains the path tokens

   procedure Fusion_Parent_Connections
     (Old_Proc, New_Proc, Old_Thread_1, Old_Thread_2 :     Node_Id;
      Success                                        : out Boolean);
   --  Fusion connections declared in parent process
   --  FIXME : does not support more than one level of
   --  subcomponent addressing

   procedure Copy_All_Features
     (Features_List                  :        List_Id;
      Mapping                        : in out Node_Mapping.Instance;
      Thread_Component, Owner_Thread :        Node_Id);
   --  Copy all features in thread_component to the feature list
   --  For each of them, create a reference and associate a 0-value
   --  to it in the counter list. Will be used to count the number
   --  of connection supported by a feature.
   --  Will also increment the related counter

   procedure Increment_Related_Counter (Feature : Node_Id);
   --  Increment the counter corresponding to the feature

   procedure Clean_Unused_Features;
   pragma Unreferenced (Clean_Unused_Features);
   --  Delete all features whose counter is equal to 0

   procedure Copy_All_Subcomponents
     (Features_List             : List_Id;
      Thread_Impl, Owner_Thread : Node_Id);
   --  Copy all subcomponents from Thread_Impl to the feature_list.

   function Search_Subprogram_By_Name
     (Key          : Name_Id;
      Call_Mapping : Node_Mapping.Instance) return Node_Id;
   --  Find a subprogram from its name
   --  return No_Node if no such was found

   procedure Manage_Component_Properties
     (Old_Thread_1, Old_Thread_2, New_Thread :     Node_Id;
      Success                                : out Boolean;
      Is_Sporadic, Is_Periodic               :     Boolean := False);
   --  Fusion the properties from old threads to the fusionned one

   function Declare_Generated_Procedure
     (New_Thread : Node_Id;
      Proc_Str   : String) return Node_Id;
   --  Factorized code for declare_*

   function Declare_Mode_Scheduler (New_Thread : Node_Id) return Node_Id;
   --  Declare the AADL code corresponding to the mode scheduler
   --  associated to the fusioned thread. Returns the implementation.

   function Declare_Next_Period (New_Thread : Node_Id) return Node_Id;
   --  Declare the AADL code corresponding to the period iterator
   --  associated to the fusioned thread. Returns the implementation.

   procedure Declare_Scheduler_Property
     (Thread            : Node_Id;
      Subcomponent_Name : Name_Id);
   --  add the scheduler_name property for the thread

   procedure Set_Original_Name_Property
     (Thread            : Node_Id;
      Subcomponent_Name : Name_Id);

   procedure Add_Needed_Packages (Thread : Node_Id);
   --  Add packages needed for any fusion
   --  (currently only 'transformations')

   function Dot_To_Underscore (Str : String) return String_Access;

   procedure Set_WCET (E : Node_Id; Max_Duration : Natural);
   --  Set the thread or subprogram WCET

   function Get_WCET (S : Node_Id) return Unsigned_Long_Long;
   --  return a component WCET

   procedure Set_Period (S : Node_Id; Quantum : Natural);
   --  Set the quantum as subprogram period

   procedure Set_Priority (S : Node_Id; Prio : Natural);
   --  Set the subprogram priority

   procedure Set_Deadline (S : Node_Id; Value : Natural);
   --  Set a thread deadline

   procedure Put_Data_Subcomponent_Back
     (Old_Process  : Node_Id;
      Old_Thread_1 : Node_Id;
      Old_Thread_2 : Node_Id;
      New_Process  : Node_Id);
   --  Put back unfusioned subcomponents in the owner process

   function Get_Property_Association
     (Component : Node_Id;
      Property  : Name_Id) return Node_Id;
   --  Search a property association in component
   --  implementation and declaration

   Total_WCET : Natural;
   --  Sum the WCET of al subprogram executed in the merged thread

   Counters : Node_Counters.Instance;

   Priorities : Node_Priorities.Instance;

   AADL_Root : Node_Id := No_Node;
   --  Root of the AADL tree

   Instance_Name : Name_Id;
   --  The newly created instance name
   New_Process : Node_Id;
   --  The newly created process implementation
   Thread_Namespace : Node_Id;
   --  namespace of the first thread to be fusioned into (both old
   --  and new) processes
   Dummy_Internal_Type : Node_Id;

   Th1_Mapping : Node_Mapping.Instance;
   Th2_Mapping : Node_Mapping.Instance;
   --  Features

   Th1_Inst, Th2_Inst : String_Access;

   Mode_Scheduler : Node_Id;
   Next_Period    : Node_Id;
   Stop_Mode_Name : Name_Id;

   Cnx_Num : Int := 1;
   --  A counter of the number of connections created

   Dispatch_Protocol_Str : constant String := "dispatch_protocol";
   Deadline_Str          : constant String := "deadline";
   WCET_Str              : constant String := "compute_execution_time";
   Cheddar_Priority_Str  : constant String :=
     "cheddar_properties::fixed_priority";
   Deployment_Priority_Str : constant String := "deployment::priority";
   Scheduler_Call_Str      : constant String := "schedule";
   Iterator_Call_Str       : constant String := "next_period";
   Compute_Entrypoint_Str  : constant String :=
     "compute_entrypoint_call_sequence";
   CS_Period_Str : constant String := "transformations::call_sequence_period";
   Period_Str             : constant String := "period";
   Transfo_Priority_Str   : constant String := "transformations::priority";
   Raw_Priority_Str       : constant String := "priority";
   Raw_Fixed_Priority_Str : constant String := "fixed_priority";
   Transfo_Occurred_Str   : constant String :=
     "transformations::fusion_occurred";
   Raw_Occurred_Str     : constant String := "fusion_occurred";
   Priority_Shifter_Str : constant String :=
     "transformations::priority_shifter";
   Raw_Priority_Shifter_Str : constant String := "priority_shifter";

   Stop_Mode_Str           : constant String := "stop_mode";
   Transfo_Pkg_Str         : constant String := "transformations";
   Cheddar_Pkg_Str         : constant String := "cheddar_properties";
   Raw_Data_Str            : constant String := "data_representation";
   Data_Representation_Str : constant String :=
     "data_model::data_representation";
   Data_Model_Pkg_Str         : constant String := "data_model";
   Transfo_Scheduler_Name_Str : constant String :=
     "transformations::scheduler_name";
   Scheduler_Name_Str        : constant String := "scheduler_name";
   Transfo_Original_Name_Str : constant String :=
     "transformations::original_name";
   Original_Name_Str : constant String := "original_name";

   -----------------------
   -- Dot_To_Underscore --
   -----------------------

   function Dot_To_Underscore (Str : String) return String_Access is
      Len  : constant Natural       := Str'Length;
      Str2 : constant String_Access := new String'(Str);
   begin
      for I in 1 .. Len loop
         if Str (I) = '.' then
            Str2 (I) := '_';
         end if;
      end loop;

      return Str2;
   end Dot_To_Underscore;

   ---------------------------------
   -- Declare_Generated_Procedure --
   ---------------------------------

   function Declare_Generated_Procedure
     (New_Thread : Node_Id;
      Proc_Str   : String) return Node_Id
   is
      use Ocarina.Builder.AADL.Components;

      Decl    : Node_Id;
      Impl    : Node_Id;
      Id_Name : Name_Id :=
        Build_Unique_Name
          (Declarations (Namespace (New_Thread)),
           Get_String_Name (Proc_Str));
      Str : String_Access := Dot_To_Underscore (Get_Name_String (Id_Name));
   begin
      Id_Name := Get_String_Name (Str.all);

      --  Create the declaration

      Decl :=
        Add_New_Component_Type
          (No_Location,
           Make_Identifier (No_Location, Id_Name, Id_Name, No_Node),
           Namespace (New_Thread),
           Ocarina.ME_AADL.CC_Subprogram);
      if No (Decl) then
         DE ("Could not declare the mode scheduler");
         raise Program_Error;
      end if;

      --  Create the implementation

      Id_Name := Get_String_Name (Str.all & ".i");
      Impl    :=
        Add_New_Component_Implementation
          (No_Location,
           Make_Identifier (No_Location, Id_Name, Id_Name, No_Node),
           Namespace (New_Thread),
           Ocarina.ME_AADL.CC_Subprogram);
      Set_Component_Type_Identifier (Impl, Identifier (Decl));
      if No (Impl) then
         DE ("Could not implements the procedure");
         raise Program_Error;
      end if;

      --  Add properties

      declare
         use Ocarina.Builder.AADL.Properties;
         use Ocarina.AADL_Values;

         Lang : constant Name_Id := Get_String_Name ("source_language");
         Raw_Source_Name : constant Name_Id := Get_String_Name ("source_name");
         Source_Name     : constant Name_Id :=
           Get_String_Name ("transformations::source_name");
         Ada : constant Name_Id := Get_String_Name ("ada95");
         Src : constant Name_Id :=
           Get_String_Name
             (Get_Name_String (Name (Identifier (Namespace (New_Thread)))) &
              "_" &
              Get_Name_String (Instance_Name) &
              "_mode." &
              Proc_Str);
         S, S2 : Node_Id;
         Id    : Node_Id;
      begin
         S := New_Node (K_Property_Term, No_Location);
         Set_Identifier (S, Make_Identifier (No_Location, Ada, Ada, No_Node));
         Id := Make_Identifier (No_Location, Lang, Lang, No_Node);
         S2 := New_Node (K_Entity_Reference, No_Location);
         Set_Identifier
           (S2,
            Make_Identifier (No_Location, Lang, Lang, No_Node));
         Set_Full_Identifier (S2, Id);
         Id :=
           Add_New_Property_Association
             (Loc            => No_Location,
              Name           => Id,
              Property_Name  => S2,
              Container      => Impl,
              In_Binding     => No_Node,
              In_Modes       => No_Node,
              Property_Value => S,
              Is_Constant    => False,
              Is_Access      => False,
              Is_Additive    => False,
              Applies_To     => No_List);
         if No (Id) then
            raise Program_Error;
         end if;

         S := New_Node (K_Literal, No_Location);
         Set_Value (S, New_String_Value (Src));
         Id :=
           Make_Identifier (No_Location, Source_Name, Source_Name, No_Node);
         S2 := New_Node (K_Entity_Reference, No_Location);
         Set_Identifier
           (S2,
            Make_Identifier
              (No_Location,
               Raw_Source_Name,
               Raw_Source_Name,
               No_Node));
         Set_Namespace_Identifier
           (S2,
            Make_Identifier (No_Location, Transfo_Pkg, Transfo_Pkg, No_Node));
         Set_Full_Identifier (S2, Id);
         Id :=
           Add_New_Property_Association
             (Loc            => No_Location,
              Name           => Id,
              Property_Name  => S2,
              Container      => Impl,
              In_Binding     => No_Node,
              In_Modes       => No_Node,
              Property_Value => S,
              Is_Constant    => False,
              Is_Access      => False,
              Is_Additive    => False,
              Applies_To     => No_List);
         if No (Id) then
            raise Program_Error;
         end if;
      end;
      Free (Str);

      return Impl;
   end Declare_Generated_Procedure;

   ----------------------------
   -- Declare_Mode_Scheduler --
   ----------------------------

   function Declare_Mode_Scheduler (New_Thread : Node_Id) return Node_Id is
   begin
      return Declare_Generated_Procedure (New_Thread, "mode_scheduler");
   end Declare_Mode_Scheduler;

   -------------------------
   -- Declare_Next_Period --
   -------------------------

   function Declare_Next_Period (New_Thread : Node_Id) return Node_Id is
   begin
      return Declare_Generated_Procedure (New_Thread, "next_period");
   end Declare_Next_Period;

   -------------------------
   -- Add_Needed_Packages --
   -------------------------

   procedure Add_Needed_Packages (Thread : Node_Id) is
      use Ocarina.Analyzer.AADL.Finder;

      pragma Assert (Kind (Thread) = K_Component_Implementation);

      Transfo    : constant Name_Id := Get_String_Name ("transformations");
      Transfo_Id : constant Node_Id :=
        Make_Identifier (No_Location, Transfo, Transfo, No_Node);
      Pkg : constant Node_Id := Namespace (Thread);
   begin
      if not Find_In_Import_Declaration (Pkg, Transfo_Id) then
         declare
            Import_Decl : constant Node_Id :=
              New_Node (K_Import_Declaration, No_Location);
            Visibility_Decl : constant Node_Id :=
              New_Node (K_Name_Visibility_Declaration, No_Location);
         begin
            if Is_Empty (Declarations (Pkg)) then
               Set_Declarations (Pkg, New_List (K_List_Id, No_Location));
            end if;

            Set_List_Items
              (Visibility_Decl,
               New_List (K_List_Id, No_Location));
            Set_List_Items (Import_Decl, New_List (K_List_Id, No_Location));
            Append_Node_To_List (Transfo_Id, List_Items (Import_Decl));
            Append_Node_To_List (Import_Decl, List_Items (Visibility_Decl));
            Push_Node_To_List (Visibility_Decl, Declarations (Pkg));
         end;
      end if;
   end Add_Needed_Packages;

   --------------------------------
   -- Set_Original_Name_Property --
   --------------------------------

   procedure Set_Original_Name_Property
     (Thread            : Node_Id;
      Subcomponent_Name : Name_Id)
   is
      pragma Assert (Kind (Thread) = K_Component_Implementation);
      use Ocarina.AADL_Values;
      use Ocarina.Builder.AADL.Properties;

      N      : Name_Id;
      L      : constant Node_Id := New_Node (K_Literal, No_Location);
      S2, Id : Node_Id;
   begin
      N :=
        Get_String_Name
          (Get_Name_String (Name (Identifier (Namespace (Thread)))) &
           "_" &
           Get_Name_String (Subcomponent_Name));

      S2 := New_Node (K_Entity_Reference, No_Location);
      Id :=
        Make_Identifier
          (No_Location,
           Transfo_Original_Name,
           Transfo_Original_Name,
           No_Node);
      Set_Identifier
        (S2,
         Make_Identifier
           (No_Location,
            Raw_Original_Name,
            Raw_Original_Name,
            No_Node));
      Set_Namespace_Identifier
        (S2,
         Make_Identifier (No_Location, Transfo_Pkg, Transfo_Pkg, No_Node));
      Set_Full_Identifier
        (S2,
         Make_Identifier
           (No_Location,
            Transfo_Original_Name,
            Transfo_Original_Name,
            No_Node));

      Set_Value (L, New_String_Value (N));

      Id :=
        Add_New_Property_Association
          (Loc            => No_Location,
           Name           => Id,
           Property_Name  => S2,
           Container      => Thread,
           In_Binding     => No_Node,
           In_Modes       => No_Node,
           Property_Value => L,
           Is_Constant    => False,
           Is_Access      => False,
           Is_Additive    => False,
           Applies_To     => No_List);
      if No (Id) then
         raise Program_Error;
      end if;
   end Set_Original_Name_Property;

   --------------------------------
   -- Declare_Scheduler_Property --
   --------------------------------

   procedure Declare_Scheduler_Property
     (Thread            : Node_Id;
      Subcomponent_Name : Name_Id)
   is
      pragma Assert (Kind (Thread) = K_Component_Implementation);
      use Ocarina.AADL_Values;
      use Ocarina.Builder.AADL.Properties;

      N      : Name_Id;
      L      : constant Node_Id := New_Node (K_Literal, No_Location);
      S2, Id : Node_Id;
   begin
      N :=
        Get_String_Name
          (Get_Name_String (Name (Identifier (Namespace (Thread)))) &
           "_" &
           Get_Name_String (Subcomponent_Name) &
           "_Scheduler_Instance");

      S2 := New_Node (K_Entity_Reference, No_Location);
      Id :=
        Make_Identifier
          (No_Location,
           Transfo_Scheduler_Name,
           Transfo_Scheduler_Name,
           No_Node);
      Set_Identifier
        (S2,
         Make_Identifier
           (No_Location,
            Raw_Scheduler_Name,
            Raw_Scheduler_Name,
            No_Node));
      Set_Namespace_Identifier
        (S2,
         Make_Identifier (No_Location, Transfo_Pkg, Transfo_Pkg, No_Node));
      Set_Full_Identifier
        (S2,
         Make_Identifier
           (No_Location,
            Transfo_Scheduler_Name,
            Transfo_Scheduler_Name,
            No_Node));

      Set_Value (L, New_String_Value (N));

      Id :=
        Add_New_Property_Association
          (Loc            => No_Location,
           Name           => Id,
           Property_Name  => S2,
           Container      => Thread,
           In_Binding     => No_Node,
           In_Modes       => No_Node,
           Property_Value => L,
           Is_Constant    => False,
           Is_Access      => False,
           Is_Additive    => False,
           Applies_To     => No_List);
      if No (Id) then
         raise Program_Error;
      end if;
   end Declare_Scheduler_Property;

   --------------------
   -- Fusion_Threads --
   --------------------

   procedure Fusion_Threads
     (Root                              :     Node_Id;
      Owner_Process, Thread_1, Thread_2 :     Name_Id;
      New_Thread                        : out Node_Id;
      Success                           : out Boolean)
   is
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      Proc_Str        : constant String := Get_Name_String (Owner_Process);
      Proc            : Node_Id;
      Th_1            : Node_Id;
      Th_2            : Node_Id;
      Th1_Impl        : Node_Id;
      Th2_Impl        : Node_Id;
      Id              : Node_Id;
      Thread_Impl     : Node_Id;
      Th_Namespace    : Node_Id;
      Thread_Impl_Str : String_Access;
   begin
      AADL_Root := Root;

      --  1/ Search the AADL process instance

      Proc := Search_Process_By_Name (Proc_Str);
      if No (Proc) then
         Success := False;
         DE ("Could not found process instance " & Proc_Str);
         return;
      end if;

      --  2/ Search for the threads

      Th_1 := Search_Thread_By_Name (Proc, Get_Name_String (Thread_1));
      if No (Th_1) then
         Success := False;
         DE ("Could not found thread instance " &
            Get_Name_String (Thread_1) &
            " in process " &
            Proc_Str);
         return;
      end if;

      Th_2 := Search_Thread_By_Name (Proc, Get_Name_String (Thread_2));
      if No (Th_2) then
         Success := False;
         DE ("Could not found thread instance " &
            Get_Name_String (Thread_2) &
            " in process " &
            Proc_Str);
         return;
      end if;

      --  FIXME
      --  check that all namespace definition is done relatively to
      --  th_1.

      Thread_Namespace :=
        Namespace_Identifier
          (Entity_Ref (AIN.Corresponding_Declaration (Th_1)));

      Th1_Inst := new String'(Get_Name_String (Thread_1));
      Th2_Inst := new String'(Get_Name_String (Thread_2));

      Th1_Impl :=
        AIN.Corresponding_Declaration (AIN.Corresponding_Instance (Th_1));
      Th2_Impl :=
        AIN.Corresponding_Declaration (AIN.Corresponding_Instance (Th_2));

      Instance_Name :=
        Build_Unique_Subcomponent_Name
          (AIN.Corresponding_Declaration (Proc),
           Thread_Prefix);

      --  3/ Fusion the 2 threads

      declare
         use Ocarina.Builder.AADL.Components;

         Th1_Decl : constant Node_Id :=
           Corresponding_Entity (Component_Type_Identifier (Th1_Impl));
         Th2_Decl : constant Node_Id :=
           Corresponding_Entity (Component_Type_Identifier (Th2_Impl));
         Thread_Name : constant Name_Id :=
           Build_Unique_Component_Name
             (Namespace (Th1_Decl),
              Concat_Names (Th1_Decl, Th2_Decl));
         Thread_Comp : Node_Id;
         Impl_Name   : Name_Id;
         L           : List_Id;

         pragma Assert
           ((Kind (Th1_Decl) = Kind (Th2_Decl))
            and then (Kind (Th1_Decl) = K_Component_Type));
      begin
         --  Add the transformation package

         Add_Needed_Packages (Th1_Impl);

         --  3.1/ Create a new thread component declaration

         Th_Namespace := Namespace (Th1_Decl);
         pragma Assert (Th_Namespace = Namespace (Th2_Decl));

         --  3.1.1/ Create the new component node and identifier

         Id :=
           Make_Identifier (No_Location, Thread_Name, Thread_Name, No_Node);
         Thread_Comp :=
           Add_New_Component_Type
             (No_Location,
              Id,
              Th_Namespace,
              CC_Thread,
              Is_Private => False);
         Set_Identifier (Thread_Comp, Id);

         --  3.1.2/ Copy all the features from the old threads

         L := New_List (K_List_Id, No_Location);
         Copy_All_Features (L, Th1_Mapping, Th1_Decl, Thread_Comp);
         Copy_All_Features (L, Th2_Mapping, Th2_Decl, Thread_Comp);
         Set_Features (Thread_Comp, L);

         --  3.1.3/ Copy the properties

         Manage_Component_Properties
           (Th1_Decl,
            Th2_Decl,
            Thread_Comp,
            Success);
         if not Success then
            return;
         end if;

         --  3.1.4/  Remove unused thread type declarations in namepsace

         Clean_Type_Declaration (Th1_Decl, Th_Namespace);
         Clean_Type_Declaration (Th2_Decl, Th_Namespace);

         --  3.2/ Create the corresponding implementation

         declare
            use Ocarina.Builder.AADL.Components.Subprogram_Calls;

            Impl_Id : Node_Id;
         begin
            --  3.2.1/ Build the identifier and link to the component type

            Id :=
              Make_Identifier (No_Location, Thread_Name, Thread_Name, No_Node);
            Set_Corresponding_Entity (Id, Thread_Comp);

            Thread_Impl_Str :=
              new String'(Get_Name_String (Thread_Name) & ".i");
            Impl_Name := Get_String_Name (Thread_Impl_Str.all);
            Impl_Id   :=
              Make_Identifier (No_Location, Impl_Name, Impl_Name, No_Node);

            --  3.2.2/ Create the implementation node

            Thread_Impl :=
              Add_New_Component_Implementation
                (No_Location,
                 Impl_Id,
                 Th_Namespace,
                 CC_Thread);
            Set_Component_Type_Identifier
              (Thread_Impl,
               Identifier (Thread_Comp));

            --  3.2.3/ Copy subcomponents from both threads

            L := New_List (K_List_Id, No_Location);
            Copy_All_Subcomponents (L, Th1_Impl, Thread_Impl);
            Copy_All_Subcomponents (L, Th2_Impl, Thread_Impl);
            Set_Subcomponents (Thread_Impl, L);

            --  3.2.4/ Create the related mode scheduler and
            --         Iterator

            Mode_Scheduler := Declare_Mode_Scheduler (Thread_Impl);
            Next_Period    := Declare_Next_Period (Thread_Impl);

            --  3.3.5/ Create a deep copy of the parent process,
            --  with *no* connections or subcomponents

            New_Process :=
              Duplicate_Process (AIN.Corresponding_Declaration (Proc));

            --  3.2.6/ Copy subprogram calls from both threads

            Compute_Call_Sequences
              (AIN.Corresponding_Instance (Th_1),
               AIN.Corresponding_Instance (Th_2),
               Thread_Impl,
               Th1_Mapping,
               Th2_Mapping);

            --  3.2.7/ Put back data subcomponents that are either :
            --  * _not_ priority shifter
            --  * priority shifter connected to a non-fusionned thread

            Put_Data_Subcomponent_Back
              (AIN.Corresponding_Declaration (Proc),
               Th_1,
               Th_2,
               New_Process);

            --  3.2.6/ Annexes

            --  FIXME TODO
         end;
      end;

      --  3.3.x/ Delete unused thread implementation in namespace

      Clean_Implementation_Declaration (Th1_Impl, Th_Namespace);
      Clean_Implementation_Declaration (Th2_Impl, Th_Namespace);

      --  3.3.x/ Add *used* thread implementation in the new process

      declare
         Switcher : constant Name_Id := Get_String_Name ("priority_switcher_");
         Inst     : Node_Id          :=
           First_Node (Subcomponents (AIN.Corresponding_Declaration (Proc)));
         Cp : Node_Id;
      begin
         while Present (Inst) loop
            if Name (Identifier (Inst)) /= Thread_1
              and then Name (Identifier (Inst)) /= Thread_2
            then
               declare
                  use Utils;
                  N2       : constant Node_Id := Entity (Entity_Ref (Inst));
                  Obj_Name : constant Name_Id := Name (Identifier (N2));
               begin
                  if not Is_Prefix (Switcher, Obj_Name) then
                     Cp := Duplicate_Subcomponent (Inst, New_Process);
                     Append_Node_To_List (Cp, Subcomponents (New_Process));
                  end if;
               end;
            end if;
            Inst := Next_Node (Inst);
         end loop;
      end;

      --  3.3.x/ Register the new process implementation

      Success :=
        Ocarina.Builder.AADL.Namespaces.Add_Declaration
          (Namespace (AIN.Corresponding_Declaration (Proc)),
           New_Process);

      if not Success then
         raise Program_Error;
      end if;

      --  3.3.y/ Register the component implementation in the
      --  new process

      declare
         use Ocarina.Builder.AADL.Components.Subcomponents;

         SC        : Node_Id;
         Impl_Name : Name_Id;
         E : constant Node_Id := New_Node (K_Entity_Reference, No_Location);
         C, V      : Node_Id;
         Th1_Ref   : constant Node_Id :=
           Entity_Ref (AIN.Corresponding_Declaration (Th_1));
         pragma Unreferenced (V);
      begin
         Id :=
           Make_Identifier
             (No_Location,
              Instance_Name,
              Instance_Name,
              No_Node);
         SC :=
           Add_New_Subcomponent
             (Loc                 => No_Location,
              Name                => Id,
              Comp_Impl           => New_Process,
              Is_Refinement       => False,
              Category            => CC_Thread,
              In_Modes            => No_Node,
              Prototypes_Bindings => No_List);

         --  Create the new entity reference

         Set_Namespace_Path (E, Namespace_Path (Th1_Ref));
         Set_Path (E, Path (Th1_Ref));
         V := Remove_Last_Node_From_List (Path (E));
         C :=
           Make_Identifier
             (No_Location,
              Instance_Name,
              Instance_Name,
              No_Node);
         Append_Node_To_List (C, Path (E));
         Set_Namespace_Path (E, No_List);

         if Present (Thread_Namespace) then
            C :=
              Make_Identifier
                (No_Location,
                 Name (Thread_Namespace),
                 Display_Name (Thread_Namespace),
                 E);
            Set_Namespace_Identifier (E, C);
         else
            Set_Namespace_Identifier (E, No_Node);
         end if;
         Impl_Name := Get_String_Name (Thread_Impl_Str.all);
         Set_Identifier
           (E,
            Make_Identifier (No_Location, Impl_Name, Impl_Name, E));
         Set_Entity_Ref (SC, E);

         --  Add reference to the Ada package containing the schedule

         Declare_Scheduler_Property (Thread_Impl, Instance_Name);
         Set_Original_Name_Property (Thread_Impl, Instance_Name);

         --  Fusion connections

         Fusion_Parent_Connections
           (AIN.Corresponding_Declaration (Proc),
            New_Process,
            Th1_Impl,
            Th2_Impl,
            Success);
         if not Success then
            Clean;
            return;
         end if;

         --  Remove the old process if it is no more referenced
         --  (that is, if their was only one instance before fusion)

         Clean_Declaration
           (AIN.Corresponding_Declaration (Proc),
            Th_Namespace);

         --  Add an instance of the new process in the parent system,
         --  and remove the subcomponent corresponding to the previous
         --  process

         Exchange_Processes_In_System
           (AIN.Corresponding_Declaration
              (AIN.Parent_Component (AIN.Parent_Subcomponent (Proc))),
            AIN.Corresponding_Declaration (AIN.Parent_Subcomponent (Proc)),
            New_Process,
            Success);

         --  Delete old features that are not used anymore

         --  Clean_Unused_Features;

         --  Delete the old process declaration if it is not used
         --  anymore

         C := AIN.Corresponding_Declaration (Proc);
         if AINU.Length (ATN.Instances (C)) = 1 then
            Remove_Node_From_List (C, Declarations (Namespace (C)));
         end if;
      end;

      Free (Thread_Impl_Str);
      Clean;

      New_Thread := Thread_Impl;
   end Fusion_Threads;

   --------------------------------
   -- Put_Data_Subcomponent_Back --
   --------------------------------

   procedure Put_Data_Subcomponent_Back
     (Old_Process  : Node_Id;
      Old_Thread_1 : Node_Id;
      Old_Thread_2 : Node_Id;
      New_Process  : Node_Id)
   is
      T1 : constant Node_Id := AIN.Corresponding_Declaration (Old_Thread_1);
      T2 : constant Node_Id := AIN.Corresponding_Declaration (Old_Thread_2);

      S           : Node_Id;
      Cnx         : Node_Id;
      C           : Node_Id;
      Copy        : Boolean;
      Change_Name : Boolean;
      SC          : Node_Id;
   begin
      if not Is_Empty (Subcomponents (Old_Process))
        or else Is_Empty (Connections (Old_Process))
      then
         S   := First_Node (Subcomponents (Old_Process));
         Cnx := First_Node (Connections (Old_Process));
      else
         return;
      end if;

      while Present (S) loop
         Copy        := False;
         Change_Name := False;

         C := Cnx;
         while Present (C) loop
            declare
               use Ocarina.Builder.AADL.Components.Connections;
               use Ocarina.ME_AADL.AADL_Tree.Entities;

               Dst : constant Node_Id :=
                 Corresponding_Entity
                   (Item (First_Node (Path (Destination (C)))));
               Src : constant Node_Id :=
                 Corresponding_Entity (Item (First_Node (Path (Source (C)))));
               Cnx_Number : constant Name_Id :=
                 Build_Unique_Name
                   (Connections (New_Process),
                    Connection_Prefix);
               Dst_Item     : Node_Id := No_Node;
               E            : Node_Id;
               Local_Change : Boolean := False;
               C2           : Node_Id;
            begin
               if Kind (Dst) = K_Port_Spec then
                  Copy := False;

               else  --  case subcomponent
                  Dst_Item := Entity (Entity_Ref (Dst));

                  if Dst_Item /= Entity (Entity_Ref (T1))
                    and then Dst_Item /= Entity (Entity_Ref (T2))
                    and then Kind (Src) = K_Subcomponent
                    and then
                      Get_Category_Of_Component (Entity (Entity_Ref (Src))) =
                      CC_Data
                    and then Entity (Entity_Ref (Src)) = S
                  then
                     Copy := True;

                     Cnx_Num := Cnx_Num + 1;
                     if not Is_Empty (Subcomponents (New_Process)) then
                        SC := First_Node (Subcomponents (New_Process));
                        while Present (SC) loop
                           if Name (Identifier (SC)) =
                             Name (Identifier (S))
                           then
                              Local_Change := True;
                              exit;
                           end if;
                           SC := Next_Node (SC);
                        end loop;
                     end if;

                     if Local_Change then
                        declare
                           N : constant Name_Id :=
                             Get_String_Name
                               (Get_Name_String
                                  (Name (Identifier (Source (C)))) &
                                "_n");
                        begin
                           E := New_Node (K_Entity_Reference, No_Location);
                           Set_Identifier
                             (E,
                              Make_Identifier (No_Location, N, N, No_Node));
                           Set_Path (E, Path (Source (C)));
                           Set_Namespace_Path (E, Namespace_Path (Source (C)));
                           Set_Entity (E, Entity (Source (C)));
                        end;
                        Change_Name := True;
                     else
                        E := Source (C);
                     end if;

                     C2 :=
                       Add_New_Connection
                         (No_Location,
                          Make_Identifier
                            (No_Location,
                             Cnx_Number,
                             Cnx_Number,
                             No_Node),
                          New_Process,
                          Category    => CT_Access_Data,
                          Source      => E,
                          Destination => Destination (C));
                     if No (C2) then
                        raise Program_Error;
                     end if;
                  end if;
               end if;
            end;

            C := Next_Node (C);
         end loop;

         if Copy then
            declare
               S2 : constant Node_Id := New_Node (K_Subcomponent, No_Location);
            begin
               Set_Category (S2, Category (S));
               Set_In_Modes (S2, In_Modes (S));
               Set_Is_Refinement (S2, Is_Refinement (S));
               Set_Is_Implicit_Inverse (S2, Is_Implicit_Inverse (S));
               Set_Inversed_Entity (S2, Inversed_Entity (S));
               Set_Array_Dimensions (S2, No_Node);
               Set_Prototype_Bindings (S2, No_List);

               if Change_Name then
                  Copy_Feature_Core (S, S2, New_Process, "_n");
               else
                  Copy_Feature_Core (S, S2, New_Process);
               end if;
               Append_Node_To_List (S2, Subcomponents (New_Process));
            end;
         end if;

         S := Next_Node (S);
      end loop;
   end Put_Data_Subcomponent_Back;

   -------------------------------
   -- Is_Call_Sequence_Sporadic --
   -------------------------------

   function Is_Call_Sequence_Sporadic
     (Call_Seq, Thread : Node_Id) return Boolean
   is
   begin
      return Present (Get_Entrypoint_Sporadic_Port (Call_Seq, Thread));
   end Is_Call_Sequence_Sporadic;

   ----------------------------------
   -- Get_Entrypoint_Sporadic_Port --
   ----------------------------------

   function Get_Entrypoint_Sporadic_Port
     (Call_Seq, Thread : Node_Id) return Node_Id
   is
      use Ocarina.Instances.Queries;

      pragma Assert
        (Kind (Thread) = K_Component_Implementation
         and then Kind (Call_Seq) = K_Subprogram_Call_Sequence);
      N : Node_Id;
   begin
      --  Search in connections from event * ports

      if not Is_Empty (Connections (Thread)) then
         N := First_Node (Connections (Thread));
         while Present (N) loop
            declare
               Source_Port : constant Node_Id :=
                 Corresponding_Entity (Item (First_Node (Path (Source (N)))));
            begin
               --  If the source is a port, then we search the
               --  corresponding feature instance and check the
               --  port type

               if Kind (Source_Port) = K_Port_Spec
                 and then Is_In_Call_Sequence
                   (Name (Item (First_Node (Path (Destination (N))))),
                    Call_Seq)
               then
                  if ATN.Is_Event (Source_Port) then
                     return Source_Port;
                  end if;
               end if;
            end;
            N := ATN.Next_Node (N);
         end loop;
      end if;

      --  Search call sequence entrypoints

      declare
         use AIN;
      begin

         N :=
           AIN.First_Node
             (AIN.Features (AIN.First_Node (ATN.Instances (Thread))));
         while Present (N) loop
            if AIN.Kind (N) = AIN.K_Port_Spec_Instance then
               declare
                  Entrypoint : constant Name_Id :=
                    Get_String_Name ("compute_entrypoint_call_sequence");
                  R : Node_Id;
               begin
                  if Is_Defined_Reference_Property (N, Entrypoint) then
                     R := Get_Reference_Property (N, Entrypoint);
                     if AIN.Name (AIN.Identifier (R)) =
                       ATN.Name (ATN.Identifier (Call_Seq))
                     then
                        return AIN.Corresponding_Declaration (N);
                     end if;
                  end if;
               end;
            end if;
            N := AIN.Next_Node (N);
         end loop;
      end;

      return No_Node;
   end Get_Entrypoint_Sporadic_Port;

   -------------------------
   -- Is_In_Call_Sequence --
   -------------------------

   function Is_In_Call_Sequence
     (Call          : Name_Id;
      Call_Sequence : Node_Id) return Boolean
   is
      N : Node_Id := First_Node (Subprogram_Calls (Call_Sequence));
   begin
      while Present (N) loop
         --  Call sequences have no specific scope range, so subprogram
         --  calls names are unique in the thread.

         if Name (Identifier (N)) = Call then
            return True;
         end if;

         N := Next_Node (N);
      end loop;

      return False;
   end Is_In_Call_Sequence;

   ----------------------------
   -- Compute_Call_Sequences --
   ----------------------------

   procedure Compute_Call_Sequences
     (Th_Inst_1, Th_Inst_2     : Node_Id;
      New_Thread_Impl          : Node_Id;
      Th1_Mapping, Th2_Mapping : Node_Mapping.Instance)
   is
      function Create_Call_Sequence return Node_Id;
      --  Create a new call sequence for the current thread

      function Copy_Subprogram_Call
        (Call, Owner_Sequence : Node_Id;
         Num                  : Int) return Node_Id;
      --  Copy the subprogram call

      procedure Copy_Call_Sequence
        (Call_Seq, Old_Thread, New_Thread :     Node_Id;
         Feature_Mapping                  :     Node_Mapping.Instance;
         New_Call_Seq                     : out Node_Id;
         Relative_WCET                    : out Natural);
      --  Copy both subprogram calls and relatives connections

      procedure Add_Entrypoint
        (New_Call_Seq, New_Thread : Node_Id;
         Old_Call_Seq, Old_Thread : Node_Id;
         Mode_Binding             : Node_Id;
         Port_Mapping             : Node_Mapping.Instance);
      --  Link the call sequence to the corresponding entrypoint,
      --  and the correct mode

      procedure Copy_All_Call_Sequences
        (Thread_Impl              :        Node_Id;
         New_Thread_Impl          :        Node_Id;
         Periodic_Counter         : in out Int;
         Thread_Mapping           :        Node_Mapping.Instance;
         Priorities               : in out Node_Priorities.Instance;
         Build_Modes              :        Boolean;
         Is_Sporadic, Is_Periodic :    out Boolean);
      --  Copy all call sequences from a thread to the fusioned thread
      --  creating new modes when finding periodic call sequences

      procedure Add_Call_Sequence
        (Call_Seq_Src       :        Node_Id;
         Call_Seq           :        Node_Id;
         Subprogram_Mapping : in out Node_Mapping.Instance);
      --  Copy the content of call_seq_src into Call_seq

      procedure Append_Subprogram_Call
        (Call_Sequence, Subprogram : Node_Id;
         Label                     : String);
      --  Add a subprogram call referencing the label subprogram

      function Search_Parameter
        (Subprogram : Node_Id;
         Key        : Name_Id) return Node_Id;
      --  Return the mapping of a given parameter, or no_node

      function Create_Stop_Mode (New_Thread_Impl : Node_Id) return Node_Id;
      --  Create a stop mode in the thread

      function Find_Periodic_Sequences
        (Thread_1, Thread_2 : Node_Id) return Int;
      --  Return the number of periodic call sequences in the two
      --  fusioned threads

      procedure Register_Priorities
        (Thread_1, Thread_2 :        Node_Id;
         Prio               : in out Node_Priorities.Instance;
         Minimum            :    out Int;
         Maximum            :    out Int);
      --  Associate a priority to each call sequence,
      --  and return the minimum

      function Find_Corresponding_Priority
        (Prio     : Node_Priorities.Instance;
         Call_Seq : Node_Id) return Int;
      --  Find the priority associated to a given call sequence

      function Find_First_Object_With_Priority
        (Prio : Node_Priorities.Instance;
         P    : Int) return Node_Id;

      function Find_First_Object_With_Same_Priority
        (Prio : Node_Priorities.Instance;
         N    : Node_Id) return Node_Id;

      Call_Seq_Num : Int := 1;
      Call_Num     : Int := 1;
      Wrapper_Num  : Int := 1;
      Method_Num   : Int := 1;

      ----------------------------
      -- Append_Subprogram_Call --
      ----------------------------

      procedure Append_Subprogram_Call
        (Call_Sequence, Subprogram : Node_Id;
         Label                     : String)
      is
         use Ocarina.Builder.AADL.Components.Subprogram_Calls;

         Node : constant Node_Id := New_Node (K_Subprogram_Call, No_Location);
         Lab     : constant Name_Id := Get_String_Name (Label);
         Sp_Name : constant Name_Id := Name (Identifier (Subprogram));
      begin
         Set_Identifier (Node, Make_Identifier (No_Location, Lab, Lab, Node));
         Set_Flat_Entity_Reference (Node, Sp_Name);
         Set_Entity (Entity_Ref (Node), Subprogram);

         if not Add_Subprogram_Call (Call_Sequence, Node) then
            raise Program_Error;
         end if;
      end Append_Subprogram_Call;

      --------------------------
      -- Copy_Subprogram_Call --
      --------------------------

      function Copy_Subprogram_Call
        (Call, Owner_Sequence : Node_Id;
         Num                  : Int) return Node_Id
      is
         Node : constant Node_Id := New_Node (K_Subprogram_Call, No_Location);
         Id       : Node_Id;
         Concat   : String_Access;
         Str_Name : Name_Id;
      begin
         Set_Property_Scope (Node, New_Node (K_Scope_Definition, No_Location));
         Set_Properties (Node, ATN.Properties (Call));
         Set_Entity_Ref (Node, Entity_Ref (Call));
         Set_Container_Component (Node, Owner_Sequence);

         Concat   := new String'("sp" & Image (Num));
         Str_Name := Get_String_Name (Concat.all);
         Id       := Make_Identifier (No_Location, Str_Name, Str_Name, Node);
         Set_Identifier (Node, Id);

         return Node;
      end Copy_Subprogram_Call;

      ----------------------
      -- Search_Parameter --
      ----------------------

      function Search_Parameter
        (Subprogram : Node_Id;
         Key        : Name_Id) return Node_Id
      is
         use Utils;

         Decl : constant Node_Id :=
           Corresponding_Entity (Component_Type_Identifier (Subprogram));
         N : Node_Id;
      begin
         N := First_Node (Features (Decl));
         while Present (N) loop
            if Kind (N) = K_Parameter then
               if To_Lower (Name (Identifier (N))) = To_Lower (Key) then
                  return N;
               end if;
            end if;
            N := Next_Node (N);
         end loop;
         return No_Node;
      end Search_Parameter;

      ------------------------
      -- Copy_Call_Sequence --
      ------------------------

      procedure Copy_Call_Sequence
        (Call_Seq, Old_Thread, New_Thread :     Node_Id;
         Feature_Mapping                  :     Node_Mapping.Instance;
         New_Call_Seq                     : out Node_Id;
         Relative_WCET                    : out Natural)
      is
         use Ocarina.Builder.AADL.Components.Subprogram_Calls;

         pragma Assert (Kind (Old_Thread) = K_Component_Implementation);
         pragma Assert (Kind (New_Thread) = K_Component_Implementation);
         pragma Assert (Kind (Call_Seq) = K_Subprogram_Call_Sequence);

         procedure Copy_Connections
           (Component : Node_Id;
            Parent    : Node_Id := No_Node);

         Local_Subprogram_Mapping : Node_Mapping.Instance;
         Wrapper : constant Node_Id := Declare_Wrapper_Subprogram (New_Thread);
         Sp                       : Node_Id;
         Wrapper_Call_Seq         : Node_Id;
         Wrapper_Cs_Name : constant Name_Id := Get_String_Name ("CS1");
         Wrapper_Name             : Name_Id;
         Target_CS                : Node_Id          := Call_Seq;
         Is_Wrapper               : Boolean          := False;

         ----------------------
         -- Copy_Connections --
         ----------------------

         procedure Copy_Connections
           (Component : Node_Id;
            Parent    : Node_Id := No_Node)
         is
            use Ocarina.ME_AADL.AADL_Tree.Entities;

            Parent_Thread : Node_Id;
            Cnx           : Node_Id;
         begin
            if Is_Empty (Connections (Component)) then
               return;
            end if;

            if Get_Category_Of_Component (Component) = CC_Thread then
               Parent_Thread := Component;
            else
               Parent_Thread := Parent;
            end if;

            Cnx := First_Node (Connections (Component));
            while Present (Cnx) loop
               declare
                  use Ocarina.Builder.AADL.Components.Features;

                  Src : constant Node_Id := First_Node (Path (Source (Cnx)));
                  Dst : constant Node_Id :=
                    First_Node (Path (Destination (Cnx)));
                  Src_Feat   : Node_Id;
                  Dst_Feat   : Node_Id;
                  Src_Name   : Name_Id;
                  Dst_Name   : Name_Id;
                  Src_Path   : List_Id          := No_List;
                  Dst_Path   : List_Id          := No_List;
                  Param, C   : Node_Id;
                  Cnx_Number : constant Name_Id :=
                    Get_String_Name ("cnx_" & Image (Cnx_Num));
                  Ignore : Boolean := False;
               begin
                  if
                    (Kind (Corresponding_Entity (Item (Src))) /= K_Port_Spec
                     and then
                       Kind (Corresponding_Entity (Item (Src))) /=
                       K_Parameter
                     and then No
                       (Search_Subprogram_By_Name
                          (Name (Item (Src)),
                           Local_Subprogram_Mapping)))
                    or else
                    (Kind (Corresponding_Entity (Item (Dst))) /= K_Port_Spec
                     and then
                       Kind (Corresponding_Entity (Item (Dst))) /=
                       K_Parameter
                     and then No
                       (Search_Subprogram_By_Name
                          (Name (Item (Dst)),
                           Local_Subprogram_Mapping)))
                  then
                     Ignore := True;
                  end if;

                  if not Ignore
                    and then
                      Kind (Corresponding_Entity (Item (Src))) =
                      K_Port_Spec
                  then

                     Src_Feat :=
                       Find_New_Feature_Node
                         (Name (Item (Src)),
                          Parent_Thread,
                          Feature_Mapping);
                     Increment_Related_Counter (Src_Feat);
                     Src_Name := Name (Identifier (Src_Feat));
                     Create_Connection_End_Path (Src_Path, Src_Name);

                     --  Create a new in parameter to the wrapper, if
                     --  not already existing
                     --  Create a connexion from the thread port
                     --  toward the wrapper parameter

                     Param := Search_Parameter (Wrapper, Src_Name);
                     if No (Param) then

                        --  The connection actual type depends on the
                        --  thread destination, not only source

                        if Is_Event (Src_Feat)
                          and then
                            Kind (Corresponding_Entity (Item (Dst))) =
                            K_Port_Spec
                        then

                           Param :=
                             Add_New_Port_Spec
                               (No_Location,
                                Make_Identifier
                                  (No_Location,
                                   Src_Name,
                                   Src_Name,
                                   No_Node),
                                Corresponding_Entity
                                  (Component_Type_Identifier (Wrapper)),
                                Is_In (Src_Feat),
                                Is_Out (Src_Feat),
                                Is_Data (Src_Feat),
                                Is_Feature (Src_Feat),
                                True);
                        else

                           Param :=
                             Add_New_Parameter
                               (No_Location,
                                Make_Identifier
                                  (No_Location,
                                   Src_Name,
                                   Src_Name,
                                   No_Node),
                                Corresponding_Entity
                                  (Component_Type_Identifier (Wrapper)),
                                True,
                                False);
                        end if;
                        Set_Entity_Ref
                          (Param,
                           Entity_Ref (Corresponding_Entity (Item (Src))));
                     end if;
                     Cnx_Num := Cnx_Num + 1;
                     declare
                        use Ocarina.Builder.AADL.Components.Connections;

                        Cnx_Num_Name : constant Name_Id :=
                          Get_String_Name ("cnx_" & Image (Cnx_Num));
                        Src_Path_2 : List_Id := No_List;
                        Dst_Path_2 : List_Id := No_List;
                        CT         : Connection_Type;
                     begin
                        Create_Connection_End_Path
                          (Dst_Path_2,
                           Wrapper_Name,
                           Src_Name);
                        Create_Connection_End_Path (Src_Path_2, Src_Name);

                        if Kind (Param) = K_Parameter then
                           CT := CT_Parameter;
                        else
                           CT := CT_Port_Connection;
                        end if;
                        Create_Connection
                          (Src_Path_2,
                           Dst_Path_2,
                           No_Node,
                           New_Thread,
                           CT,
                           Cnx_Num_Name);
                     end;

                  elsif not Ignore
                    and then
                      Kind (Corresponding_Entity (Item (Src))) =
                      K_Parameter
                  then

                     --  In this case we have a wrapper parameter
                     --  so we must check out which port the wrapper is
                     --  connected to, add a corresponding new parameter
                     --  if one is not already existing, and add
                     --  connections

                     --  Find out connection in parent component

                     C := First_Node (Connections (Parent));
                     while Present (C) loop
                        if Name (Item (Last_Node (Path (Destination (C))))) =
                          Name (Item (Src))
                        then

                           --  Search related feature and create connection
                           --  end

                           Src_Feat :=
                             Find_New_Feature_Node
                               (Name
                                  (Item (Last_Node (Path (Destination (C))))),
                                Parent_Thread,
                                Feature_Mapping);
                           Increment_Related_Counter (Src_Feat);
                           Src_Name := Name (Identifier (Src_Feat));
                           Create_Connection_End_Path (Src_Path, Src_Name);
                           exit;
                        end if;
                        C := Next_Node (C);
                     end loop;

                     --  Add a new parameter to the wrapper if needed

                     Param := Search_Parameter (Wrapper, Src_Name);
                     if No (Param) then

                        --  The connection actual type depends on the
                        --  thread destination, not only source

                        if Is_Event (Src_Feat)
                          and then
                            Kind (Corresponding_Entity (Item (Dst))) =
                            K_Port_Spec
                        then

                           Param :=
                             Add_New_Port_Spec
                               (No_Location,
                                Make_Identifier
                                  (No_Location,
                                   Src_Name,
                                   Src_Name,
                                   No_Node),
                                Corresponding_Entity
                                  (Component_Type_Identifier (Wrapper)),
                                Is_In (Src_Feat),
                                Is_Out (Src_Feat),
                                Is_Data (Src_Feat),
                                Is_Feature (Src_Feat),
                                True);
                        else

                           Param :=
                             Add_New_Parameter
                               (No_Location,
                                Make_Identifier
                                  (No_Location,
                                   Src_Name,
                                   Src_Name,
                                   No_Node),
                                Corresponding_Entity
                                  (Component_Type_Identifier (Wrapper)),
                                True,
                                False);
                        end if;
                        Set_Entity_Ref
                          (Param,
                           Entity_Ref (Corresponding_Entity (Item (Src))));
                     end if;

                     --  Create a connection from the thread port
                     --  to the wrapper

                     Cnx_Num := Cnx_Num + 1;
                     declare
                        use Ocarina.Builder.AADL.Components.Connections;

                        Cnx_Num_Name : constant Name_Id :=
                          Get_String_Name ("cnx_" & Image (Cnx_Num));
                        Src_Path_2 : List_Id := No_List;
                        Dst_Path_2 : List_Id := No_List;
                        CT         : Connection_Type;
                     begin
                        Create_Connection_End_Path
                          (Dst_Path_2,
                           Wrapper_Name,
                           Src_Name);
                        Create_Connection_End_Path (Src_Path_2, Src_Name);

                        if Kind (Param) = K_Parameter then
                           CT := CT_Parameter;
                        else
                           CT := CT_Port_Connection;
                        end if;
                        Create_Connection
                          (Src_Path_2,
                           Dst_Path_2,
                           No_Node,
                           New_Thread,
                           CT,
                           Cnx_Num_Name);
                     end;
                  elsif not Ignore then
                     Sp :=
                       Search_Subprogram_By_Name
                         (Name (Item (Src)),
                          Local_Subprogram_Mapping);
                     if No (Sp) then
                        --  Then the connection does not concern the current
                        --  call sequence, so we ignore this case

                        Ignore := True;
                     else
                        Src_Name := Name (Sp);
                        Create_Connection_End_Path
                          (Src_Path,
                           Src_Name,
                           Name (Item (Next_Node (Src))));
                     end if;
                  end if;

                  if not Ignore
                    and then
                      Kind (Corresponding_Entity (Item (Dst))) =
                      K_Port_Spec
                  then

                     Dst_Feat :=
                       Find_New_Feature_Node
                         (Name (Item (Dst)),
                          Parent_Thread,
                          Feature_Mapping);
                     Increment_Related_Counter (Dst_Feat);
                     Dst_Name := Name (Identifier (Dst_Feat));
                     Create_Connection_End_Path (Dst_Path, Dst_Name);

                     --  Create a new in parameter to the wrapper, if not
                     --  already existing
                     --  Create a connexion from the thread port
                     --  toward the wrapper parameter

                     Param := Search_Parameter (Wrapper, Dst_Name);
                     if No (Param) then
                        if Is_Event (Dst_Feat) then

                           Param :=
                             Add_New_Port_Spec
                               (No_Location,
                                Make_Identifier
                                  (No_Location,
                                   Dst_Name,
                                   Dst_Name,
                                   No_Node),
                                Corresponding_Entity
                                  (Component_Type_Identifier (Wrapper)),
                                Is_In (Dst_Feat),
                                Is_Out (Dst_Feat),
                                Is_Data (Dst_Feat),
                                Is_Feature (Dst_Feat),
                                True);
                        else

                           Param :=
                             Add_New_Parameter
                               (No_Location,
                                Make_Identifier
                                  (No_Location,
                                   Dst_Name,
                                   Dst_Name,
                                   No_Node),
                                Corresponding_Entity
                                  (Component_Type_Identifier (Wrapper)),
                                False,
                                True);
                        end if;
                        Set_Entity_Ref
                          (Param,
                           Entity_Ref (Corresponding_Entity (Item (Dst))));
                     end if;
                     Cnx_Num := Cnx_Num + 1;
                     declare
                        use Ocarina.Builder.AADL.Components.Connections;

                        Cnx_Num_Name : constant Name_Id :=
                          Get_String_Name ("cnx_" & Image (Cnx_Num));
                        Src_Path_2 : List_Id := No_List;
                        Dst_Path_2 : List_Id := No_List;
                        CT         : Connection_Type;
                     begin
                        Create_Connection_End_Path
                          (Src_Path_2,
                           Wrapper_Name,
                           Dst_Name);
                        Create_Connection_End_Path (Dst_Path_2, Dst_Name);
                        if Kind (Param) = K_Parameter then
                           CT := CT_Parameter;
                        else
                           CT := CT_Port_Connection;
                        end if;
                        Create_Connection
                          (Src_Path_2,
                           Dst_Path_2,
                           No_Node,
                           New_Thread,
                           CT,
                           Cnx_Num_Name);
                     end;

                  elsif not Ignore
                    and then
                      Kind (Corresponding_Entity (Item (Dst))) =
                      K_Parameter
                  then

                     --  In this case we have a wrapper parameter
                     --  so we must check out which port the wrapper is
                     --  connected to, add a corresponding new parameter
                     --  if one is not already existing, and add
                     --  connections

                     --  Find out connection in parent component

                     C := First_Node (Connections (Parent));
                     while Present (C) loop
                        if Name (Item (Last_Node (Path (Source (C))))) =
                          Name (Item (Dst))
                        then

                           --  Search related feature and create connection
                           --  end

                           Dst_Feat :=
                             Find_New_Feature_Node
                               (Name (Item (Last_Node (Path (Source (C))))),
                                Parent_Thread,
                                Feature_Mapping);
                           Increment_Related_Counter (Dst_Feat);
                           Dst_Name := Name (Identifier (Dst_Feat));
                           Create_Connection_End_Path (Dst_Path, Dst_Name);
                           exit;
                        end if;
                        C := Next_Node (C);
                     end loop;

                     --  Add a new parameter to the wrapper if needed

                     Param := Search_Parameter (Wrapper, Dst_Name);
                     if No (Param) then
                        if Is_Event (Dst_Feat) then

                           Param :=
                             Add_New_Port_Spec
                               (No_Location,
                                Make_Identifier
                                  (No_Location,
                                   Dst_Name,
                                   Dst_Name,
                                   No_Node),
                                Corresponding_Entity
                                  (Component_Type_Identifier (Wrapper)),
                                Is_In (Dst_Feat),
                                Is_Out (Dst_Feat),
                                Is_Data (Dst_Feat),
                                Is_Feature (Dst_Feat),
                                True);
                        else

                           Param :=
                             Add_New_Parameter
                               (No_Location,
                                Make_Identifier
                                  (No_Location,
                                   Dst_Name,
                                   Dst_Name,
                                   No_Node),
                                Corresponding_Entity
                                  (Component_Type_Identifier (Wrapper)),
                                False,
                                True);
                        end if;
                        Set_Entity_Ref
                          (Param,
                           Entity_Ref (Corresponding_Entity (Item (Dst))));
                     end if;

                     --  Create a connection from the thread port
                     --  to the wrapper

                     Cnx_Num := Cnx_Num + 1;
                     declare
                        use Ocarina.Builder.AADL.Components.Connections;

                        Cnx_Num_Name : constant Name_Id :=
                          Get_String_Name ("cnx_" & Image (Cnx_Num));
                        Src_Path_2 : List_Id := No_List;
                        Dst_Path_2 : List_Id := No_List;
                        CT         : Connection_Type;
                     begin
                        Create_Connection_End_Path
                          (Src_Path_2,
                           Wrapper_Name,
                           Dst_Name);
                        Create_Connection_End_Path (Dst_Path_2, Dst_Name);

                        if Kind (Param) = K_Parameter then
                           CT := CT_Parameter;
                        else
                           CT := CT_Port_Connection;
                        end if;
                        Create_Connection
                          (Src_Path_2,
                           Dst_Path_2,
                           No_Node,
                           New_Thread,
                           CT,
                           Cnx_Num_Name);
                     end;

                  elsif not Ignore then
                     Sp :=
                       Search_Subprogram_By_Name
                         (Name (Item (Dst)),
                          Local_Subprogram_Mapping);
                     if No (Sp) then
                        --  Then the connection does not concern the
                        --  current call sequence, so we ignore this case

                        Ignore := True;
                     else
                        Dst_Name := Name (Sp);
                        Create_Connection_End_Path
                          (Dst_Path,
                           Dst_Name,
                           Name (Item (Next_Node (Dst))));
                     end if;
                  end if;

                  if not Ignore then
                     if Category (Cnx) =
                       Connection_Type'Pos (CT_Parameter)
                     then
                        Create_Connection
                          (Src_Path,
                           Dst_Path,
                           No_Node,
                           Wrapper,
                           CT_Parameter,
                           Cnx_Number);
                     else
                        Create_Connection
                          (Src_Path,
                           Dst_Path,
                           No_Node,
                           Wrapper,
                           CT_Port_Connection,
                           Cnx_Number);
                     end if;
                     Cnx_Num := Cnx_Num + 1;
                  end if;
               end;

               Cnx := Next_Node (Cnx);
            end loop;
         end Copy_Connections;
      begin
         New_Call_Seq := Create_Call_Sequence;

         --  1/ we check wheither the subprogram is actually a wrapper,
         --  or not

         if Length (Subprogram_Calls (Call_Seq)) = 1 then
            Sp :=
              Entity (Entity_Ref (First_Node (Subprogram_Calls (Call_Seq))));
            if Kind (Sp) = K_Component_Implementation
              and then Calls (Sp) /= No_List
              and then not Is_Empty (Calls (Sp))
            then
               Target_CS  := First_Node (Calls (Sp));
               Is_Wrapper := True;
            end if;
         end if;

         --  2/ we copy all subprogram calls except call to 'schedule',
         --  and keep track in the new list

         Node_Mapping.Init (Local_Subprogram_Mapping);

         Wrapper_Call_Seq :=
           Add_New_Subprogram_Call_Sequence
             (No_Location,
              Make_Identifier
                (No_Location,
                 Wrapper_Cs_Name,
                 Wrapper_Cs_Name,
                 No_Node),
              Wrapper);
         Add_Call_Sequence
           (Target_CS,
            Wrapper_Call_Seq,
            Local_Subprogram_Mapping);

         --  3/ We create the top-level call sequence

         Append_Subprogram_Call
           (New_Call_Seq,
            Wrapper,
            "main_" & Image (Wrapper_Num));
         Wrapper_Num  := Wrapper_Num + 1;
         Wrapper_Name :=
           Name (Identifier (First_Node (Subprogram_Calls ((New_Call_Seq)))));

         --  4/ We add the period to the call sequence

         declare
            use Ocarina.Builder.AADL.Properties;
            use Ocarina.Transfo.Fusions.Scheduler;
            use Ocarina.AADL_Values;

            CSP : Natural;
         begin
            CSP := Get_Call_Sequence_Period (Call_Seq);

            if CSP = 0 then
               if Is_Defined_Property (Old_Thread, Period) then
                  CSP := Natural (Get_Integer_Property (Old_Thread, Period));
               end if;
            end if;
            if CSP /= 0 then
               Set_Period (Wrapper, CSP);
            end if;
         end;

         --  5/ If the call sequence has a WCET set, we copy it,
         --     else we copy the parent thread WCET

         if Is_Wrapper then
            if Is_Defined_Property (Sp, WCET) then
               Relative_WCET := Natural (Get_WCET (Sp));
               Set_WCET (Wrapper, Relative_WCET);
            else
               Display_Located_Error
                 (Loc (Sp),
                  "WCET must be declared",
                  Fatal => True);
            end if;
         else
            if Is_Defined_Property (Old_Thread, WCET) then
               Relative_WCET := Natural (Get_WCET (Old_Thread));
               Set_WCET (Wrapper, Relative_WCET);
            else
               Display_Located_Error
                 (Loc (Old_Thread),
                  "WCET must be declared",
                  Fatal => True);
            end if;
         end if;

         --  6/ we search the connections

         if Is_Wrapper then
            --  Easy case, since all relevant connections are in the
            --  old wrapper

            Copy_Connections (Sp, Old_Thread);
         end if;

         if Connections (New_Thread) = No_List then
            Set_Connections (New_Thread, New_List (K_List_Id, No_Location));
         end if;
         Copy_Connections (Old_Thread);
         Node_Mapping.Free (Local_Subprogram_Mapping);
      end Copy_Call_Sequence;

      -----------------------
      -- Add_Call_Sequence --
      -----------------------

      procedure Add_Call_Sequence
        (Call_Seq_Src       :        Node_Id;
         Call_Seq           :        Node_Id;
         Subprogram_Mapping : in out Node_Mapping.Instance)
      is
         use Ocarina.Builder.AADL.Components.Subprogram_Calls;

         Sp, Sp_Cpy : Node_Id;
         Last       : constant Node_Id :=
           Last_Node (Subprogram_Calls (Call_Seq_Src));
      begin
         --  Copy all subprogram calls except the last one if its name
         --  is "schedule"

         Sp := First_Node (Subprogram_Calls (Call_Seq_Src));
         while Present (Sp) loop
            if Sp /= Last
              or else Sp = First_Node (Subprogram_Calls (Call_Seq_Src))
              or else Name (Identifier (Sp)) /= Scheduler_Call
            then

               Sp_Cpy := Copy_Subprogram_Call (Sp, Call_Seq, Call_Num);
               if not Add_Subprogram_Call (Call_Seq, Sp_Cpy) then
                  raise Program_Error;
               end if;

               declare
                  NA : Node_Association;
               begin
                  NA.Old_Node := Identifier (Sp);
                  NA.New_Node := Identifier (Sp_Cpy);
                  Node_Mapping.Append (Subprogram_Mapping, NA);
               end;

               Call_Num := Call_Num + 1;
            end if;
            Sp := Next_Node (Sp);
         end loop;
      end Add_Call_Sequence;

      --------------------
      -- Add_Entrypoint --
      --------------------

      procedure Add_Entrypoint
        (New_Call_Seq, New_Thread : Node_Id;
         Old_Call_Seq, Old_Thread : Node_Id;
         Mode_Binding             : Node_Id;
         Port_Mapping             : Node_Mapping.Instance)
      is
         use Ocarina.Builder.AADL.Properties;

         pragma Assert (Kind (New_Call_Seq) = K_Subprogram_Call_Sequence);
         pragma Assert (Kind (New_Thread) = K_Component_Implementation);
         pragma Assert (Kind (Old_Thread) = K_Component_Implementation);
         pragma Assert
           (Mode_Binding = No_Node or else Kind (Mode_Binding) = K_In_Modes);

         Port          : Node_Id          := No_Node;
         New_Feature   : Node_Id;
         New_Port_Name : Name_Id          := No_Name;
         S, S2, N, Id  : Node_Id;
         Id2           : Node_Id;
         L             : constant List_Id := New_List (K_List_Id, No_Location);
         L2            : List_Id          := No_List;
         CS_Name       : constant Name_Id := Name (Identifier (New_Call_Seq));
      begin
         --  A sporadic call sequence has exactly one in event or in
         --  event data connection
         --  A periodic call sequence has no in event nor in event data
         --  connection

         Port := Get_Entrypoint_Sporadic_Port (Old_Call_Seq, Old_Thread);

         Id :=
           Make_Identifier
             (No_Location,
              Compute_Entrypoint,
              Compute_Entrypoint,
              No_Node);
         S2 := New_Node (K_Entity_Reference, No_Location);
         Set_Identifier (S2, Id);
         if Present (Port) then

            --  Case sporadic

            New_Feature :=
              Find_New_Feature_Node
                (Name (Identifier (Port)),
                 Old_Thread,
                 Port_Mapping);
            if No (New_Feature) then
               DE ("new port " &
                  Get_Name_String (Name (Identifier (Port))) &
                  " not found");
               raise Program_Error;
            end if;
            New_Port_Name := Name (Identifier (New_Feature));
            L2            := New_List (K_List_Id, No_Location);
            Append_Node_To_List
              (Make_Identifier
                 (No_Location,
                  New_Port_Name,
                  New_Port_Name,
                  No_Node),
               L2);
            Id2 := New_Node (K_Contained_Element_Path, No_Location);
            Set_List_Items (Id2, L2);
            L2 := New_List (K_List_Id, No_Location);
            Append_Node_To_List (Id2, L2);
         end if;

         --  Create the property

         S := Make_Identifier (No_Location, CS_Name, CS_Name, No_Node);
         Append_Node_To_List (S, L);
         N := New_Node (K_Contained_Element_Path, No_Location);
         Set_List_Items (N, L);
         S := New_Node (K_Reference_Term, No_Location);
         Set_Reference_Term (S, N);
         Set_Full_Identifier (S2, Id);
         Id :=
           Add_New_Property_Association
             (Loc            => No_Location,
              Name           => Id,
              Property_Name  => S2,
              Container      => New_Thread,
              In_Binding     => No_Node,
              In_Modes       => Mode_Binding,
              Property_Value => S,
              Is_Constant    => False,
              Is_Access      => False,
              Is_Additive    => False,
              Applies_To     => L2);
         if No (Id) then
            raise Program_Error;
         end if;
      end Add_Entrypoint;

      --------------------------
      -- Create_Call_Sequence --
      --------------------------

      function Create_Call_Sequence return Node_Id is
         New_Call_Seq  : Node_Id;
         Call_Seq_Name : constant Name_Id :=
           Get_String_Name ("cs_" & Image (Call_Seq_Num));
      begin

         New_Call_Seq := New_Node (K_Subprogram_Call_Sequence, No_Location);
         Set_Identifier
           (New_Call_Seq,
            Make_Identifier
              (No_Location,
               Call_Seq_Name,
               Call_Seq_Name,
               New_Call_Seq));
         Call_Seq_Num := Call_Seq_Num + 1;

         return New_Call_Seq;
      end Create_Call_Sequence;

      ----------------------
      -- Create_Stop_Mode --
      ----------------------

      function Create_Stop_Mode (New_Thread_Impl : Node_Id) return Node_Id is
         New_Mode : constant Node_Id :=
           New_Node (K_Mode, Loc (New_Thread_Impl));
         New_Call_Seq : constant Node_Id := Create_Call_Sequence;
         Wrapper      : constant Node_Id :=
           Declare_Wrapper_Subprogram (New_Thread_Impl);
      begin
         --  Add the stop mode to the thread mode list

         Set_Container_Component (New_Mode, New_Thread_Impl);
         Set_Identifier
           (New_Mode,
            Make_Identifier
              (No_Location,
               Stop_Mode_Name,
               Stop_Mode_Name,
               New_Mode));
         Append_Node_To_List (New_Mode, Modes (New_Thread_Impl));

         --  Add an entry for the stop mode in the thread behaviour
         --  Only call the mode scheduler

         declare
            use Ocarina.Builder.AADL.Components;
            use Ocarina.Builder.AADL.Components.Subprogram_Calls;

            CS_Name   : constant Name_Id := Name (Identifier (New_Call_Seq));
            Mode_List : Node_Id          := No_Node;
            Mode_Ref  : constant Node_Id :=
              New_Node (K_Entity_Reference, Loc (New_Thread_Impl));
            Wrapper_CS_Name  : constant Name_Id := Get_String_Name ("cs");
            Wrapper_Call_Seq : Node_Id;
         begin
            Wrapper_Call_Seq :=
              Add_New_Subprogram_Call_Sequence
                (No_Location,
                 Make_Identifier
                   (No_Location,
                    Wrapper_CS_Name,
                    Wrapper_CS_Name,
                    No_Node),
                 Wrapper);
            Append_Subprogram_Call
              (Wrapper_Call_Seq,
               Mode_Scheduler,
               Get_Name_String (Scheduler_Call));
            Append_Subprogram_Call
              (Wrapper_Call_Seq,
               Next_Period,
               Get_Name_String (Iterator_Call));
            Append_Subprogram_Call (New_Call_Seq, Wrapper, "Next_Period");
            Mode_List := New_Node (K_In_Modes, Loc (New_Call_Seq));
            Set_Modes (Mode_List, New_List (K_List_Id, Loc (New_Call_Seq)));
            Set_Identifier
              (Mode_Ref,
               Make_Identifier
                 (Loc (New_Call_Seq),
                  Stop_Mode_Name,
                  Stop_Mode_Name,
                  No_Node));
            Set_Entity (Mode_Ref, New_Mode);
            Append_Node_To_List (Mode_Ref, Modes (Mode_List));
            Set_In_Modes (New_Call_Seq, Mode_List);

            if not Add_Subprogram_Call_Sequence
                (New_Thread_Impl,
                 New_Call_Seq)
            then
               raise Program_Error;
            end if;

            --  Add an artificial low priority
            --  Should be the maximum of the thread call sequences
            --  priorities

            Set_Priority
              (Wrapper,
               Natural (Ocarina.Transfo.Fusions.Scheduler.Min_Priority));

            --  Add en entry point to the stop mode

            declare
               use Ocarina.Builder.AADL.Properties;

               Mode_Ref : constant Node_Id :=
                 New_Node (K_Entity_Reference, Loc (New_Thread_Impl));
               L : constant List_Id := New_List (K_List_Id, No_Location);
               S, S2, Id, N : Node_Id;
            begin
               Id :=
                 Make_Identifier
                   (No_Location,
                    Compute_Entrypoint,
                    Compute_Entrypoint,
                    No_Node);
               S2 := New_Node (K_Entity_Reference, No_Location);
               Set_Identifier (S2, Id);
               S := Make_Identifier (No_Location, CS_Name, CS_Name, No_Node);
               Append_Node_To_List (S, L);
               N := New_Node (K_Contained_Element_Path, No_Location);
               Set_List_Items (N, L);
               S := New_Node (K_Reference_Term, No_Location);
               Set_Reference_Term (S, N);
               Mode_List := New_Node (K_In_Modes, Loc (New_Call_Seq));
               Set_Modes (Mode_List, New_List (K_List_Id, Loc (New_Call_Seq)));
               Set_Identifier
                 (Mode_Ref,
                  Make_Identifier
                    (Loc (New_Call_Seq),
                     Stop_Mode_Name,
                     Stop_Mode_Name,
                     No_Node));
               Append_Node_To_List (Mode_Ref, Modes (Mode_List));
               Set_Full_Identifier (S2, Id);
               Id :=
                 Add_New_Property_Association
                   (Loc            => No_Location,
                    Name           => Id,
                    Property_Name  => S2,
                    Container      => New_Thread_Impl,
                    In_Binding     => No_Node,
                    In_Modes       => Mode_List,
                    Property_Value => S,
                    Is_Constant    => False,
                    Is_Access      => False,
                    Is_Additive    => False,
                    Applies_To     => No_List);
               if No (Id) then
                  raise Program_Error;
               end if;
            end;
         end;

         return First_Node (Subprogram_Calls (New_Call_Seq));
      end Create_Stop_Mode;

      -----------------------------
      -- Find_Periodic_Sequences --
      -----------------------------

      function Find_Periodic_Sequences
        (Thread_1, Thread_2 : Node_Id) return Int
      is
         Res      : Int := 0;
         Call_Seq : Node_Id;
      begin
         Call_Seq := ATN.First_Node (ATN.Calls (Thread_1));
         while Present (Call_Seq) loop
            if not Is_Call_Sequence_Sporadic (Call_Seq, Thread_1) then
               Res := Res + 1;
            end if;
            Call_Seq := Next_Node (Call_Seq);
         end loop;

         Call_Seq := ATN.First_Node (ATN.Calls (Thread_2));
         while Present (Call_Seq) loop
            if not Is_Call_Sequence_Sporadic (Call_Seq, Thread_2) then
               Res := Res + 1;
            end if;
            Call_Seq := Next_Node (Call_Seq);
         end loop;

         return Res;
      end Find_Periodic_Sequences;

      ---------------------------------
      -- Find_Corresponding_Priority --
      ---------------------------------

      function Find_Corresponding_Priority
        (Prio     : Node_Priorities.Instance;
         Call_Seq : Node_Id) return Int
      is
         use Node_Priorities;
         use Utils;

         It : Natural := First;
      begin
         --  We search the current node

         while It <= Last (Prio) loop
            if Prio.Table (It).Node = Call_Seq then

               return Prio.Table (It).Cnt;
            end if;
            It := It + 1;
         end loop;

         return 0;
      end Find_Corresponding_Priority;

      -------------------------------------
      -- Find_First_Object_With_Priority --
      -------------------------------------

      function Find_First_Object_With_Priority
        (Prio : Node_Priorities.Instance;
         P    : Int) return Node_Id
      is
         use Node_Priorities;
         use Utils;

         It : Natural := First;
      begin
         while It <= Last (Prio) loop
            if Prio.Table (It).Cnt = P then
               return Prio.Table (It).Object;
            end if;
            It := It + 1;
         end loop;
         return No_Node;
      end Find_First_Object_With_Priority;

      ------------------------------------------
      -- Find_First_Object_With_Same_Priority --
      ------------------------------------------

      function Find_First_Object_With_Same_Priority
        (Prio : Node_Priorities.Instance;
         N    : Node_Id) return Node_Id
      is
         use Node_Priorities;
         use Utils;

         It : Natural := First;
      begin
         --  We search the current node

         while It <= Last (Prio) loop
            if Prio.Table (It).Node = N then
               return Find_First_Object_With_Priority
                   (Prio,
                    Prio.Table (It).Cnt);
            end if;
            It := It + 1;
         end loop;

         return No_Node;
      end Find_First_Object_With_Same_Priority;

      -------------------------
      -- Register_Priorities --
      -------------------------

      procedure Register_Priorities
        (Thread_1, Thread_2 :        Node_Id;
         Prio               : in out Node_Priorities.Instance;
         Minimum            :    out Int;
         Maximum            :    out Int)
      is
         procedure Register_Thread_Priorities
           (Thread  :        Node_Id;
            Prio    : in out Node_Priorities.Instance;
            Minimum : in out Int;
            Maximum : in out Int);

         --------------------------------
         -- Register_Thread_Priorities --
         --------------------------------

         procedure Register_Thread_Priorities
           (Thread  :        Node_Id;
            Prio    : in out Node_Priorities.Instance;
            Minimum : in out Int;
            Maximum : in out Int)
         is
            use Ocarina.Transfo.Fusions.Scheduler;

            pragma Assert (Kind (Thread) = K_Component_Implementation);

            Thread_Decl : constant Node_Id :=
              Corresponding_Entity (Component_Type_Identifier (Thread));
            Priority : Int;
            Call_Seq : Node_Id;
            P        : Node_Priority;
            Register : Boolean;
         begin
            --  A call sequence priority is either :
            --  1/ if the CS calls is a single call to a method of a
            --  protected object, the object priority
            --  2/ the previous call sequence priority
            --  3/ else the thread priority

            Call_Seq := ATN.First_Node (ATN.Calls (Thread));
            while Present (Call_Seq) loop
               Register := True;

               if Present (In_Modes (Call_Seq))
                 and then not Is_Empty (Modes (In_Modes (Call_Seq)))
                 and then Length (Modes (In_Modes (Call_Seq))) = 1
                 and then
                   Name
                     (Identifier (First_Node (Modes (In_Modes (Call_Seq))))) =
                   Stop_Mode_Name
               then
                  Register := False;
               end if;

               if Register then
                  Priority := Int (Get_Call_Sequence_Priority (Call_Seq));

                  if Priority = 0 then
                     Priority :=
                       Int (Get_Integer_Property (Thread, Cheddar_Priority));
                     if Priority = 0 then
                        Priority :=
                          Int
                            (Get_Integer_Property
                               (Thread_Decl,
                                Cheddar_Priority));
                     end if;
                  end if;

                  if Priority = 0 then
                     DE (Image (Loc (Thread)) &
                        " : priority must be " &
                        "specified for all threads");
                     raise Program_Error;
                  end if;

                  if Priority > Minimum then
                     Minimum := Priority;
                  end if;

                  if Priority < Maximum then
                     Maximum := Priority;
                  end if;

                  P.Cnt    := Priority;
                  P.Node   := Call_Seq;
                  P.Object := No_Node;
                  Node_Priorities.Append (Prio, P);
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end Register_Thread_Priorities;

         Min : Int := 0;
         Max : Int := 255;
      --  FIXME
      --  Find how to know AADL priority range
      begin
         Register_Thread_Priorities (Thread_1, Prio, Min, Max);
         Register_Thread_Priorities (Thread_2, Prio, Min, Max);

         Minimum := Min;
         Maximum := Max;
      end Register_Priorities;

      -----------------------------
      -- Build_Protected_Objects --
      -----------------------------

      procedure Build_Protected_Objects
        (Thread  :        Node_Id;
         Prio    : in out Node_Priorities.Instance;
         Minimum :        Int)
      is
         use Node_Priorities;
         use Utils;

         function Declare_Object_With_Priority
           (New_Thread : Node_Id;
            P          : Int) return Node_Id;

         function Declare_Dummy_Internal_Type
           (New_Thread : Node_Id) return Node_Id;

         ---------------------------------
         -- Declare_Dummy_Internal_Type --
         ---------------------------------

         function Declare_Dummy_Internal_Type
           (New_Thread : Node_Id) return Node_Id
         is
            use Ocarina.Builder.AADL.Components;
            use Ocarina.Builder.AADL.Properties;
            use Ocarina.Analyzer.AADL.Finder;

            Dummy_Type : Node_Id;
            Impl       : Node_Id;
            Data_Name  : constant Name_Id :=
              Get_String_Name ("dummy_internal_type");
            Decl_Id : constant Node_Id :=
              Make_Identifier (No_Location, Data_Name, Data_Name, No_Node);
            Impl_Name : constant Name_Id :=
              Get_String_Name ("dummy_internal_type.i");
            Impl_Id : constant Node_Id :=
              Make_Identifier (No_Location, Impl_Name, Impl_Name, No_Node);
            Struct    : constant Name_Id := Get_String_Name ("integer");
            S, Id, S2 : Node_Id;
         begin

            Dummy_Type :=
              Find_Component_Classifier
                (AADL_Root,
                 Identifier (Namespace (New_Thread)),
                 Decl_Id);
            if No (Dummy_Type) then
               Dummy_Type :=
                 Add_New_Component_Type
                   (No_Location,
                    Decl_Id,
                    Namespace (New_Thread),
                    CC_Data,
                    Is_Private => False);

               --  Add property data representation (integer)

               S := New_Node (K_Property_Term, No_Location);
               Set_Identifier
                 (S,
                  Make_Identifier (No_Location, Struct, Struct, No_Node));
               Id :=
                 Make_Identifier
                   (No_Location,
                    Data_Representation,
                    Data_Representation,
                    No_Node);
               S2 := New_Node (K_Entity_Reference, No_Location);
               Set_Identifier
                 (S2,
                  Make_Identifier
                    (No_Location,
                     Raw_Data_Representation,
                     Raw_Data_Representation,
                     No_Node));
               Set_Namespace_Identifier
                 (S2,
                  Make_Identifier
                    (No_Location,
                     Data_Model_Pkg,
                     Data_Model_Pkg,
                     No_Node));
               Set_Full_Identifier (S2, Id);

               Id :=
                 Add_New_Property_Association
                   (Loc            => No_Location,
                    Name           => Id,
                    Property_Name  => S2,
                    Container      => Dummy_Type,
                    In_Binding     => No_Node,
                    In_Modes       => No_Node,
                    Property_Value => S,
                    Is_Constant    => False,
                    Is_Access      => False,
                    Is_Additive    => False,
                    Applies_To     => No_List);
               if No (Id) then
                  raise Program_Error;
               end if;
            end if;

            Impl :=
              Find_Component_Classifier
                (AADL_Root,
                 Identifier (Namespace (New_Thread)),
                 Impl_Id);
            if No (Impl) then
               Impl :=
                 Add_New_Component_Implementation
                   (No_Location,
                    Impl_Id,
                    Namespace (New_Thread),
                    CC_Data,
                    Is_Private => False);
               Set_Component_Type_Identifier (Impl, Identifier (Dummy_Type));
            end if;

            return Impl;
         end Declare_Dummy_Internal_Type;

         ----------------------------------
         -- Declare_Object_With_Priority --
         ----------------------------------

         function Declare_Object_With_Priority
           (New_Thread : Node_Id;
            P          : Int) return Node_Id
         is
            use Ocarina.Builder.AADL.Components;
            use Ocarina.Builder.AADL.Components.Features;
            use Ocarina.Builder.AADL.Components.Subcomponents;
            use Ocarina.Builder.AADL.Properties;
            use Ocarina.Analyzer.AADL.Finder;
            use Ocarina.Analyzer.AADL.Naming_Rules;
            use Ocarina.AADL_Values;

            Concurrency : constant Name_Id :=
              Get_String_Name ("concurrency_control_protocol");
            PCP : constant Name_Id := Get_String_Name ("priority_ceiling");
            Data_Representation : constant Name_Id :=
              Get_String_Name ("data_model::data_representation");
            Struct : constant Name_Id := Get_String_Name ("struct");

            Decl_Name : constant Name_Id :=
              Get_String_Name ("priority_switcher_" & Image (P));
            Decl_Id : constant Node_Id :=
              Make_Identifier (No_Location, Decl_Name, Decl_Name, No_Node);
            Impl_Name : constant Name_Id :=
              Get_String_Name (Get_Name_String (Decl_Name) & ".i");
            Impl_Id : constant Node_Id :=
              Make_Identifier (No_Location, Impl_Name, Impl_Name, No_Node);
            Obj_Name : constant Name_Id :=
              Get_String_Name (Get_Name_String (Decl_Name) & "_obj");
            L : constant Node_Id := New_Node (K_Literal, No_Location);
            Field_Name    : constant Name_Id := Get_String_Name ("field");
            Comp_Decl     : Node_Id;
            Comp_Impl     : Node_Id;
            Old_Comp_Decl : Node_Id;
            Old_Comp_Impl : Node_Id;

            Object : Node_Id;
            S, S2  : Node_Id;
            Id     : Node_Id;
            Pkg    : constant Node_Id := Namespace (New_Thread);
            E      : Node_Id := New_Node (K_Entity_Reference, No_Location);
         begin
            --  Remove previous declaration and implementation
            --  of the priority_shifter

            Old_Comp_Decl :=
              Find_Component_Classifier (AADL_Root, Identifier (Pkg), Decl_Id);
            if Present (Old_Comp_Decl) then
               Remove_From_Scope
                 (Identifier (Old_Comp_Decl),
                  Entity_Scope (Pkg));
               Remove_Node_From_List (Old_Comp_Decl, Declarations (Pkg));
            end if;
            Old_Comp_Decl :=
              Find_Component_Classifier (AADL_Root, Identifier (Pkg), Impl_Id);
            if Present (Old_Comp_Decl) then
               Remove_From_Scope
                 (Identifier (Old_Comp_Decl),
                  Entity_Scope (Pkg));
               Remove_Node_From_List (Old_Comp_Decl, Declarations (Pkg));
            end if;

            --  Create new declaration and implementation

            Comp_Decl :=
              Add_New_Component_Type
                (No_Location,
                 Decl_Id,
                 Namespace (New_Thread),
                 CC_Data,
                 Is_Private => False);

            --  Add properties concurrency protocol and priority
            --  to declaration

            S := New_Node (K_Property_Term, No_Location);
            Set_Identifier
              (S,
               Make_Identifier (No_Location, PCP, PCP, No_Node));
            Id :=
              Make_Identifier (No_Location, Concurrency, Concurrency, No_Node);
            S2 := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier (S2, Id);
            Set_Full_Identifier (S2, Id);
            Id :=
              Add_New_Property_Association
                (Loc            => No_Location,
                 Name           => Id,
                 Property_Name  => S2,
                 Container      => Comp_Decl,
                 In_Binding     => No_Node,
                 In_Modes       => No_Node,
                 Property_Value => S,
                 Is_Constant    => False,
                 Is_Access      => False,
                 Is_Additive    => False,
                 Applies_To     => No_List);
            if No (Id) then
               raise Program_Error;
            end if;

            Set_Value (L, New_Integer_Value (Unsigned_Long_Long (P)));
            Id :=
              Make_Identifier
                (No_Location,
                 Raw_Priority,
                 Raw_Priority,
                 No_Node);
            S2 := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier (S2, Id);
            Set_Full_Identifier (S2, Id);
            Id :=
              Add_New_Property_Association
                (Loc            => No_Location,
                 Name           => Id,
                 Property_Name  => S2,
                 Container      => Comp_Decl,
                 In_Binding     => No_Node,
                 In_Modes       => No_Node,
                 Property_Value => L,
                 Is_Constant    => False,
                 Is_Access      => False,
                 Is_Additive    => False,
                 Applies_To     => No_List);
            if No (Id) then
               raise Program_Error;
            end if;

            Id :=
              Make_Identifier
                (No_Location,
                 Priority_Shifter,
                 Priority_Shifter,
                 No_Node);
            S2 := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Raw_Priority_Shifter,
                  Raw_Priority_Shifter,
                  No_Node));
            Set_Namespace_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Transfo_Pkg,
                  Transfo_Pkg,
                  No_Node));
            Set_Full_Identifier (S2, Id);

            S := New_Node (K_Literal, No_Location);
            Set_Value (S, New_Boolean_Value (True));
            Id :=
              Add_New_Property_Association
                (Loc            => No_Location,
                 Name           => Id,
                 Property_Name  => S2,
                 Container      => Comp_Decl,
                 In_Binding     => No_Node,
                 In_Modes       => No_Node,
                 Property_Value => S,
                 Is_Constant    => False,
                 Is_Access      => False,
                 Is_Additive    => False,
                 Applies_To     => No_List);
            if No (Id) then
               raise Program_Error;
            end if;

            --  Create the object implementation

            Old_Comp_Impl :=
              Find_Component_Classifier (AADL_Root, Identifier (Pkg), Impl_Id);
            if Present (Old_Comp_Impl) then
               Remove_Node_From_List (Old_Comp_Impl, Declarations (Pkg));
            end if;

            Comp_Impl :=
              Add_New_Component_Implementation
                (No_Location,
                 Impl_Id,
                 Namespace (New_Thread),
                 CC_Data,
                 Is_Private => False);
            Set_Component_Type_Identifier (Comp_Impl, Identifier (Comp_Decl));

            --  Add properties to implementation

            S := New_Node (K_Property_Term, No_Location);
            Set_Identifier
              (S,
               Make_Identifier (No_Location, Struct, Struct, No_Node));
            Id :=
              Make_Identifier
                (No_Location,
                 Data_Representation,
                 Data_Representation,
                 No_Node);
            S2 := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Raw_Data_Representation,
                  Raw_Data_Representation,
                  No_Node));
            Set_Namespace_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Data_Model_Pkg,
                  Data_Model_Pkg,
                  No_Node));
            Set_Full_Identifier (S2, Id);

            Id :=
              Add_New_Property_Association
                (Loc            => No_Location,
                 Name           => Id,
                 Property_Name  => S2,
                 Container      => Comp_Impl,
                 In_Binding     => No_Node,
                 In_Modes       => No_Node,
                 Property_Value => S,
                 Is_Constant    => False,
                 Is_Access      => False,
                 Is_Additive    => False,
                 Applies_To     => No_List);
            if No (Id) then
               raise Program_Error;
            end if;

            --  Add a dummy data object as subcomponent

            Object :=
              Add_New_Subcomponent
                (No_Location,
                 Make_Identifier
                   (No_Location,
                    Field_Name,
                    Field_Name,
                    No_Node),
                 Comp_Impl,
                 CC_Data);

            E := New_Node (K_Entity_Reference, No_Location);
            if No (Dummy_Internal_Type) then
               Dummy_Internal_Type := Declare_Dummy_Internal_Type (New_Thread);
            end if;
            Set_Identifier
              (E,
               Make_Identifier
                 (No_Location,
                  Name (Identifier (Dummy_Internal_Type)),
                  Name (Identifier (Dummy_Internal_Type)),
                  No_Node));
            Set_Entity (E, Dummy_Internal_Type);
            Set_Entity_Ref (Object, E);

            --  Add a single object in the fusionned thread

            Object :=
              Add_New_Subcomponent
                (No_Location,
                 Make_Identifier (No_Location, Obj_Name, Obj_Name, No_Node),
                 New_Thread,
                 CC_Data);
            E := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier
              (E,
               Make_Identifier (No_Location, Impl_Name, Impl_Name, E));
            Set_Entity (E, Comp_Impl);
            Set_Entity_Ref (Object, E);
            Set_Namespace_Identifier (E, Thread_Namespace);

            return Object;
         end Declare_Object_With_Priority;

         It  : Natural := First;
         Obj : Node_Id;
         P   : Int;
      begin
         while It <= Last (Prio) loop
            P := Prio.Table (It).Cnt;
            if P /= Minimum then
               Obj := Find_First_Object_With_Priority (Prio, P);
               if No (Obj) then
                  Obj := Declare_Object_With_Priority (Thread, P);
               end if;

               Prio.Table (It).Object := Obj;
            else
               Prio.Table (It).Object := No_Node;
            end if;

            It := It + 1;
         end loop;
      end Build_Protected_Objects;

      pragma Unreferenced (Build_Protected_Objects);

      -----------------------------
      -- Copy_All_Call_Sequences --
      -----------------------------

      procedure Copy_All_Call_Sequences
        (Thread_Impl              :        Node_Id;
         New_Thread_Impl          :        Node_Id;
         Periodic_Counter         : in out Int;
         Thread_Mapping           :        Node_Mapping.Instance;
         Priorities               : in out Node_Priorities.Instance;
         Build_Modes              :        Boolean;
         Is_Sporadic, Is_Periodic :    out Boolean)
      is
         use Ocarina.Builder.AADL.Components;

         procedure Add_Call_To_Scheduler (Call_Seq : Node_Id);
         --  appends a call to the mode scheduler

         procedure Add_Subprogram_To_Data
           (Object, Wrapper_Call, Thread : Node_Id);
         --  Add a procedure containing the call sequence code
         --  in the protected object and make the call sequence
         --  be a simple call to this procedure

         procedure Add_Call_To_Scheduler (Call_Seq : Node_Id) is
            Wrapper_Call_Seq : Node_Id;
         begin
            Wrapper_Call_Seq :=
              First_Node
                (Calls
                   (Entity
                      (Entity_Ref
                         (First_Node (ATN.Subprogram_Calls (Call_Seq))))));
            Append_Subprogram_Call
              (Wrapper_Call_Seq,
               Mode_Scheduler,
               Get_Name_String (Scheduler_Call));
         end Add_Call_To_Scheduler;

         ----------------------------
         -- Add_Subprogram_To_Data --
         ----------------------------

         procedure Add_Subprogram_To_Data
           (Object, Wrapper_Call, Thread : Node_Id)
         is
            use Ocarina.Builder.AADL.Components.Features;
            use Ocarina.Builder.AADL.Components.Subcomponents;
            use Ocarina.Builder.AADL.Components.Connections;

            pragma Assert (Kind (Wrapper_Call) = K_Subprogram_Call);

            Obj_Impl : constant Node_Id := Entity (Entity_Ref (Object));
            Obj_Decl : constant Node_Id :=
              Corresponding_Entity (Component_Type_Identifier (Obj_Impl));
            Obj_Name : constant Name_Id := Name (Identifier (Obj_Impl));
            Spg      : constant Node_Id := Entity (Entity_Ref (Wrapper_Call));
            Spg_N    : constant Name_Id := Name (Identifier (Spg));
            This     : constant Name_Id := Get_String_Name ("this");
            Mtd      : constant Name_Id :=
              Get_String_Name ("method_" & Image (Method_Num));
            E     : Node_Id;
            Param : Node_Id;
            Cnx   : Node_Id;
            Src   : Node_Id := New_Node (K_Entity_Reference, No_Location);
            Dst   : Node_Id := New_Node (K_Entity_Reference, No_Location);
         begin
            --  Add 'this' feature into the wrapper subprogram
            --  require access to the protected data

            Param :=
              Add_New_Subcomponent_Access
                (No_Location,
                 Make_Identifier (No_Location, This, This, No_Node),
                 Corresponding_Entity (Component_Type_Identifier (Spg)),
                 Is_Refinement => False,
                 Category      => CC_Data,
                 Is_Provided   => False);
            E := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier
              (E,
               Make_Identifier (No_Location, Obj_Name, Obj_Name, No_Node));
            Set_Entity_Ref (Param, E);

            --  Add a feature in the protected object

            Param :=
              Add_New_Subcomponent_Access
                (No_Location,
                 Make_Identifier (No_Location, Mtd, Mtd, No_Node),
                 Container     => Obj_Decl,
                 Is_Refinement => False,
                 Category      => CC_Subprogram,
                 Is_Provided   => True);
            E := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier
              (E,
               Make_Identifier (No_Location, Spg_N, Spg_N, No_Node));
            Set_Entity_Ref (Param, E);
            Set_Entity (E, Spg);
            Method_Num := Method_Num + 1;

            --  FIXME :
            --  TODO When AADLV2 will work fully in Ocarina,
            --  Add a subcomponent in the protected object

            --  FIXME :
            --  TODO When AADLV2 will work fully in Ocarina,
            --  Add a connection from the subcomponent to the
            --  feature in the protected object

            --  In the owner call sequence, change call to the
            --  previous wrapper to call to <protected_data>.<new_method>

            declare
               New_Call : constant Name_Id :=
                 Get_String_Name
                   (Get_Name_String (Name (Identifier (Obj_Decl))) &
                    "." &
                    Get_Name_String (Mtd));
               Obj_Name  : constant Name_Id := Name (Identifier (Object));
               Call_Name : constant Name_Id :=
                 Get_String_Name
                   (Get_Name_String (Name (Identifier (Wrapper_Call))) &
                    "." &
                    Get_Name_String (This));
               Cnx_Number : constant Name_Id :=
                 Get_String_Name ("cnx_" & Image (Cnx_Num));
               New_Path : List_Id;
               Id       : Node_Id;
            begin
               E := Entity_Ref (Wrapper_Call);
               Set_Identifier
                 (E,
                  Make_Identifier (No_Location, New_Call, New_Call, No_Node));
               New_Path := New_List (K_List_Id, No_Location);
               Create_Connection_End_Path
                 (New_Path,
                  Name (Identifier (Obj_Decl)),
                  Mtd);
               Set_Path (E, New_Path);
               Set_Entity (E, Param);

               --  In owner thread, add a connection from the localy
               --  (pre)declared object to the new subprogram call

               Src      := New_Node (K_Entity_Reference, No_Location);
               New_Path := New_List (K_List_Id, No_Location);
               Id       :=
                 Make_Identifier (No_Location, Obj_Name, Obj_Name, No_Node);
               Set_Identifier (Src, Id);
               Create_Connection_End_Path (New_Path, Obj_Name, No_Name);
               Set_Path (Src, New_Path);

               Dst      := New_Node (K_Entity_Reference, No_Location);
               New_Path := New_List (K_List_Id, No_Location);
               Id       :=
                 Make_Identifier (No_Location, Call_Name, Call_Name, No_Node);
               Set_Identifier (Dst, Id);
               Create_Connection_End_Path
                 (New_Path,
                  Name (Identifier (Wrapper_Call)),
                  This);
               Set_Path (Dst, New_Path);

               Cnx :=
                 Add_New_Connection
                   (No_Location,
                    Make_Identifier
                      (No_Location,
                       Cnx_Number,
                       Cnx_Number,
                       No_Node),
                    Thread,
                    Category    => CT_Access_Data,
                    Source      => Src,
                    Destination => Dst);
               Cnx_Num := Cnx_Num + 1;
               if No (Cnx) then
                  raise Program_Error;
               end if;
            end;
         end Add_Subprogram_To_Data;

         use Ocarina.Transfo.Fusions.Scheduler;

         New_Call_Seq : Node_Id;
         Call_Seq     : Node_Id;
         Mode_List    : Node_Id;
         Obj          : Node_Id;
         T_Call_Seq   : Node_Id;
         Priority_Val : Natural;
         Register     : Boolean;
         Wrapper_Wcet : Natural;
      begin
         Is_Sporadic := False;
         Is_Periodic := False;
         Call_Seq    := ATN.First_Node (ATN.Calls (Thread_Impl));
         while Present (Call_Seq) loop
            Register := True;

            --  * If the call sequence is bind to stop_mode, we ignore it
            --  * If it contains a call to a subprogram access of a
            --    priority_switcher, we extract the inner call sequence from
            --    this call sequence
            --  * Else we simply copy the call sequence

            if Present (In_Modes (Call_Seq))
              and then not Is_Empty (Modes (In_Modes (Call_Seq)))
              and then Length (Modes (In_Modes (Call_Seq))) = 1
              and then
                Name (Identifier (First_Node (Modes (In_Modes (Call_Seq))))) =
                Stop_Mode_Name

            then
               Register := False;
            end if;

            T_Call_Seq := Call_Seq;
            if Register
              and then
                Kind
                  (Entity
                     (Entity_Ref
                        (First_Node (ATN.Subprogram_Calls (Call_Seq))))) =
                K_Subcomponent_Access
            then
               declare
                  Obj : constant Node_Id :=
                    Container_Component
                      (Entity
                         (Entity_Ref
                            (First_Node (ATN.Subprogram_Calls (Call_Seq)))));
               begin
                  --  XXX FIXME

                  --  Because of the non-conformant implementation of
                  --  calls of subprogram access from data, we must create
                  --  a "false" call sequence, non-existing in the hierarchy
                  --  yet containing the actual call.

                  if Kind (Obj) = K_Component_Type
                    and then Is_Defined_Property (Obj, Priority_Shifter)
                  then
                     declare
                        N   : constant Node_Id := Create_Call_Sequence;
                        SPC : constant Node_Id :=
                          New_Node (K_Subprogram_Call, No_Location);
                        Ref : constant Node_Id :=
                          New_Node (K_Entity_Reference, No_Location);
                        Call : constant Name_Id := Get_String_Name ("main");
                        Base_Spg : constant Node_Id :=
                          Entity
                            (Entity_Ref
                               (First_Node (ATN.Subprogram_Calls (Call_Seq))));
                     begin

                        Set_Identifier
                          (SPC,
                           Make_Identifier (No_Location, Call, Call, No_Node));
                        Set_Identifier
                          (Ref,
                           Make_Identifier
                             (No_Location,
                              Name (Identifier (Base_Spg)),
                              Name (Identifier (Base_Spg)),
                              No_Node));
                        Set_Entity_Ref (SPC, Ref);
                        Set_Entity (Ref, Entity (Entity_Ref (Base_Spg)));

                        ATN.Set_Subprogram_Calls
                          (N,
                           New_List (K_List_Id, No_Location));
                        Append_Node_To_List (SPC, ATN.Subprogram_Calls (N));
                        T_Call_Seq := N;
                     end;
                  end if;
               end;
            end if;

            if Register then

               --  Copy the call sequences and relatives connections

               Copy_Call_Sequence
                 (T_Call_Seq,
                  Thread_Impl,
                  New_Thread_Impl,
                  Thread_Mapping,
                  New_Call_Seq,
                  Wrapper_Wcet);
               if not Add_Subprogram_Call_Sequence
                   (New_Thread_Impl,
                    New_Call_Seq)
               then
                  raise Program_Error;
               end if;

               --  Compute the new thread WCET

               Total_WCET := Total_WCET + Wrapper_Wcet;

               --  Set priority

               Priority_Val :=
                 Natural (Find_Corresponding_Priority (Priorities, Call_Seq));
               Set_Priority
                 (Entity
                    (Entity_Ref
                       (ATN.First_Node (ATN.Subprogram_Calls (New_Call_Seq)))),
                  Priority_Val);

               --  Set modes for periodic call sequences

               Mode_List := No_Node;
               if not Is_Call_Sequence_Sporadic (T_Call_Seq, Thread_Impl) then
                  Is_Periodic := True;
                  if Build_Modes then
                     Periodic_Counter := Periodic_Counter + 1;
                     declare
                        New_Mode : constant Node_Id :=
                          New_Node (K_Mode, No_Location);
                        Mode_Name : constant Name_Id :=
                          Get_String_Name ("mode_" & Image (Periodic_Counter));
                        Mode_Ref : constant Node_Id :=
                          New_Node (K_Entity_Reference, No_Location);
                     begin
                        --  Create a new mode

                        Mode_List := New_Node (K_In_Modes, No_Location);
                        Set_Modes
                          (Mode_List,
                           New_List (K_List_Id, No_Location));
                        Set_Identifier
                          (Mode_Ref,
                           Make_Identifier
                             (No_Location,
                              Mode_Name,
                              Mode_Name,
                              No_Node));
                        Set_Entity (Mode_Ref, New_Mode);
                        Append_Node_To_List (Mode_Ref, Modes (Mode_List));
                        Set_In_Modes (New_Call_Seq, Mode_List);

                        --  Append the new mode to the thread mode list

                        if Is_Empty (Modes (New_Thread_Impl)) then
                           Set_Modes
                             (New_Thread_Impl,
                              New_List (K_List_Id, Loc (Thread_Impl)));
                        end if;
                        Set_Container_Component (New_Mode, New_Thread_Impl);
                        Set_Identifier
                          (New_Mode,
                           Make_Identifier
                             (Loc (T_Call_Seq),
                              Mode_Name,
                              Mode_Name,
                              New_Mode));
                        Append_Node_To_List
                          (New_Mode,
                           Modes (New_Thread_Impl));
                        Add_Call_To_Scheduler (New_Call_Seq);
                     end;
                  end if;
                  Add_Entrypoint
                    (New_Call_Seq,
                     New_Thread_Impl,
                     Call_Seq,
                     Thread_Impl,
                     Mode_List,
                     Thread_Mapping);
               else
                  Is_Sporadic := True;
                  Add_Entrypoint
                    (New_Call_Seq,
                     New_Thread_Impl,
                     Call_Seq,
                     Thread_Impl,
                     No_Node,
                     Thread_Mapping);
               end if;

               --  In case of priority change :
               --  FIXMER : disabled now

               --  1/ search for priority_switcher object of the same priority
               --     in the thread subcomponents

               Obj :=
                 Find_First_Object_With_Same_Priority (Priorities, Call_Seq);

               --  2/ Add a procedure containing the call sequence code
               --     and make the call sequence be a simple call to this
               --     procedure

               if Present (Obj) then

                  Add_Subprogram_To_Data
                    (Obj,
                     First_Node (ATN.Subprogram_Calls (New_Call_Seq)),
                     New_Thread_Impl);
               end if;
            end if;

            Call_Seq := Next_Node (Call_Seq);
         end loop;
      end Copy_All_Call_Sequences;

      use Ocarina.Builder.AADL.Components;
      use AIN;

      Th1_Impl : constant Node_Id := AIN.Corresponding_Declaration (Th_Inst_1);
      Th2_Impl : constant Node_Id := AIN.Corresponding_Declaration (Th_Inst_2);
      Periodic_Counter                 : Int              := 0;
      Is_Src_Sporadic, Is_Src_Periodic : Boolean;
      Is_Dst_Sporadic, Is_Dst_Periodic : Boolean;
      Periodic, Sporadic               : Boolean          := False;
      Success, Build_Modes             : Boolean;
      Minimum_Priority                 : Int;
      Maximum_Priority                 : Int;
   begin
      Build_Modes := (Find_Periodic_Sequences (Th1_Impl, Th2_Impl) > 1);

      Register_Priorities
        (Th1_Impl,
         Th2_Impl,
         Priorities,
         Minimum_Priority,
         Maximum_Priority);
--        Build_Protected_Objects
--          (New_Thread_Impl, Priorities, Minimum_Priority);
      Ocarina.Transfo.Fusions.Scheduler.Min_Priority := Minimum_Priority;
      Ocarina.Transfo.Fusions.Scheduler.Max_Priority := Maximum_Priority;

      Copy_All_Call_Sequences
        (Th1_Impl,
         New_Thread_Impl,
         Periodic_Counter,
         Th1_Mapping,
         Priorities,
         Build_Modes,
         Is_Src_Sporadic,
         Is_Src_Periodic);

      Copy_All_Call_Sequences
        (Th2_Impl,
         New_Thread_Impl,
         Periodic_Counter,
         Th2_Mapping,
         Priorities,
         Build_Modes,
         Is_Dst_Sporadic,
         Is_Dst_Periodic);

      Set_WCET (New_Thread_Impl, Total_WCET);

      Sporadic := Is_Src_Sporadic or else Is_Dst_Sporadic;
      Periodic := Is_Src_Periodic or else Is_Dst_Periodic;

      --  Build the scheduler

      Ocarina.Transfo.Fusions.Scheduler.Initialize
        (New_Thread_Impl,
         Th1_Impl,
         Th2_Impl);

      if Build_Modes then
         declare
            use Ocarina.Transfo.Fusions.Scheduler;

            S            : Node_Id;
            I            : Node_Id;
            Package_Name : constant Name_Id :=
              AIN.Name (AIN.Identifier (AIN.Namespace (Th_Inst_1)));

         begin
            --  Add the "stop" mode and minimum priority

            S := Create_Stop_Mode (New_Thread_Impl);
            Set_Period (Entity (Entity_Ref (S)), Get_Quantum);

            Generate_Schedule;

            --  Find the initial mode

            I := Find_Initial_Mode;
            ATN.Set_Is_Initial (I, True);

            --  Print the scheduler package

            Print_Schedule (Package_Name, Instance_Name);
         end;

      end if;

      Manage_Component_Properties
        (Th1_Impl,
         Th2_Impl,
         New_Thread_Impl,
         Success,
         Sporadic,
         Periodic);

      if not Success then
         raise Program_Error with "could not handle thread properties";
      end if;
   end Compute_Call_Sequences;

   ----------------
   -- Set_Period --
   ----------------

   procedure Set_Period (S : Node_Id; Quantum : Natural) is
      use Ocarina.AADL_Values;
      use Ocarina.Builder.AADL.Properties;
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      pragma Assert
        (Kind (S) = K_Component_Implementation
         or else Kind (S) = K_Component_Type);

      MS     : constant Name_Id := Get_String_Name ("ms");
      L      : constant Node_Id := New_Node (K_Signed_AADLNumber, No_Location);
      L2     : constant Node_Id := New_Node (K_Literal, No_Location);
      Path   : constant List_Id := New_List (K_List_Id, No_Location);
      C      : Node_Id;
      Lab2   : Name_Id;
      S2, Id : Node_Id          := No_Node;
   begin
      S2 := New_Node (K_Entity_Reference, No_Location);

      if Get_Category_Of_Component (S) = CC_Subprogram then
         Lab2 := Get_String_Name ("call_sequence_period");
         Id   := Make_Identifier (No_Location, CS_Period, CS_Period, No_Node);
         C    := New_Node (K_Node_Container, No_Location);
         Set_Item
           (C,
            Make_Identifier (No_Location, Transfo_Pkg, Transfo_Pkg, No_Node));
         Append_Node_To_List (C, Path);
         Set_Namespace_Identifier
           (S2,
            Make_Identifier (No_Location, Transfo_Pkg, Transfo_Pkg, No_Node));
      else
         Lab2 := Get_String_Name ("period");
         Id   := Make_Identifier (No_Location, Lab2, Lab2, No_Node);
      end if;

      Set_Identifier (S2, Make_Identifier (No_Location, Lab2, Lab2, No_Node));
      C := New_Node (K_Node_Container, No_Location);
      Set_Item (C, Make_Identifier (No_Location, Lab2, Lab2, No_Node));
      Append_Node_To_List (C, Path);
      Set_Path (S2, Path);

      Set_Value (L2, New_Integer_Value (Unsigned_Long_Long (Quantum)));
      Set_Unit_Identifier (L, Make_Identifier (No_Location, MS, MS, No_Node));
      Set_Number_Value (L, L2);
      Set_Full_Identifier (S2, Id);
      Id :=
        Add_New_Property_Association
          (Loc            => No_Location,
           Name           => Id,
           Property_Name  => S2,
           Container      => S,
           In_Binding     => No_Node,
           In_Modes       => No_Node,
           Property_Value => L,
           Is_Constant    => False,
           Is_Access      => False,
           Is_Additive    => False,
           Applies_To     => No_List);
      if No (Id) then
         raise Program_Error;
      end if;
   end Set_Period;

   --------------
   -- Set_WCET --
   --------------

   procedure Set_WCET (E : Node_Id; Max_Duration : Natural) is
      use Ocarina.AADL_Values;
      use Ocarina.Builder.AADL.Properties;

      Decl              : Node_Id;
      Values            : Node_Id;
      V1, V2, S, Id, S2 : Node_Id;
      Unit_Name         : constant Name_Id := Get_String_Name ("ms");
   begin

      --  Check weither the property has already been set
      --  in this case, remove the previous property association

      S := Ocarina.Analyzer.AADL.Queries.Get_Property_Association (E, WCET);
      if S /= No_Node then
         Remove_Node_From_List (S, ATN.Properties (E));

      elsif ATN.Kind (E) = K_Component_Implementation then
         Decl := ATN.Corresponding_Entity (ATN.Component_Type_Identifier (E));
         S    :=
           Ocarina.Analyzer.AADL.Queries.Get_Property_Association (Decl, WCET);
         if S /= No_Node then
            Remove_Node_From_List (S, ATN.Properties (Decl));
         end if;
      end if;

      --  Set time range value

      V1 := New_Node (ATN.K_Literal, No_Location);
      V2 := New_Node (ATN.K_Literal, No_Location);
      ATN.Set_Value (V1, New_Integer_Value (0));
      ATN.Set_Value
        (V2,
         New_Integer_Value (Unsigned_Long_Long (Max_Duration)));
      Id := New_Node (ATN.K_Identifier, No_Location);
      ATN.Set_Name (Id, Unit_Name);
      ATN.Set_Display_Name (Id, ATN.Name (Id));
      Values := New_Node (ATN.K_Number_Range_Term, No_Location);
      S      := New_Node (ATN.K_Signed_AADLNumber, No_Location);
      ATN.Set_Number_Value (S, V1);
      ATN.Set_Unit_Identifier (S, Id);
      V1 := S;
      S  := New_Node (ATN.K_Signed_AADLNumber, No_Location);
      ATN.Set_Number_Value (S, V2);
      ATN.Set_Unit_Identifier (S, Id);
      V2 := S;
      ATN.Set_Lower_Bound (Values, V1);
      ATN.Set_Upper_Bound (Values, V2);
      ATN.Set_Delta_Term (Values, No_Node);

      Id := Make_Identifier (No_Location, WCET, WCET, No_Node);
      S2 := New_Node (K_Entity_Reference, No_Location);
      Set_Identifier (S2, Make_Identifier (No_Location, WCET, WCET, No_Node));
      Set_Full_Identifier (S2, Id);

      Id :=
        Add_New_Property_Association
          (Loc            => No_Location,
           Name           => Id,
           Property_Name  => S2,
           Container      => E,
           In_Binding     => No_Node,
           In_Modes       => No_Node,
           Property_Value => Values,
           Is_Constant    => False,
           Is_Access      => False,
           Is_Additive    => False,
           Applies_To     => No_List);
      if No (Id) then
         raise Program_Error;
      end if;
   end Set_WCET;

   --------------
   -- Get_WCET --
   --------------

   function Get_WCET (S : Node_Id) return Unsigned_Long_Long is
      use Ocarina.AADL_Values;

      R : Node_Id;
      N : Node_Id;
      V : Value_Id;
   begin
      R := Get_Property_Association (S, WCET);
      R := Property_Association_Value (R);
      N := Number_Value (Upper_Bound (Single_Value (R)));
      V := Value (N);
      return Get_Value_Type (V).IVal;
   end Get_WCET;

   ------------------
   -- Set_Deadline --
   ------------------

   procedure Set_Deadline (S : Node_Id; Value : Natural) is
      use Ocarina.AADL_Values;
      use Ocarina.Builder.AADL.Properties;
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      pragma Assert
        (Kind (S) = K_Component_Implementation
         or else Kind (S) = K_Component_Type);

      MS     : constant Name_Id := Get_String_Name ("ms");
      L      : constant Node_Id := New_Node (K_Signed_AADLNumber, No_Location);
      L2     : constant Node_Id := New_Node (K_Literal, No_Location);
      S2, Id : Node_Id;
   begin
      S2 := New_Node (K_Entity_Reference, No_Location);
      Id := Make_Identifier (No_Location, Deadline, Deadline, No_Node);
      Set_Identifier
        (S2,
         Make_Identifier (No_Location, Deadline, Deadline, No_Node));

      Set_Value (L2, New_Integer_Value (Unsigned_Long_Long (Value)));
      Set_Unit_Identifier (L, Make_Identifier (No_Location, MS, MS, No_Node));
      Set_Number_Value (L, L2);
      Set_Full_Identifier (S2, Id);

      Id :=
        Add_New_Property_Association
          (Loc            => No_Location,
           Name           => Id,
           Property_Name  => S2,
           Container      => S,
           In_Binding     => No_Node,
           In_Modes       => No_Node,
           Property_Value => L,
           Is_Constant    => False,
           Is_Access      => False,
           Is_Additive    => False,
           Applies_To     => No_List);
      if No (Id) then
         raise Program_Error;
      end if;
   end Set_Deadline;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (S : Node_Id; Prio : Natural) is
      use Ocarina.AADL_Values;
      use Ocarina.Builder.AADL.Properties;
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      pragma Assert
        (Kind (S) = K_Component_Implementation
         or else Kind (S) = K_Component_Type);

      L2     : constant Node_Id := New_Node (K_Literal, No_Location);
      S2, Id : Node_Id;
   begin
      S2 := New_Node (K_Entity_Reference, No_Location);
      case Get_Category_Of_Component (S) is

         when CC_Thread =>
            Id :=
              Make_Identifier
                (No_Location,
                 Cheddar_Priority,
                 Cheddar_Priority,
                 No_Node);
            Set_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Raw_Fixed_Priority,
                  Raw_Fixed_Priority,
                  No_Node));
            Set_Namespace_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Cheddar_Pkg,
                  Cheddar_Pkg,
                  No_Node));

         when CC_Subprogram =>
            Id :=
              Make_Identifier
                (No_Location,
                 Transfo_Priority,
                 Transfo_Priority,
                 No_Node);
            Set_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Raw_Priority,
                  Raw_Priority,
                  No_Node));
            Set_Namespace_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Transfo_Pkg,
                  Transfo_Pkg,
                  No_Node));

         when others =>
            raise Program_Error;
      end case;

      Set_Value (L2, New_Integer_Value (Unsigned_Long_Long (Prio)));
      Set_Full_Identifier (S2, Id);

      Id :=
        Add_New_Property_Association
          (Loc            => No_Location,
           Name           => Id,
           Property_Name  => S2,
           Container      => S,
           In_Binding     => No_Node,
           In_Modes       => No_Node,
           Property_Value => L2,
           Is_Constant    => False,
           Is_Access      => False,
           Is_Additive    => False,
           Applies_To     => No_List);
      if No (Id) then
         raise Program_Error;
      end if;
   end Set_Priority;

   -------------------------------
   -- Set_Flat_Entity_Reference --
   -------------------------------

   procedure Set_Flat_Entity_Reference (Node : Node_Id; Name : Name_Id) is
      Path   : constant List_Id := New_List (K_List_Id, No_Location);
      C, Ref : Node_Id;
   begin
      Ref := New_Node (K_Entity_Reference, No_Location);
      C   := New_Node (K_Node_Container, No_Location);
      Set_Item (C, Make_Identifier (No_Location, Name, Name, No_Node));
      Append_Node_To_List (C, Path);
      Set_Path (Ref, Path);
      Set_Identifier (Ref, Make_Identifier (No_Location, Name, Name, No_Node));
      Set_Entity_Ref (Node, Ref);
   end Set_Flat_Entity_Reference;

   --------------------------------
   -- Declare_Wrapper_Subprogram --
   --------------------------------

   procedure Declare_Wrapper_Subprogram (Owner_Thread : Node_Id) is
      Wrapper : Node_Id;
      pragma Unreferenced (Wrapper);
   begin
      Wrapper := Declare_Wrapper_Subprogram (Owner_Thread);
   end Declare_Wrapper_Subprogram;

   --------------------------------
   -- Declare_Wrapper_Subprogram --
   --------------------------------

   function Declare_Wrapper_Subprogram
     (Owner_Thread : Node_Id) return Node_Id
   is
      use Ocarina.Builder.AADL.Components;
      use Ocarina.Builder.AADL.Components.Subcomponents;

      Decl         : Node_Id;
      Impl         : Node_Id;
      Decl_Id_Name : constant Name_Id :=
        Build_Unique_Name
          (Declarations (Namespace (Owner_Thread)),
           Wrapper_Prefix);
      Impl_Id_Name : Name_Id;
   begin
      Impl_Id_Name := Get_String_Name (Get_Name_String (Decl_Id_Name) & ".i");

      --  Create the declaration

      Decl :=
        Add_New_Component_Type
          (No_Location,
           Make_Identifier (No_Location, Decl_Id_Name, Decl_Id_Name, No_Node),
           Namespace (Owner_Thread),
           Ocarina.ME_AADL.CC_Subprogram);
      if No (Decl) then
         DE ("Could not create a wrapper");
         raise Program_Error;
      end if;
      Set_Features (Decl, New_List (K_List_Id, No_Location));

      --  Create the implementation

      Impl :=
        Add_New_Component_Implementation
          (No_Location,
           Make_Identifier (No_Location, Impl_Id_Name, Impl_Id_Name, No_Node),
           Namespace (Owner_Thread),
           Ocarina.ME_AADL.CC_Subprogram);
      Set_Component_Type_Identifier (Impl, Identifier (Decl));
      if No (Impl) then
         DE ("Could not create a wrapper");
         raise Program_Error;
      end if;
      Set_Connections (Impl, New_List (K_List_Id, No_Location));

      return Impl;
   end Declare_Wrapper_Subprogram;

   ---------------------------------
   -- Manage_Component_Properties --
   ---------------------------------

   procedure Manage_Component_Properties
     (Old_Thread_1, Old_Thread_2, New_Thread :     Node_Id;
      Success                                : out Boolean;
      Is_Sporadic, Is_Periodic               :     Boolean := False)
   is
      use Ocarina.Builder.AADL.Properties;

      pragma Assert
        ((Kind (Old_Thread_1) = K_Component_Implementation
          or else Kind (Old_Thread_1) = K_Component_Type)
         and then
         (Kind (Old_Thread_2) = K_Component_Implementation
          or else Kind (Old_Thread_2) = K_Component_Type)
         and then
         (Kind (New_Thread) = K_Component_Implementation
          or else Kind (New_Thread) = K_Component_Type));

      procedure Copy_All_Others_Priorities (Old_Thread, New_Thread : Node_Id);

      --------------------------------
      -- Copy_All_Others_Priorities --
      --------------------------------

      procedure Copy_All_Others_Priorities
        (Old_Thread, New_Thread : Node_Id)
      is
         use Ocarina.Transfo.Fusions.Scheduler;

         pragma Assert
           (Kind (Old_Thread) = K_Component_Implementation
            or else Kind (Old_Thread) = K_Component_Type);
         pragma Assert
           (Kind (New_Thread) = K_Component_Implementation
            or else Kind (New_Thread) = K_Component_Type);

         P      : Node_Id;
         Prop   : Node_Id;
         P_Name : Name_Id;
      begin
         if Properties (Old_Thread) = No_List then
            return;
         end if;
         P := First_Node (Properties (Old_Thread));
         while Present (P) loop
            P_Name := Name (Full_Identifier (Property_Name (P)));

            if P_Name = Dispatch_Protocol then
               null;
            elsif P_Name = Period then
               null;
            elsif P_Name = Deadline then
               null;
            elsif P_Name = Cheddar_Priority then
               null;
            elsif P_Name = WCET then
               null;
            elsif P_Name = Compute_Entrypoint then
               null;
            elsif P_Name = Transfo_Occurred then
               null;
            elsif P_Name = Transfo_Scheduler_Name then
               null;
            elsif P_Name = Transfo_Original_Name then
               null;

            else
               declare
                  Property_Val : Node_Id;
               begin
                  if Present
                      (Single_Value (Property_Association_Value (P)))
                  then
                     Property_Val :=
                       Single_Value (Property_Association_Value (P));
                  else
                     --  FIXME
                     --  Very dirty !
                     --  Need to fix Add_New_Property_Association...

                     Property_Val :=
                       Node_Id (Multi_Value (Property_Association_Value (P)));
                  end if;

                  Prop :=
                    Add_New_Property_Association
                      (Loc  => No_Location,
                       Name =>
                         Make_Identifier
                           (No_Location,
                            Name (Identifier (P)),
                            Display_Name (Identifier (P)),
                            No_Node),
                       Property_Name  => Property_Name (P),
                       Container      => New_Thread,
                       In_Binding     => In_Binding (P),
                       In_Modes       => In_Modes (P),
                       Property_Value => Property_Val,
                       Is_Constant    => Is_Constant (P),
                       Is_Access      => Is_Access (P),
                       Is_Additive    => Is_Additive_Association (P),
                       Applies_To     => Applies_To_Prop (P),
                       Override       => True);
                  Set_Corresponding_Entity (Identifier (Prop), Prop);
               end;
            end if;

            P := Next_Node (P);
         end loop;
      end Copy_All_Others_Priorities;

      -----------------
      -- Compute_GCD --
      -----------------

      function Compute_GCD (V1, V2 : Int) return Int;
      pragma Warnings (Off, Compute_GCD);

      function Compute_GCD (V1, V2 : Int) return Int is
      begin
         if V2 = 0 then
            return V1;
         else
            return Compute_GCD (V2, V1 mod V2);
         end if;
      end Compute_GCD;
   begin
      Success := True;
      if Is_Periodic or else Is_Sporadic then

         --  1/ find the value of timed-related properties

         --  FIXME
         --  Should set WCET as the sum of the wrappers' WCETs

         --  1.1/ dispatch protocol

         declare
            S, S2, Id     : Node_Id;
            Property_Name : constant Name_Id :=
              Get_String_Name ("dispatch_protocol");
            Periodic : constant Name_Id := Get_String_Name ("periodic");
            Sporadic : constant Name_Id := Get_String_Name ("sporadic");
            Hybrid   : constant Name_Id := Get_String_Name ("hybrid");
            Chosen   : Name_Id;
         begin
            if Is_Sporadic then
               if Is_Periodic then
                  Chosen := Hybrid;
               else
                  Chosen := Sporadic;
               end if;
            else
               if Is_Periodic then
                  Chosen := Periodic;
               else
                  raise Program_Error;
               end if;
            end if;

            S := New_Node (K_Property_Term, No_Location);
            Set_Identifier
              (S,
               Make_Identifier (No_Location, Chosen, Chosen, No_Node));
            Id :=
              Make_Identifier
                (No_Location,
                 Property_Name,
                 Property_Name,
                 No_Node);
            S2 := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier (S2, Id);
            Set_Full_Identifier (S2, Id);
            Id :=
              Add_New_Property_Association
                (Loc            => No_Location,
                 Name           => Id,
                 Property_Name  => S2,
                 Container      => New_Thread,
                 In_Binding     => No_Node,
                 In_Modes       => No_Node,
                 Property_Value => S,
                 Is_Constant    => False,
                 Is_Access      => False,
                 Is_Additive    => False,
                 Applies_To     => No_List);
         end;

         --  1.2/ Period & Deadline
         --  We set the PGCD of all periods of periodic call sequences
         --  Period not supported yet for sporadic call sequences

         declare
            use Ocarina.Transfo.Fusions.Scheduler;
            use Ocarina.AADL_Values;

            Gcd : Unsigned_Long_Long;
         begin
            --  FIXME TODO :
            --  * Handle sporadic call sequences MIAT

            Gcd := Unsigned_Long_Long (Get_Quantum);

            if Gcd = 0 then
               if Is_Defined_Property (Old_Thread_1, Period) then
                  Gcd := Get_Integer_Property (Old_Thread_1, Period);

               elsif Is_Defined_Property (Old_Thread_2, Period) then
                  Gcd := Get_Integer_Property (Old_Thread_2, Period);

               else
                  --  Should be periodic !
                  Display_Located_Error
                    (Loc (Old_Thread_1),
                     "should be periodic",
                     Fatal => True);
               end if;
            end if;

            --  Period

            Set_Period (New_Thread, Standard.Integer (Gcd));

            --  Deadline

            Set_Deadline (New_Thread, Standard.Integer (Gcd));

            --  1.3/ Priority

            Set_Priority
              (New_Thread,
               Natural (Ocarina.Transfo.Fusions.Scheduler.Min_Priority));

         end;

         --  Flag the new thread as fusioned

         declare
            use Ocarina.AADL_Values;

            L2     : constant Node_Id := New_Node (K_Literal, No_Location);
            Id, S2 : Node_Id;
         begin
            Id :=
              Make_Identifier
                (No_Location,
                 Transfo_Occurred,
                 Transfo_Occurred,
                 No_Node);
            S2 := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Raw_Occurred,
                  Raw_Occurred,
                  No_Node));
            Set_Namespace_Identifier
              (S2,
               Make_Identifier
                 (No_Location,
                  Transfo_Pkg,
                  Transfo_Pkg,
                  No_Node));

            Set_Value (L2, New_Boolean_Value (True));
            Set_Full_Identifier (S2, Id);
            Id :=
              Add_New_Property_Association
                (Loc            => No_Location,
                 Name           => Id,
                 Property_Name  => S2,
                 Container      => New_Thread,
                 In_Binding     => No_Node,
                 In_Modes       => No_Node,
                 Property_Value => L2,
                 Is_Constant    => False,
                 Is_Access      => False,
                 Is_Additive    => False,
                 Applies_To     => No_List);
            if No (Id) then
               raise Program_Error;
            end if;
         end;
      end if;

      --  2/ copy others properties

      Copy_All_Others_Priorities (Old_Thread_1, New_Thread);
      Copy_All_Others_Priorities (Old_Thread_2, New_Thread);
   end Manage_Component_Properties;

   ----------------------------------
   -- Exchange_Processes_In_System --
   ----------------------------------

   procedure Exchange_Processes_In_System
     (System, Old_Proc_Subcomponent, New_Proc :     Node_Id;
      Success                                 : out Boolean)
   is
      use Ocarina.Builder.AADL.Components;
      use Ocarina.Analyzer.AADL.Naming_Rules;

      pragma Assert (ATN.Kind (Old_Proc_Subcomponent) = ATN.K_Subcomponent);

      pragma Assert (ATN.Kind (New_Proc) = ATN.K_Component_Implementation);

      Property_Scop : constant Node_Id :=
        New_Node (K_Scope_Definition, No_Location);
      S : constant Node_Id := New_Node (K_Subcomponent, No_Location);
      E : constant Node_Id := New_Node (K_Entity_Reference, No_Location);
      L : List_Id;
      R : Node_Id;
   begin
      Success := True;

      if Present (Parent (System))
        and then Kind (Parent (System)) = K_Entity_Reference
      then
         --  In this case, the system is actually extended
         --  relevant subcomponents are in the original system

         R := Entity (Parent (System));
      else
         R := System;
      end if;

      ATN.Set_Category (S, ATN.Category (Old_Proc_Subcomponent));
      ATN.Set_In_Modes (S, ATN.In_Modes (Old_Proc_Subcomponent));
      ATN.Set_Is_Refinement (S, ATN.Is_Refinement (Old_Proc_Subcomponent));
      ATN.Set_Is_Implicit_Inverse
        (S,
         ATN.Is_Implicit_Inverse (Old_Proc_Subcomponent));
      ATN.Set_Inversed_Entity (S, ATN.Inversed_Entity (Old_Proc_Subcomponent));

      --  FIXME :
      --  AADLv2 arrays and prototype binding declarations

      ATN.Set_Array_Dimensions (S, No_Node);
      ATN.Set_Prototype_Bindings (S, No_List);

      ATN.Set_Namespace_Path
        (E,
         Namespace_Path (ATN.Entity_Ref (Old_Proc_Subcomponent)));
      L := New_List (K_List_Id, No_Location);
      Append_Node_To_List
        (Make_Identifier
           (No_Location,
            ATN.Name (ATN.Identifier (New_Proc)),
            ATN.Display_Name (ATN.Identifier (New_Proc)),
            No_Node),
         L);
      ATN.Set_Path (E, L);
      ATN.Set_Identifier
        (E,
         Make_Identifier
           (No_Location,
            ATN.Name (ATN.Identifier (New_Proc)),
            ATN.Display_Name (ATN.Identifier (New_Proc)),
            E));
      ATN.Set_Entity_Ref (S, E);

      ATN.Set_Identifier
        (S,
         Make_Identifier
           (No_Location,
            ATN.Name (ATN.Identifier (Old_Proc_Subcomponent)),
            ATN.Display_Name (ATN.Identifier (Old_Proc_Subcomponent)),
            S));
      Set_Container_Component (S, R);

      ATN.Set_Property_Scope (S, Property_Scop);
      ATN.Set_Properties (S, ATN.Properties (Old_Proc_Subcomponent));

      Remove_From_Scope (Identifier (Old_Proc_Subcomponent), Entity_Scope (R));

      --  Delete the old process instance from the parent subcomponent
      --  list

      ATNU.Replace_Node_To_List
        (ATN.Subcomponents (R),
         Old_Proc_Subcomponent,
         S);
   end Exchange_Processes_In_System;

   -------------------------------
   -- Fusion_Parent_Connections --
   -------------------------------

   procedure Fusion_Parent_Connections
     (Old_Proc, New_Proc, Old_Thread_1, Old_Thread_2 :     Node_Id;
      Success                                        : out Boolean)
   is
      use Charset;

      pragma Assert
        (Kind (Old_Thread_1) = K_Component_Implementation
         and then Kind (Old_Thread_2) = K_Component_Implementation);

      N  : Node_Id;
      L  : constant List_Id := Connections (Old_Proc);
      NL : List_Id          := Connections (New_Proc);
      S  : Node_Id;
   begin
      --  We create a copy of the original connection list,
      --  Fixing connections when necessary

      Success := True;
      if L = No_List then
         return;
      end if;

      if Is_Empty (NL) then
         NL := New_List (K_List_Id, No_Location);
      end if;

      N := First_Node (L);
      while Present (N) loop
         declare
            Src        : constant Node_Id := Source (N);
            Dst        : constant Node_Id := Destination (N);
            Is_Src_Th1 : Boolean          := False;
            Is_Src_Th2 : Boolean          := False;
            Is_Dst_Th1 : Boolean          := False;
            Is_Dst_Th2 : Boolean          := False;
            S2         : Node_Id;
            F1         : Name_Id;
            Cnx        : Node_Id;
            New_Path   : List_Id          := No_List;
            New_Name   : constant Name_Id := Instance_Name;
            Id, T      : Node_Id;
         begin

            --  The instances name is known since it has
            --  been provided by the user

            S  := First_Node (Path (Src));
            S2 := First_Node (Path (Dst));

            --  FIXME
            --  We only copy connections from port to port yet.
            --  TODO : accesses

            if
              (Kind (Corresponding_Entity (Item (S))) = K_Port_Spec
               or else
               (Present (Next_Node (S))
                and then
                  Kind (Corresponding_Entity (Item (Next_Node (S)))) =
                  K_Port_Spec))
              and then
              (Kind (Corresponding_Entity (Item (S2))) = K_Port_Spec
               or else
               (Present (Next_Node (S2))
                and then
                  Kind (Corresponding_Entity (Item (Next_Node (S2)))) =
                  K_Port_Spec))
            then

               Is_Src_Th1 :=
                 (To_Lower (Get_Name_String (Name (Item (S)))) = Th1_Inst.all);
               Is_Src_Th2 :=
                 (To_Lower (Get_Name_String (Name (Item (S)))) = Th2_Inst.all);
               Is_Dst_Th1 :=
                 (To_Lower (Get_Name_String (Name (Item (S2)))) =
                  Th1_Inst.all);
               Is_Dst_Th2 :=
                 (To_Lower (Get_Name_String (Name (Item (S2)))) =
                  Th2_Inst.all);

               Cnx := New_Node (K_Connection, No_Location);
               Set_Category (Cnx, Category (N));
               Copy_Feature_Core (N, Cnx, New_Proc);

               --  Create connection source

               if Is_Src_Th1 then
                  T :=
                    Find_New_Feature_Node
                      (Name (Item (Next_Node (S))),
                       Old_Thread_1,
                       Th1_Mapping);
                  Increment_Related_Counter (T);
                  F1 := Name (Identifier (T));
                  if F1 = No_Name then
                     DE ("Could not find feature " &
                        (Get_Name_String (Name (Item (Next_Node (S))))));
                     Success := False;
                     return;
                  end if;

                  Create_Connection_End_Path (New_Path, New_Name, F1);
                  Set_Source (Cnx, New_Node (K_Entity_Reference, No_Location));
                  Set_Path (Source (Cnx), New_Path);

               elsif Is_Src_Th2 then
                  T :=
                    Find_New_Feature_Node
                      (Name (Item (Next_Node (S))),
                       Old_Thread_2,
                       Th2_Mapping);
                  Increment_Related_Counter (T);
                  F1 := Name (Identifier (T));
                  if F1 = No_Name then
                     DE ("Could not find feature " &
                        (Get_Name_String (Name (Item (Next_Node (S))))));
                     Success := False;
                     return;
                  end if;
                  Create_Connection_End_Path (New_Path, New_Name, F1);
                  Set_Source (Cnx, New_Node (K_Entity_Reference, No_Location));
                  Set_Path (Source (Cnx), New_Path);

               else
                  Set_Source (Cnx, Source (N));
               end if;

               F1 := Build_Name_From_Path (Path (Source (Cnx)));
               Id := Make_Identifier (No_Location, F1, F1, Source (Cnx));
               Set_Identifier (Source (Cnx), Id);

               --  Create connection destination

               if Is_Dst_Th1 then
                  T :=
                    Find_New_Feature_Node
                      (Name (Item (Next_Node (S2))),
                       Old_Thread_1,
                       Th1_Mapping);
                  Increment_Related_Counter (T);
                  if No (T) then
                     DE ("feature " &
                        Get_Name_String (Name (Item (Next_Node (S2)))) &
                        " not found");
                     raise Program_Error;
                  end if;
                  F1 := Name (Identifier (T));
                  if F1 = No_Name then
                     DE ("Could not find feature " &
                        (Get_Name_String (Name (Item (Next_Node (S2))))));
                     Success := False;
                     return;
                  end if;
                  Create_Connection_End_Path (New_Path, New_Name, F1);
                  Set_Destination
                    (Cnx,
                     New_Node (K_Entity_Reference, No_Location));
                  Set_Path (Destination (Cnx), New_Path);

               elsif Is_Dst_Th2 then
                  T :=
                    Find_New_Feature_Node
                      (Name (Item (Next_Node (S2))),
                       Old_Thread_2,
                       Th2_Mapping);
                  Increment_Related_Counter (T);
                  F1 := Name (Identifier (T));
                  if F1 = No_Name then
                     DE ("Could not find feature " &
                        (Get_Name_String (Name (Item (Next_Node (S2))))));
                     Success := False;
                     return;
                  end if;
                  Create_Connection_End_Path (New_Path, New_Name, F1);
                  Set_Destination
                    (Cnx,
                     New_Node (K_Entity_Reference, No_Location));
                  Set_Path (Destination (Cnx), New_Path);

               else
                  Set_Destination (Cnx, Destination (N));
               end if;
               F1 := Build_Name_From_Path (Path (Destination (Cnx)));
               Id := Make_Identifier (No_Location, F1, F1, Destination (Cnx));
               Set_Identifier (Destination (Cnx), Id);

               Append_Node_To_List (Cnx, NL);
            end if;
         end;

         N := Next_Node (N);
      end loop;

      Set_Connections (New_Proc, NL);
   end Fusion_Parent_Connections;

   --------------------------------
   -- Create_Connection_End_Path --
   --------------------------------

   procedure Create_Connection_End_Path
     (Path    : in out List_Id;
      Token_1 :        Name_Id;
      Token_2 :        Name_Id := No_Name)
   is
      C : Node_Id;
   begin
      Path := New_List (K_List_Id, No_Location);
      C    := New_Node (K_Node_Container, No_Location);
      Set_Item (C, Make_Identifier (No_Location, Token_1, Token_1, No_Node));
      Append_Node_To_List (C, Path);

      if Token_2 /= No_Name then
         C := New_Node (K_Node_Container, No_Location);
         Set_Item
           (C,
            Make_Identifier (No_Location, Token_2, Token_2, No_Node));
         Append_Node_To_List (C, Path);
      end if;
   end Create_Connection_End_Path;

   -----------------------
   -- Create_Connection --
   -----------------------

   procedure Create_Connection
     (Src_Path, Dst_Path    : List_Id;
      Old_Cnx, Wrapper_impl : Node_Id;
      CT                    : Ocarina.ME_AADL.Connection_Type;
      New_Name              : Name_Id := No_Name)
   is
      use Ocarina.Builder.AADL.Components.Connections;

      pragma Assert (No (Old_Cnx) or else Kind (Old_Cnx) = K_Connection);
      pragma Assert (Kind (Wrapper_impl) = K_Component_Implementation);

      Cnx       : Node_Id;
      Id, E, E2 : Node_Id;
      M         : Name_Id;
   begin
      --  Create the wrapper connection
      --  We keep the previous connection name (deep copy)

      E := New_Node (K_Entity_Reference, No_Location);
      Set_Path (E, Src_Path);
      M  := Build_Name_From_Path (Src_Path);
      Id := Make_Identifier (No_Location, M, M, E);
      Set_Identifier (E, Id);
      E2 := New_Node (K_Entity_Reference, No_Location);
      Set_Path (E2, Dst_Path);
      M  := Build_Name_From_Path (Dst_Path);
      Id := Make_Identifier (No_Location, M, M, E2);
      Set_Identifier (E2, Id);

      if New_Name = No_Name then
         if Present (Identifier (Old_Cnx)) then
            Id :=
              Make_Identifier
                (No_Location,
                 Name (Identifier (Old_Cnx)),
                 Name (Identifier (Old_Cnx)),
                 No_Node);
         else
            Id := No_Node;
         end if;
      else
         Id := Make_Identifier (No_Location, New_Name, New_Name, No_Node);
      end if;

      Cnx :=
        Add_New_Connection
          (No_Location,
           Id,
           Wrapper_impl,
           CT,
           Is_Refinement => False,
           Source        => E,
           Destination   => E2);
      if No (Cnx) then
         raise Program_Error;
      end if;
   end Create_Connection;

   -----------------------
   -- Clean_Declaration --
   -----------------------

   procedure Clean_Declaration (Old_Decl, Namespace : Node_Id) is
      pragma Assert
        (Kind (Old_Decl) = K_Component_Type
         or else Kind (Old_Decl) = K_Component_Implementation);
   begin
      case Kind (Old_Decl) is
         when K_Component_Type =>
            Clean_Type_Declaration (Old_Decl, Namespace);

         when K_Component_Implementation =>
            Clean_Implementation_Declaration (Old_Decl, Namespace);

         when others =>
            raise Program_Error;
      end case;
   end Clean_Declaration;

   --------------------------------------
   -- Clean_Implementation_Declaration --
   --------------------------------------

   procedure Clean_Implementation_Declaration
     (Old_Impl, Namespace : Node_Id)
   is
      pragma Assert (Kind (Old_Impl) = K_Component_Implementation);

      N         : Node_Id;
      Same_Loc  : Boolean := True;
      Prev_Inst : Node_Id := No_Node;
   begin
      --  In some cases, "false" instances are created
      --  (I suspect it is when the extended mechanism is
      --  used)
      --  Thus we check the instances locations (the same
      --  in case of false instances)

      if AINU.Is_Empty (ATN.Instances (Old_Impl)) then
         N := No_Node;
      else
         N := AIN.First_Node (ATN.Instances (Old_Impl));
      end if;
      while Present (N) and then Same_Loc loop
         Same_Loc  := No (Prev_Inst) or else AIN.Loc (N) = AIN.Loc (Prev_Inst);
         Prev_Inst := N;
         N         := AIN.Next_Node (N);
      end loop;

      if Same_Loc then
         Remove_Node_From_List (Old_Impl, Declarations (Namespace));
      end if;
   end Clean_Implementation_Declaration;

   ----------------------------
   -- Clean_Type_Declaration --
   ----------------------------

   procedure Clean_Type_Declaration (Old_Decl, Namespace : Node_Id) is
      pragma Assert (Kind (Old_Decl) = K_Component_Type);

      N         : Node_Id;
      Same_Loc  : Boolean := True;
      Prev_Inst : Node_Id := No_Node;
   begin
      --  In some cases, "false" instances are created
      --  (I suspect it is when the extended mechanism is
      --  used)
      --  Thus we check the instances locations (the same
      --  in case of false instances)

      --  First we check the direct instances of the fusionned
      --  component types

      if AINU.Is_Empty (ATN.Instances (Old_Decl)) then
         N := No_Node;
      else
         N := AIN.First_Node (ATN.Instances (Old_Decl));
      end if;
      while Present (N) and then Same_Loc loop
         Same_Loc  := No (Prev_Inst) or else AIN.Loc (N) = AIN.Loc (Prev_Inst);
         Prev_Inst := N;
         N         := AIN.Next_Node (N);
      end loop;

      --  Second we check the instances of the component's
      --  implementations

      if AINU.Is_Empty (ATN.Instances (Old_Decl)) then
         N := No_Node;
      else
         N := ATN.First_Node (ATN.Declarations (Namespace));
      end if;
      while Present (N) and then Same_Loc loop
         if ATN.Kind (N) = K_Component_Implementation
           and then
             ATN.Name (Component_Type_Identifier (N)) =
             ATN.Name (Identifier (Old_Decl))
         then
            declare
               N2 : Node_Id := AIN.First_Node (ATN.Instances (N));
            begin
               while Present (N2) and then Same_Loc loop
                  Same_Loc :=
                    No (Prev_Inst) or else AIN.Loc (N2) = AIN.Loc (Prev_Inst);
                  Prev_Inst := N2;
                  N2        := AIN.Next_Node (N2);
               end loop;
            end;
         end if;
         N := ATN.Next_Node (N);
      end loop;

      if Same_Loc then
         Remove_Node_From_List (Old_Decl, Declarations (Namespace));
      end if;
   end Clean_Type_Declaration;

   -----------------------
   -- Copy_All_Features --
   -----------------------

   procedure Copy_All_Features
     (Features_List    :        List_Id;
      Mapping          : in out Node_Mapping.Instance;
      Thread_Component :        Node_Id;
      Owner_Thread     :        Node_Id)
   is
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      pragma Assert (Kind (Thread_Component) = K_Component_Type);

      function Find_Feature_By_Name
        (Feature_Name : Name_Id;
         Feature_List : List_Id) return Boolean;

      --------------------------
      -- Find_Feature_By_Name --
      --------------------------

      function Find_Feature_By_Name
        (Feature_Name : Name_Id;
         Feature_List : List_Id) return Boolean
      is
         N : Node_Id := First_Node (Feature_List);
      begin
         while Present (N) loop
            if Name (Identifier (N)) = Feature_Name then
               return True;
            end if;
            N := Next_Node (N);
         end loop;
         return False;
      end Find_Feature_By_Name;

      L       : constant List_Id := Features (Thread_Component);
      N       : Node_Id;
      F       : Node_Id;
      C       : Node_Counter;
      C2      : Node_Association;
      Success : Boolean;
   begin
      if L = No_List then
         return;
      else
         N := First_Node (L);
      end if;

      while Present (N) loop
         case Kind (N) is
            when K_Port_Spec =>
               F := New_Node (K_Port_Spec, No_Location);
               Set_Is_In (F, Is_In (N));
               Set_Is_Out (F, Is_Out (N));
               Set_Is_Event (F, Is_Event (N));
               Set_Is_Data (F, Is_Data (N));
               Set_Is_Implicit_Inverse (F, Is_Implicit_Inverse (N));
               Set_Inversed_Entity (F, Inversed_Entity (N));

               if not Find_Feature_By_Name
                   (Name (Identifier (N)),
                    Features_List)
               then
                  Copy_Feature_Core (N, F, Owner_Thread);
               else
                  declare
                     Base_Name : constant Name_Id := Name (Identifier (N));
                     Cnt       : Int              := 2;
                     New_Name  : Name_Id          :=
                       Get_String_Name
                         (Get_Name_String (Base_Name) & "_" & Image (Cnt));
                  begin
                     while Find_Feature_By_Name (New_Name, Features_List) loop
                        Cnt      := Cnt + 1;
                        New_Name :=
                          Get_String_Name
                            (Get_Name_String (Base_Name) & "_" & Image (Cnt));
                     end loop;
                     Copy_Feature_Core (N, F, Owner_Thread, "_" & Image (Cnt));
                  end;
               end if;
               Success := True;

            when others =>
               --  FIXME : handle other cases instead
               --  of just ignoring them
               Success := False;
         end case;

         if Success then
            --  Create a new reference node

            C2.Old_Node := N;
            C2.New_Node := F;

            --  Create a new counter node

            C.Node := F;
            C.Cnt  := 0;

            Append_Node_To_List (F, Features_List);
            Node_Counters.Append (Counters, C);
            Node_Mapping.Append (Mapping, C2);
         end if;

         N := Next_Node (N);
      end loop;
   end Copy_All_Features;

   ----------------------------
   -- Copy_All_Subcomponents --
   ----------------------------

   procedure Copy_All_Subcomponents
     (Features_List             : List_Id;
      Thread_Impl, Owner_Thread : Node_Id)
   is
      pragma Assert (Kind (Thread_Impl) = K_Component_Implementation);

      use Ocarina.ME_AADL.AADL_Tree.Entities;

      L        : constant List_Id := Subcomponents (Thread_Impl);
      Switcher : constant Name_Id := Get_String_Name ("priority_switcher_");
      N        : Node_Id;
      S        : Node_Id;
      Copy     : Boolean;
   begin
      if L = No_List then
         return;
      else
         N := First_Node (L);
      end if;

      while Present (N) loop
         declare
            use Utils;

            N2       : constant Node_Id := Entity (Entity_Ref (N));
            Obj_Name : constant Name_Id := Name (Identifier (N2));
         begin
            case Get_Category_Of_Component (N2) is

               when CC_Data =>
                  Copy := not Is_Prefix (Switcher, Obj_Name);

               when others =>
                  null;
            end case;
         end;

         if Copy then
            S := New_Node (K_Subcomponent, No_Location);
            Set_Category (S, Category (N));
            Set_In_Modes (S, In_Modes (N));
            Set_Is_Refinement (S, Is_Refinement (N));
            Set_Is_Implicit_Inverse (S, Is_Implicit_Inverse (N));
            Set_Inversed_Entity (S, Inversed_Entity (N));

            --  FIXME :
            --  AADLv2 arrays and prototype binding declarations

            Set_Array_Dimensions (S, No_Node);
            Set_Prototype_Bindings (S, No_List);
            Copy_Feature_Core (N, S, Owner_Thread);

            Append_Node_To_List (S, Features_List);
         end if;

         N := Next_Node (N);
      end loop;

   end Copy_All_Subcomponents;

   -----------------------
   -- Copy_Feature_Core --
   -----------------------

   procedure Copy_Feature_Core
     (Feature, New_Feature, Owner_Component : Node_Id;
      Suffix                                : String := "")
   is
      Id            : Node_Id;
      Property_Scop : constant Node_Id :=
        New_Node (K_Scope_Definition, No_Location);
   begin
      Set_Property_Scope (New_Feature, Property_Scop);
      Set_Properties (New_Feature, ATN.Properties (Feature));
      Set_Entity_Ref (New_Feature, Entity_Ref (Feature));
      Set_Container_Component (New_Feature, Owner_Component);

      if Suffix = "" then
         if No (Identifier (Feature)) then
            Id := No_Node;
         else
            Id :=
              Make_Identifier
                (No_Location,
                 Name (Identifier (Feature)),
                 Display_Name (Identifier (Feature)),
                 New_Feature);
         end if;
      else
         declare
            Concat   : String_Access;
            Str_Name : Name_Id;
         begin
            if No (Identifier (Feature)) then
               Id := No_Node;
            else
               Concat :=
                 new String'
                   (Get_Name_String (Name (Identifier (Feature))) & Suffix);
               Str_Name := Get_String_Name (Concat.all);
               Id       :=
                 Make_Identifier
                   (No_Location,
                    Str_Name,
                    Str_Name,
                    New_Feature);
            end if;
         end;
      end if;

      Set_Identifier (New_Feature, Id);
   end Copy_Feature_Core;

   -------------------------------
   -- Search_Subprogram_By_Name --
   -------------------------------

   function Search_Subprogram_By_Name
     (Key          : Name_Id;
      Call_Mapping : Node_Mapping.Instance) return Node_Id
   is
      use Node_Mapping;
      use Utils;

      It : Natural := First;
   begin
      while It <= Last (Call_Mapping) loop

         if To_Lower (Name (Call_Mapping.Table (It).Old_Node)) =
           To_Lower (Key)
         then
            return Call_Mapping.Table (It).New_Node;
         end if;

         It := It + 1;
      end loop;

      return No_Node;
   end Search_Subprogram_By_Name;

   -------------------------------
   -- Increment_Related_Counter --
   -------------------------------

   procedure Increment_Related_Counter (Feature : Node_Id) is
      use Node_Counters;

      It : Natural := First;
   begin
      while It <= Last (Counters) loop
         if Counters.Table (It).Node = Feature then
            Counters.Table (It).Cnt := Counters.Table (It).Cnt + 1;
            return;
         end if;
         It := It + 1;
      end loop;
   end Increment_Related_Counter;

   ---------------------------
   -- Find_New_Feature_Node --
   ---------------------------

   function Find_New_Feature_Node
     (Old_Feature_Name : Name_Id;
      Parent           : Node_Id;
      Mapping          : Node_Mapping.Instance) return Node_Id
   is
      use Node_Mapping;
      use Utils;

      It          : Natural          := First;
      Parent_Type : constant Node_Id :=
        Corresponding_Entity (Component_Type_Identifier (Parent));
   begin
      while It <= Last (Mapping) loop
         if To_Lower (Name (Identifier (Mapping.Table (It).Old_Node))) =
           To_Lower (Old_Feature_Name)
         then
            declare
               NA : constant Node_Association := Mapping.Table (It);
            begin
               if Container_Component (NA.Old_Node) = Parent_Type then
                  return NA.New_Node;
               end if;
            end;
         end if;

         It := It + 1;
      end loop;

      return No_Node;
   end Find_New_Feature_Node;

   ----------------------------
   -- Duplicate_Subcomponent --
   ----------------------------

   function Duplicate_Subcomponent
     (Old_Subcomp, Owner : Node_Id;
      Suffix             : String := "") return Node_Id
   is
      pragma Assert (Kind (Old_Subcomp) = K_Subcomponent);

      New_Subcomp : Node_Id;
   begin
      New_Subcomp := New_Node (K_Subcomponent, No_Location);
      Set_Category (New_Subcomp, Category (Old_Subcomp));
      Copy_Feature_Core (Old_Subcomp, New_Subcomp, Owner, Suffix);
      Set_In_Modes (New_Subcomp, In_Modes (Old_Subcomp));
      Set_Array_Dimensions (New_Subcomp, Array_Dimensions (Old_Subcomp));
      Set_Prototype_Bindings (New_Subcomp, No_List);

      return New_Subcomp;
   end Duplicate_Subcomponent;

   -----------------------
   -- Duplicate_Process --
   -----------------------

   function Duplicate_Process (Old_Proc : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      pragma Assert (Get_Category_Of_Component (Old_Proc) = CC_Process);

      Proc_Impl : Node_Id;
      Id_Str    : constant String :=
        Get_Name_String (Name (Identifier (Old_Proc))) & "_fus";
      Entity_Scop : constant Node_Id :=
        New_Node (K_Scope_Definition, No_Location);
      Property_Scop : constant Node_Id :=
        New_Node (K_Scope_Definition, No_Location);
   begin
      Proc_Impl := New_Node (K_Component_Implementation, No_Location);

      Set_Identifier
        (Proc_Impl,
         Make_Identifier
           (No_Location,
            Get_String_Name (Id_Str),
            Get_String_Name (Id_Str),
            Proc_Impl));
      Set_Category (Proc_Impl, Category (Old_Proc));

      Set_Namespace (Proc_Impl, Namespace (Old_Proc));

      Set_Component_Type_Identifier
        (Proc_Impl,
         Make_Identifier
           (No_Location,
            Name (Component_Type_Identifier (Old_Proc)),
            Display_Name (Component_Type_Identifier (Old_Proc)),
            No_Node));
      Set_Parent (Proc_Impl, Parent (Old_Proc));
      Set_Refines_Type (Proc_Impl, Refines_Type (Old_Proc));

      Set_Entity_Scope (Proc_Impl, Entity_Scop);
      Set_Property_Scope (Proc_Impl, Property_Scop);
      Set_Corresponding_Entity (Entity_Scop, Proc_Impl);
      Set_Corresponding_Entity (Property_Scop, Proc_Impl);
      Set_Subcomponents (Proc_Impl, New_List (K_List_Id, No_Location));

      Set_Calls (Proc_Impl, No_List);
      --  A process cannot calls subprograms

      Set_Modes (Proc_Impl, Modes (Old_Proc));

      Set_Flows (Proc_Impl, Flows (Proc_Impl));
      --  FIXME

      Set_Properties (Proc_Impl, Properties (Proc_Impl));
      --  FIXME

      Set_Annexes (Proc_Impl, Annexes (Proc_Impl));
      --  FIXME

      Set_Prototypes (Proc_Impl, No_List);
      --  FIXME
      --  Unsupported yet

      Set_Prototype_Bindings (Proc_Impl, No_List);
      Set_Instances (Proc_Impl, No_List);
      --  Instantiation stuff

      return Proc_Impl;
   end Duplicate_Process;

   ---------------------------
   -- Clean_Unused_Feature --
   ---------------------------

   procedure Clean_Unused_Features is
      use Node_Counters;

      It : Natural := First;
      P  : Node_Id;
   begin
      while It <= Last (Counters) loop
         if Counters.Table (It).Cnt = 0 then
            P := Container_Component (Counters.Table (It).Node);
            pragma Assert (Kind (P) = K_Component_Type);
            Remove_Node_From_List (Counters.Table (It).Node, Features (P));
         end if;
         It := It + 1;
      end loop;
   end Clean_Unused_Features;

   -----------
   -- Clean --
   -----------

   procedure Clean is
   begin
      Free (Th1_Inst);
      Free (Th2_Inst);
      Node_Counters.Free (Counters);
      Node_Priorities.Free (Priorities);
      Node_Mapping.Free (Th1_Mapping);
      Node_Mapping.Free (Th2_Mapping);
   end Clean;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Raw_Original_Name       := Get_String_Name (Original_Name_Str);
      Transfo_Original_Name   := Get_String_Name (Transfo_Original_Name_Str);
      Raw_Scheduler_Name      := Get_String_Name (Scheduler_Name_Str);
      Transfo_Scheduler_Name  := Get_String_Name (Transfo_Scheduler_Name_Str);
      Dispatch_Protocol       := Get_String_Name (Dispatch_Protocol_Str);
      Deadline                := Get_String_Name (Deadline_Str);
      WCET                    := Get_String_Name (WCET_Str);
      Cheddar_Priority        := Get_String_Name (Cheddar_Priority_Str);
      Deployment_Priority     := Get_String_Name (Deployment_Priority_Str);
      Scheduler_Call          := Get_String_Name (Scheduler_Call_Str);
      Iterator_Call           := Get_String_Name (Iterator_Call_Str);
      Compute_Entrypoint      := Get_String_Name (Compute_Entrypoint_Str);
      CS_Period               := Get_String_Name (CS_Period_Str);
      Period                  := Get_String_Name (Period_Str);
      Transfo_Priority        := Get_String_Name (Transfo_Priority_Str);
      Transfo_Occurred        := Get_String_Name (Transfo_Occurred_Str);
      Priority_Shifter        := Get_String_Name (Priority_Shifter_Str);
      Raw_Priority_Shifter    := Get_String_Name (Raw_Priority_Shifter_Str);
      Stop_Mode_Name          := Get_String_Name (Stop_Mode_Str);
      Transfo_Pkg             := Get_String_Name (Transfo_Pkg_Str);
      Cheddar_Pkg             := Get_String_Name (Cheddar_Pkg_Str);
      Raw_Priority            := Get_String_Name (Raw_Priority_Str);
      Raw_Fixed_Priority      := Get_String_Name (Raw_Fixed_Priority_Str);
      Raw_Occurred            := Get_String_Name (Raw_Occurred_Str);
      Raw_Data_Representation := Get_String_Name (Raw_Data_Str);
      Data_Representation     := Get_String_Name (Data_Representation_Str);
      Data_Model_Pkg          := Get_String_Name (Data_Model_Pkg_Str);

      Node_Counters.Init (Counters);
      Node_Priorities.Init (Priorities);
      Node_Mapping.Init (Th1_Mapping);
      Node_Mapping.Init (Th2_Mapping);

      Dummy_Internal_Type := No_Node;
      Total_WCET          := 0;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
      use Ocarina.Analyzer.AADL.Semantics;
   begin
      Node_Counters.Init (Counters);
      Node_Priorities.Init (Priorities);
      Node_Mapping.Init (Th1_Mapping);
      Node_Mapping.Init (Th2_Mapping);
      Dummy_Internal_Type := No_Node;
      Total_WCET          := 0;
      if Present (AADL_Root) then
         Reset_All_Connections (AADL_Root);
      end if;
   end Reset;

   -------------------------
   -- Is_Defined_Property --
   -------------------------

   function Is_Defined_Property
     (Component : Node_Id;
      Property  : Name_Id) return Boolean
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type);

      package OAQ renames Ocarina.Analyzer.AADL.Queries;

      Decl : Node_Id;
   begin
      if Kind (Component) = K_Component_Implementation then
         Decl := Corresponding_Entity (Component_Type_Identifier (Component));
         if OAQ.Is_Defined_Property (Component, Property) then
            return True;
         elsif OAQ.Is_Defined_Property (Decl, Property) then
            return True;
         else
            return False;
         end if;
      elsif Kind (Component) = K_Component_Type then
         return OAQ.Is_Defined_Property (Component, Property);
      end if;

      return False;
   end Is_Defined_Property;

   --------------------------
   -- Get_Integer_Property --
   --------------------------

   function Get_Integer_Property
     (Component : Node_Id;
      Property  : Name_Id) return Unsigned_Long_Long
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type);

      package OAQ renames Ocarina.Analyzer.AADL.Queries;

      Decl : Node_Id;
   begin
      if Kind (Component) = K_Component_Implementation then
         Decl := Corresponding_Entity (Component_Type_Identifier (Component));
         if OAQ.Is_Defined_Integer_Property (Component, Property) then
            return OAQ.Get_Integer_Property (Component, Property);
         elsif OAQ.Is_Defined_Integer_Property (Decl, Property) then
            return OAQ.Get_Integer_Property (Decl, Property);
         else
            Display_Located_Error
              (Loc (Component),
               "integer property " & Get_Name_String (Property) & " no found",
               Fatal => True);
         end if;
      elsif Kind (Component) = K_Component_Type then
         if OAQ.Is_Defined_Integer_Property (Component, Property) then
            return OAQ.Get_Integer_Property (Component, Property);
         end if;
      end if;

      Display_Located_Error
        (Loc (Component),
         "integer property " & Get_Name_String (Property) & " no found",
         Fatal => True);
      return 0;
   end Get_Integer_Property;

   --------------------------
   -- Get_Boolean_Property --
   --------------------------

   function Get_Boolean_Property
     (Component : Node_Id;
      Property  : Name_Id) return Boolean
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type);

      package OAQ renames Ocarina.Analyzer.AADL.Queries;

      Decl : Node_Id;
   begin
      if Kind (Component) = K_Component_Implementation then
         Decl := Corresponding_Entity (Component_Type_Identifier (Component));
         if OAQ.Is_Defined_Boolean_Property (Component, Property) then
            return OAQ.Get_Boolean_Property (Component, Property);
         elsif OAQ.Is_Defined_Boolean_Property (Decl, Property) then
            return OAQ.Get_Boolean_Property (Decl, Property);
         else
            Display_Located_Error
              (Loc (Component),
               "boolean property " & Get_Name_String (Property) & " no found",
               Fatal => True);
         end if;
      elsif Kind (Component) = K_Component_Type then
         if OAQ.Is_Defined_Boolean_Property (Component, Property) then
            return OAQ.Get_Boolean_Property (Component, Property);
         end if;
      end if;

      Display_Located_Error
        (Loc (Component),
         "boolean property " & Get_Name_String (Property) & " no found",
         Fatal => True);
      return False;
   end Get_Boolean_Property;

   ------------------------------
   -- Get_Property_Association --
   ------------------------------

   function Get_Property_Association
     (Component : Node_Id;
      Property  : Name_Id) return Node_Id
   is
      package OAQ renames Ocarina.Analyzer.AADL.Queries;

      Decl : Node_Id;
   begin
      if Kind (Component) = K_Component_Implementation then
         Decl := Corresponding_Entity (Component_Type_Identifier (Component));
         if OAQ.Is_Defined_Property (Component, Property) then
            return OAQ.Get_Property_Association (Component, Property);
         elsif OAQ.Is_Defined_Property (Decl, Property) then
            return OAQ.Get_Property_Association (Decl, Property);
         else
            Display_Located_Error
              (Loc (Component),
               "property " & Get_Name_String (Property) & " no found",
               Fatal => True);
         end if;
      elsif Kind (Component) = K_Component_Type then
         if OAQ.Is_Defined_Property (Component, Property) then
            return OAQ.Get_Property_Association (Component, Property);
         end if;
      end if;

      Display_Located_Error
        (Loc (Component),
         "property " & Get_Name_String (Property) & " no found",
         Fatal => True);
      return No_Node;
   end Get_Property_Association;

end Ocarina.Transfo.Fusions;
