------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 O C A R I N A . T R A N S F O . M O V E                  --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Debug;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with GNAT.Dynamic_Tables;
with Ocarina.Analyzer.AADL.Semantics;
with Ocarina.Builder.AADL.Components;
with Ocarina.Builder.AADL.Components.Subcomponents;
with Ocarina.Builder.AADL.Components.Connections;
with Ocarina.Builder.AADL.Components.Features;
with Ocarina.Transfo;
with Ocarina.Namet;
with Locations;

package body Ocarina.Transfo.Move is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Debug;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.Namet;
   use Locations;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   package Distant_Nodes is new GNAT.Dynamic_Tables
     (Table_Component_Type => Node_Value,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,  -- # of elements
      Table_Increment      => 50); -- % increase

   procedure Manage_Connections
     (New_Thread_Subcomp :     Node_Id;
      Old_Thread_Subcomp :     Node_Id;
      Src_Process        :     Node_Id;
      Dst_Process        :     Node_Id;
      Success            : out Boolean);
   --  Redirect all connections crossing the moved thread

   function Find_Distant_Sources
     (Port      : Node_Id;
      Container : Node_Id) return Distant_Nodes.Instance;
   --  Return all connections that have a source of the port
   --  in connections of the container

   function Find_Distant_Destinations
     (Port      : Node_Id;
      Container : Node_Id) return Distant_Nodes.Instance;
   --  Return all connections that have a destination of the port
   --  in connections of the container

   System      : Node_Id;
   System_Impl : Node_Id;

   --  The system containing the processes

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      null;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset (AADL_Root : Node_Id) is
      use Ocarina.Analyzer.AADL.Semantics;
   begin
      Reset_All_Connections (AADL_Root);
   end Reset;

   ----------
   -- Move --
   ----------

   procedure Move_Thread
     (Thread_Name      : Name_Id;
      Old_Process_Name : Name_Id;
      New_Process_Name : Name_Id)
   is
      use Ocarina.Builder.AADL.Components;
      use Ocarina.Builder.AADL.Components.Subcomponents;

      --  Instances
      Thread_Inst      : Node_Id;
      Old_Process_Inst : Node_Id;
      New_Process_Inst : Node_Id;

      --  Declarations
      Old_Process : Node_Id;
      New_Process : Node_Id;

      New_Thread_Name : Name_Id;
      New_Thread_Id   : Node_Id;
      New_Thread      : Node_Id;

      Success : Boolean;
   begin
      --  Search parameters

      Old_Process_Inst :=
        Search_Process_By_Name (Get_Name_String (Old_Process_Name));
      if No (Old_Process_Inst) then
         raise Program_Error
           with "process " & Get_Name_String (Old_Process_Name) & " unknown";
      end if;
      New_Process_Inst :=
        Search_Process_By_Name (Get_Name_String (New_Process_Name));
      if No (New_Process_Inst) then
         raise Program_Error
           with "process " & Get_Name_String (New_Process_Name) & " unknown";
      end if;
      Thread_Inst :=
        Search_Thread_By_Name
          (Old_Process_Inst,
           Get_Name_String (Thread_Name));
      if No (Thread_Inst) then
         raise Program_Error
           with "thread " & Get_Name_String (Thread_Name) & " unknown";
      end if;
      Old_Process_Inst := AIN.Parent_Subcomponent (Old_Process_Inst);
      New_Process_Inst := AIN.Parent_Subcomponent (New_Process_Inst);

      Old_Process :=
        AIN.Corresponding_Declaration
          (AIN.Corresponding_Instance (Old_Process_Inst));
      New_Process :=
        AIN.Corresponding_Declaration
          (AIN.Corresponding_Instance (New_Process_Inst));

      --  Assign system

      System := AIN.Parent_Component (Old_Process_Inst);
      if System /= AIN.Parent_Component (New_Process_Inst) then
         raise Program_Error
           with "A thread cannot be moved between " &
           "processes belonging to different systems";
      end if;

      System_Impl := AIN.Corresponding_Declaration (System);
      if Present (Parent (System_Impl))
        and then Kind (Parent (System_Impl)) = K_Entity_Reference
      then
         --  In this case, the system is actually extended
         --  relevant subcomponents are in the original system

         System_Impl := Entity (Parent (System_Impl));
      end if;

      --  Cut & paste the thread from old_process to new_process

      Remove_Node_From_List
        (AIN.Corresponding_Declaration (Thread_Inst),
         ATN.Subcomponents (Old_Process));

      New_Thread_Name :=
        Build_Unique_Subcomponent_Name (New_Process, Thread_Prefix);
      New_Thread_Id :=
        Make_Identifier
          (No_Location,
           New_Thread_Name,
           New_Thread_Name,
           No_Node);

      --  FIXME :
      --  Should handle priority_shifter object
      --  for each access to a priority shifter, if such an object is
      --  not subcomponent of the (new) parent process, add it.

      New_Thread :=
        Add_New_Subcomponent
          (No_Location,
           New_Thread_Id,
           New_Process,
           CC_Thread);
      if Present (New_Thread) then
         Set_Entity_Ref
           (New_Thread,
            Entity_Ref (AIN.Corresponding_Declaration (Thread_Inst)));
      else
         raise Program_Error
           with "could not create a new subcomponent in " &
           Get_Name_String (New_Process_Name);
      end if;

      --  Manage connections

      Manage_Connections
        (New_Thread,
         AIN.Corresponding_Declaration (Thread_Inst),
         AIN.Corresponding_Declaration (Old_Process_Inst),
         AIN.Corresponding_Declaration (New_Process_Inst),
         Success);
      if not Success then
         raise Program_Error;
      end if;
   end Move_Thread;

   --------------------------
   -- Find_Distant_Sources --
   --------------------------

   function Find_Distant_Sources
     (Port      : Node_Id;
      Container : Node_Id) return Distant_Nodes.Instance
   is
      pragma Assert (Kind (Port) = K_Port_Spec);
      pragma Assert (Kind (Container) = K_Component_Implementation);

      Cnx     : Node_Id := No_Node;
      Sources : Distant_Nodes.Instance;
      N       : Node_Value;
   begin
      Distant_Nodes.Init (Sources);

      if not Is_Empty (Connections (Container)) then
         Cnx := First_Node (Connections (Container));
      end if;
      while Present (Cnx) loop
         --  If the destination is the target port then

         if Corresponding_Entity
             (Item (Last_Node (Path (Destination (Cnx))))) =
           Port
         then

            --  Add the related source port in the sources list

            N.Node := Cnx;
            Distant_Nodes.Append (Sources, N);
         end if;

         Cnx := Next_Node (Cnx);
      end loop;

      return Sources;
   end Find_Distant_Sources;

   -------------------------------
   -- Find_Distant_Destinations --
   -------------------------------

   function Find_Distant_Destinations
     (Port      : Node_Id;
      Container : Node_Id) return Distant_Nodes.Instance
   is
      pragma Assert (Kind (Port) = K_Port_Spec);
      pragma Assert (Kind (Container) = K_Component_Implementation);

      Cnx  : Node_Id := No_Node;
      Dest : Distant_Nodes.Instance;
      N    : Node_Value;
   begin
      N.Node := No_Node;
      Distant_Nodes.Init (Dest);

      if not Is_Empty (Connections (Container)) then
         Cnx := First_Node (Connections (Container));
      end if;

      while Present (Cnx) loop
         --  If the source is the target port then

         if Corresponding_Entity (Item (Last_Node (Path (Source (Cnx))))) =
           Port
         then

            --  Add the related source port in the sources list

            N.Node := Cnx;
            Distant_Nodes.Append (Dest, N);
         end if;

         Cnx := Next_Node (Cnx);
      end loop;

      return Dest;
   end Find_Distant_Destinations;

   ------------------------
   -- Manage_Connections --
   ------------------------

   procedure Manage_Connections
     (New_Thread_Subcomp :     Node_Id;
      Old_Thread_Subcomp :     Node_Id;
      Src_Process        :     Node_Id;
      Dst_Process        :     Node_Id;
      Success            : out Boolean)
   is
      use Ocarina.Builder.AADL.Components.Connections;
      use Ocarina.Builder.AADL.Components.Features;
      use Ocarina.ME_AADL.AADL_Tree.Entities;

      pragma Assert
        (Kind (Src_Process) = K_Subcomponent
         and then Kind (Dst_Process) = K_Subcomponent);

      Cnt              : Natural          := 0;
      Cnx              : Node_Id          := No_Node;
      Src_Process_Impl : constant Node_Id := Entity (Entity_Ref (Src_Process));
      Dst_Process_Impl : constant Node_Id := Entity (Entity_Ref (Dst_Process));
      To_Add_To_Src    : Distant_Nodes.Instance;
   begin
      if not Is_Empty (Connections (Src_Process_Impl)) then
         Cnx := First_Node (Connections (Src_Process_Impl));
      end if;
      Distant_Nodes.Init (To_Add_To_Src);
      while Present (Cnx) loop
         declare
            Dst : constant Node_Id :=
              Corresponding_Entity
                (Item (First_Node (Path (Destination (Cnx)))));
            Src : constant Node_Id :=
              Corresponding_Entity (Item (First_Node (Path (Source (Cnx)))));
            Dst_Feature : constant Node_Id :=
              Corresponding_Entity
                (Item (Last_Node (Path (Destination (Cnx)))));

            Src_Is_Moved_Thread : Boolean;
            Dst_Is_Moved_Thread : Boolean;
            Origins             : Distant_Nodes.Instance;
            In_Proc             : Distant_Nodes.Instance;
            Thread_Origin       : Distant_Nodes.Instance;
            P, P_Proc           : Node_Id;
            New_Port            : Node_Id := No_Node;
            Dst_P               : Node_Id := No_Node;
            Keep                : Boolean;
         begin
            Src_Is_Moved_Thread :=
              (Kind (Src) = K_Subcomponent and then Src = Old_Thread_Subcomp);

            Dst_Is_Moved_Thread :=
              (Kind (Dst) = K_Subcomponent and then Dst = Old_Thread_Subcomp);

            if Src_Is_Moved_Thread and then Dst_Is_Moved_Thread then
               --  handle self connections
               --  simply copy the connection into the destination process,
               --  with a new name
               declare
                  N : constant Name_Id :=
                    Build_Unique_Name
                      (Connections (Dst_Process_Impl),
                       Connection_Prefix);
                  E : constant Node_Id :=
                    New_Node (K_Entity_Reference, No_Location);
                  F : constant Node_Id :=
                    New_Node (K_Entity_Reference, No_Location);
                  SN : Name_Id;
                  C  : Node_Id;
                  CT : Connection_Type := CT_Parameter;
                  L  : List_Id;
               begin
                  L := New_List (K_List_Id, No_Location);
                  C := New_Node (K_Node_Container, No_Location);
                  Set_Item
                    (C,
                     Make_Identifier
                       (No_Location,
                        Name (Identifier (New_Thread_Subcomp)),
                        Name (Identifier (New_Thread_Subcomp)),
                        No_Node));
                  Append_Node_To_List (C, L);
                  C := New_Node (K_Node_Container, No_Location);
                  Set_Item (C, Item (Last_Node (Path (Source (Cnx)))));
                  Append_Node_To_List (C, L);
                  Set_Path (E, L);
                  SN := Build_Name_From_Path (L);
                  Set_Identifier
                    (E,
                     Make_Identifier (No_Location, SN, SN, No_Node));

                  L := New_List (K_List_Id, No_Location);
                  C := New_Node (K_Node_Container, No_Location);
                  Set_Item
                    (C,
                     Make_Identifier
                       (No_Location,
                        Name (Identifier (New_Thread_Subcomp)),
                        Name (Identifier (New_Thread_Subcomp)),
                        No_Node));
                  Append_Node_To_List (C, L);
                  C := New_Node (K_Node_Container, No_Location);
                  Set_Item (C, Item (Last_Node (Path (Destination (Cnx)))));
                  Append_Node_To_List (C, L);
                  Set_Path (F, L);
                  SN := Build_Name_From_Path (L);
                  Set_Identifier
                    (F,
                     Make_Identifier (No_Location, SN, SN, No_Node));

                  if Is_Event (Dst_Feature) then
                     CT := CT_Port_Connection;
                  end if;
                  C :=
                    Add_New_Connection
                      (No_Location,
                       Make_Identifier (No_Location, N, N, No_Node),
                       Dst_Process_Impl,
                       Category    => CT,
                       Source      => E,
                       Destination => F);
                  if No (C) then
                     raise Program_Error;
                  end if;

                  --  Remove local connection in src_process

                  Remove_Node_From_List (Cnx, Connections (Src_Process_Impl));
               end;

            elsif Src_Is_Moved_Thread then
               case Kind (Dst) is
                  when K_Port_Spec =>
                     --  Connection from a process' port to the thread

                     --  Find the destination of the process' port

                     Origins := Find_Distant_Destinations (Dst, System_Impl);

                     --  Check wheither their is others sources for the
                     --  destination

                     In_Proc :=
                       Find_Distant_Destinations (Dst, Src_Process_Impl);
                     Keep := (Distant_Nodes.Last (In_Proc) > 1);
                     Distant_Nodes.Free (In_Proc);

                     for I in
                       Distant_Nodes.First .. Distant_Nodes.Last (Origins)
                     loop
                        P :=
                          Corresponding_Entity
                            (Item
                               (Last_Node
                                  (Path
                                     (Destination (Origins.Table (I).Node)))));
                        P_Proc :=
                          Corresponding_Entity
                            (Item
                               (First_Node
                                  (Path
                                     (Destination (Origins.Table (I).Node)))));

                        if P_Proc = Dst_Process then
                           --  the destination belong to the process
                           --  the moved thread will be moved to

                           --  Connection become local :

                           --  1/ Add connection from source thread to
                           --     destination thread

                           Thread_Origin :=
                             Find_Distant_Destinations (P, Dst_Process_Impl);

                           for J in
                             Distant_Nodes.First ..
                                 Distant_Nodes.Last (Thread_Origin)
                           loop
                              declare
                                 C2 : constant Node_Id :=
                                   Thread_Origin.Table (J).Node;
                                 Src_Port : constant Name_Id :=
                                   Name
                                     (Item (Last_Node (Path (Source (Cnx)))));
                                 Dst_Port : constant Name_Id :=
                                   Name
                                     (Item
                                        (Last_Node (Path (Destination (C2)))));
                                 Dst_Subcomp : constant Name_Id :=
                                   Name
                                     (Item
                                        (First_Node
                                           (Path (Destination (C2)))));
                                 N : constant Name_Id :=
                                   Build_Unique_Name
                                     (Connections (Dst_Process_Impl),
                                      Connection_Prefix);
                                 Path : List_Id :=
                                   New_List (K_List_Id, No_Location);
                                 C : Node_Id;
                                 E : constant Node_Id :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 F : constant Node_Id :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 SN : Name_Id;
                              begin
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Name (Identifier (New_Thread_Subcomp)),
                                       Name (Identifier (New_Thread_Subcomp)),
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Src_Port,
                                       Src_Port,
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 Set_Path (E, Path);
                                 SN := Build_Name_From_Path (Path);
                                 Set_Identifier
                                   (E,
                                    Make_Identifier
                                      (No_Location,
                                       SN,
                                       SN,
                                       No_Node));

                                 Path := New_List (K_List_Id, No_Location);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Dst_Subcomp,
                                       Dst_Subcomp,
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 if Dst_Subcomp /= Dst_Port then
                                    C :=
                                      New_Node (K_Node_Container, No_Location);
                                    Set_Item
                                      (C,
                                       Make_Identifier
                                         (No_Location,
                                          Dst_Port,
                                          Dst_Port,
                                          No_Node));
                                    Append_Node_To_List (C, Path);
                                 end if;

                                 Set_Path (F, Path);
                                 SN := Build_Name_From_Path (Path);
                                 Set_Identifier
                                   (F,
                                    Make_Identifier
                                      (No_Location,
                                       SN,
                                       SN,
                                       No_Node));

                                 C :=
                                   Add_New_Connection
                                     (No_Location,
                                      Make_Identifier
                                        (No_Location,
                                         N,
                                         N,
                                         No_Node),
                                      Dst_Process_Impl,
                                      Category    => CT_Port_Connection,
                                      Source      => E,
                                      Destination => F);
                                 if No (C) then
                                    raise Program_Error;
                                 end if;

                                 --  Remove connection from destination thread
                                 --  to its process (always)

                                 Remove_Node_From_List
                                   (C2,
                                    Connections (Dst_Process_Impl));
                              end;
                           end loop;

                           --  Remove connection from source thread to
                           --  its process

                           if not Keep then
                              --  Check wheither their is others sources
                              --  for the destination process port

                              In_Proc := Find_Distant_Sources (P, System_Impl);

                              if Distant_Nodes.Last (In_Proc) <= 1 then
                                 Distant_Nodes.Free (In_Proc);
                                 Remove_Node_From_List
                                   (Origins.Table (I).Node,
                                    Connections (System_Impl));
                              end if;
                           end if;
                           Distant_Nodes.Free (Thread_Origin);

                        else
                           --  the destination belong to a process
                           --  the moved thread will *not* be moved to

                           --  1/ Create a new (out) port in dst_process

                           declare
                              E, F : Node_Id;
                              Path : List_Id :=
                                New_List (K_List_Id, No_Location);
                              C      : Node_Id;
                              SN     : Name_Id;
                              N1     : Name_Id;
                              N2     : Name_Id;
                              Proc_N : constant Name_Id :=
                                Name
                                  (Item
                                     (First_Node
                                        (ATN.Path
                                           (Destination
                                              (Origins.Table (I).Node)))));
                              Port_N : constant Name_Id :=
                                Name
                                  (Item
                                     (Last_Node
                                        (ATN.Path
                                           (Destination
                                              (Origins.Table (I).Node)))));
                              NM     : Name_Id;
                              Data_E : Node_Id;
                              NSN    : Name_Id;
                           begin
                              if No (New_Port) then
                                 if Present (Entity_Ref (Dst)) then
                                    NSN :=
                                      Name
                                        (Identifier
                                           (Namespace
                                              (Entity (Entity_Ref (Dst)))));
                                    Data_E :=
                                      New_Node
                                        (K_Entity_Reference,
                                         No_Location);
                                    Set_Identifier
                                      (Data_E,
                                       Identifier (Entity_Ref (Dst)));
                                    Set_Namespace_Identifier
                                      (Data_E,
                                       Make_Identifier
                                         (No_Location,
                                          NSN,
                                          NSN,
                                          No_Node));
                                 else
                                    Data_E := No_Node;
                                 end if;

                                 NM :=
                                   Build_Unique_Name
                                     (Features
                                        (Corresponding_Entity
                                           (Component_Type_Identifier
                                              (Dst_Process_Impl))),
                                      Port_N);
                                 New_Port :=
                                   Add_New_Port_Spec
                                     (Loc  => No_Location,
                                      Name =>
                                        Make_Identifier
                                          (No_Location,
                                           NM,
                                           NM,
                                           No_Node),
                                      Container =>
                                        Corresponding_Entity
                                          (Component_Type_Identifier
                                             (Dst_Process_Impl)),
                                      Is_In             => Is_In (Dst),
                                      Is_Out            => Is_Out (Dst),
                                      Is_Data           => Is_Data (Dst),
                                      Is_Event          => Is_Event (Dst),
                                      Is_Feature        => Is_Feature (Dst),
                                      Is_Refinement     => False,
                                      Associated_Entity => Data_E);
                                 if No (New_Port) then
                                    raise Program_Error;
                                 end if;

                                 --  2/ Add connection from the moved thread
                                 --     port to dst_process new port

                                 N1 :=
                                   Build_Unique_Name
                                     (Connections (Dst_Process_Impl),
                                      Connection_Prefix);
                                 E :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       NM,
                                       NM,
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 Set_Path (E, Path);
                                 SN := Build_Name_From_Path (Path);
                                 Set_Identifier
                                   (E,
                                    Make_Identifier
                                      (No_Location,
                                       SN,
                                       SN,
                                       No_Node));

                                 F :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 Path := New_List (K_List_Id, No_Location);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Name (Identifier (New_Thread_Subcomp)),
                                       Name (Identifier (New_Thread_Subcomp)),
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Name
                                         (Item
                                            (Last_Node
                                               (ATN.Path (Source (Cnx))))),
                                       Name
                                         (Item
                                            (Last_Node
                                               (ATN.Path (Source (Cnx))))),
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 Set_Path (F, Path);
                                 SN := Build_Name_From_Path (Path);
                                 Set_Identifier
                                   (F,
                                    Make_Identifier
                                      (No_Location,
                                       SN,
                                       SN,
                                       No_Node));

                                 C :=
                                   Add_New_Connection
                                     (No_Location,
                                      Make_Identifier
                                        (No_Location,
                                         N1,
                                         N1,
                                         No_Node),
                                      Dst_Process_Impl,
                                      Category    => CT_Port_Connection,
                                      Source      => F,
                                      Destination => E);
                                 if No (C) then
                                    raise Program_Error;
                                 end if;
                              else
                                 NM := Name (Identifier (New_Port));
                              end if;

                              --  3/ Add connection from dst_process new_port
                              --     to the third process' port

                              F := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);

                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Name (Identifier (Dst_Process)),
                                    Name (Identifier (Dst_Process)),
                                    No_Node));
                              Append_Node_To_List (C, Path);

                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM,
                                    NM,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (F, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (F,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              E := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Proc_N,
                                    Proc_N,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Port_N,
                                    Port_N,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (E, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (E,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              N2 :=
                                Build_Unique_Name
                                  (Connections (System_Impl),
                                   Connection_Prefix);
                              C :=
                                Add_New_Connection
                                  (No_Location,
                                   Make_Identifier
                                     (No_Location,
                                      N2,
                                      N2,
                                      No_Node),
                                   System_Impl,
                                   Category    => CT_Port_Connection,
                                   Source      => F,
                                   Destination => E);
                              if No (C) then
                                 raise Program_Error;
                              end if;
                           end;
                        end if;

                        --  Delete old connection in system

                        if not Keep then
                           Remove_Node_From_List
                             (Origins.Table (I).Node,
                              Connections (System_Impl));
                        end if;
                     end loop;

                     --  Remove local connection in src_process

                     Remove_Node_From_List
                       (Cnx,
                        Connections (Src_Process_Impl));

                     Distant_Nodes.Free (Origins);

                  when K_Subcomponent =>
                     --  FIXME :
                     --  Should generate a single port in the destination
                     --  process for all connections leaving the same
                     --  out feature of the thread

                     case Get_Category_Of_Component (Entity (Entity_Ref (Dst)))
                     is
                        when CC_Thread =>
                           --  Thread to thread connection in the source
                           --  process

                           declare
                              Src_Port : constant Node_Id :=
                                Corresponding_Entity
                                  (Item (Last_Node (Path (Source (Cnx)))));
                              Src_Port_Name : constant Name_Id :=
                                Name (Item (Last_Node (Path (Source (Cnx)))));
                              Dst_Port_Name : constant Name_Id :=
                                Name
                                  (Item
                                     (Last_Node (Path (Destination (Cnx)))));
                              Dst_Subcomp_Name : constant Name_Id :=
                                Name
                                  (Item
                                     (First_Node (Path (Destination (Cnx)))));
                              E, F, C  : Node_Id;
                              Path     : List_Id;
                              NM1, NM2 : Name_Id;
                              SN, N    : Name_Id;
                              Node     : Node_Value;
                              Src_P    : Node_Id;
                              Data_E   : Node_Id;
                              NSN      : Name_Id;
                           begin
                              --  Build the referenced data

                              if Present (Entity_Ref (Src_Port)) then
                                 NSN :=
                                   Name
                                     (Identifier
                                        (Namespace
                                           (Entity (Entity_Ref (Src_Port)))));
                                 Data_E :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 Set_Identifier
                                   (Data_E,
                                    Identifier (Entity_Ref (Src_Port)));
                                 Set_Namespace_Identifier
                                   (Data_E,
                                    Make_Identifier
                                      (No_Location,
                                       NSN,
                                       NSN,
                                       No_Node));
                              else
                                 Data_E := No_Node;
                              end if;

                              --  Add a new port in src_process

                              NM1 :=
                                Build_Unique_Name
                                  (Features
                                     (Corresponding_Entity
                                        (Component_Type_Identifier
                                           (Src_Process_Impl))),
                                   Name (Identifier (Src_Port)));
                              Src_P :=
                                Add_New_Port_Spec
                                  (Loc  => No_Location,
                                   Name =>
                                     Make_Identifier
                                       (No_Location,
                                        NM1,
                                        NM1,
                                        No_Node),
                                   Container =>
                                     Corresponding_Entity
                                       (Component_Type_Identifier
                                          (Src_Process_Impl)),
                                   Is_In             => True,
                                   Is_Out            => False,
                                   Is_Data           => Is_Data (Src_Port),
                                   Is_Event          => Is_Event (Src_Port),
                                   Is_Feature        => Is_Feature (Src_Port),
                                   Is_Refinement     => False,
                                   Associated_Entity => Data_E);
                              if No (Src_P) then
                                 raise Program_Error;
                              end if;

                              --  Add a new port in dst_process

                              NM2 :=
                                Build_Unique_Name
                                  (Features
                                     (Corresponding_Entity
                                        (Component_Type_Identifier
                                           (Dst_Process_Impl))),
                                   Name (Identifier (Src_Port)));
                              Dst_P :=
                                Add_New_Port_Spec
                                  (Loc  => No_Location,
                                   Name =>
                                     Make_Identifier
                                       (No_Location,
                                        NM2,
                                        NM2,
                                        No_Node),
                                   Container =>
                                     Corresponding_Entity
                                       (Component_Type_Identifier
                                          (Dst_Process_Impl)),
                                   Is_In             => False,
                                   Is_Out            => True,
                                   Is_Data           => Is_Data (Src_Port),
                                   Is_Event          => Is_Event (Src_Port),
                                   Is_Feature        => Is_Feature (Src_Port),
                                   Is_Refinement     => False,
                                   Associated_Entity => Data_E);
                              if No (Dst_P) then
                                 raise Program_Error;
                              end if;

                              --  Create a connection between them

                              F := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Name (Identifier (Dst_Process)),
                                    Name (Identifier (Dst_Process)),
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM2,
                                    NM2,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (F, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (F,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              E := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Name (Identifier (Src_Process)),
                                    Name (Identifier (Src_Process)),
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM1,
                                    NM1,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (E, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (E,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              N :=
                                Build_Unique_Name
                                  (Connections (System_Impl),
                                   Connection_Prefix);
                              C :=
                                Add_New_Connection
                                  (No_Location,
                                   Make_Identifier
                                     (No_Location,
                                      N,
                                      N,
                                      No_Node),
                                   System_Impl,
                                   Category    => CT_Port_Connection,
                                   Source      => F,
                                   Destination => E);
                              if No (C) then
                                 raise Program_Error;
                              end if;

                              --  Add a connection from moved_thread.port
                              --  to dst_process.new_port

                              F := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Name (Identifier (New_Thread_Subcomp)),
                                    Name (Identifier (New_Thread_Subcomp)),
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Src_Port_Name,
                                    Src_Port_Name,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (F, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (F,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              E := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM2,
                                    NM2,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (E, Path);
                              Set_Identifier
                                (E,
                                 Make_Identifier
                                   (No_Location,
                                    NM2,
                                    NM2,
                                    No_Node));

                              N :=
                                Build_Unique_Name
                                  (Connections (Dst_Process_Impl),
                                   Connection_Prefix);
                              C :=
                                Add_New_Connection
                                  (No_Location,
                                   Make_Identifier
                                     (No_Location,
                                      N,
                                      N,
                                      No_Node),
                                   Dst_Process_Impl,
                                   Category    => CT_Port_Connection,
                                   Source      => F,
                                   Destination => E);
                              if No (C) then
                                 raise Program_Error;
                              end if;

                              --  Add a connection from src_process.new_port
                              --  to destination thread

                              F := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM1,
                                    NM1,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (F, Path);
                              Set_Identifier
                                (F,
                                 Make_Identifier
                                   (No_Location,
                                    NM1,
                                    NM1,
                                    No_Node));

                              E := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Dst_Subcomp_Name,
                                    Dst_Subcomp_Name,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              if Dst_Subcomp_Name /= Dst_Port_Name then
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Dst_Port_Name,
                                       Dst_Port_Name,
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                              end if;
                              SN := Build_Name_From_Path (Path);
                              Set_Path (E, Path);
                              Set_Identifier
                                (E,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              N :=
                                Build_Unique_Name
                                  (Connections (Src_Process_Impl),
                                   Connection_Prefix,
                                   Cnt);
                              Cnt := Cnt + 1;
                              C   :=
                                New_Connection
                                  (No_Location,
                                   Make_Identifier
                                     (No_Location,
                                      N,
                                      N,
                                      No_Node),
                                   Category    => CT_Port_Connection,
                                   Source      => F,
                                   Destination => E);
                              if No (C) then
                                 raise Program_Error;
                              end if;
                              Node.Node := C;
                              Distant_Nodes.Append (To_Add_To_Src, Node);
                           end;

                           --  Remove local connection in src_process

                           Remove_Node_From_List
                             (Cnx,
                              Connections (Src_Process_Impl));

                        when CC_Data =>
                           null;

                        when others =>
                           W_Line
                             (Get_Name_String (Name (Identifier (Dst))) &
                              " : unexpected type");
                           Success := False;
                           return;
                     end case;

                  when others =>
                     W_Line
                       (Get_Name_String (Name (Identifier (Dst))) &
                        " : unexpected type");
                     Success := False;
                     return;
               end case;

            elsif Dst_Is_Moved_Thread then
               case Kind (Src) is
                  when K_Port_Spec =>
                     --  Connection from a process' port to the thread

                     --  Find the sources of the process' port

                     Origins := Find_Distant_Sources (Src, System_Impl);

                     --  Check wheither their is others destination for
                     --  the source

                     In_Proc :=
                       Find_Distant_Destinations (Src, Dst_Process_Impl);
                     Keep := (Distant_Nodes.Last (In_Proc) > 1);
                     Distant_Nodes.Free (In_Proc);

                     for I in
                       Distant_Nodes.First .. Distant_Nodes.Last (Origins)
                     loop
                        P :=
                          Corresponding_Entity
                            (Item
                               (Last_Node
                                  (Path (Source (Origins.Table (I).Node)))));
                        P_Proc :=
                          Corresponding_Entity
                            (Item
                               (First_Node
                                  (Path (Source (Origins.Table (I).Node)))));

                        if P_Proc = Dst_Process then

                           --  the source belong to the process
                           --  the moved thread will be moved to

                           --  Connection become local :

                           --  1/ Add connection from source thread to
                           --     destination thread

                           Thread_Origin :=
                             Find_Distant_Sources (P, Dst_Process_Impl);

                           for J in
                             Distant_Nodes.First ..
                                 Distant_Nodes.Last (Thread_Origin)
                           loop
                              declare
                                 C2 : constant Node_Id :=
                                   Thread_Origin.Table (J).Node;
                                 Dst_Port : constant Name_Id :=
                                   Name
                                     (Item
                                        (Last_Node
                                           (Path (Destination (Cnx)))));
                                 Src_Port : constant Name_Id :=
                                   Name
                                     (Item (Last_Node (Path (Source (C2)))));
                                 Src_Subcomp : constant Name_Id :=
                                   Name
                                     (Item (First_Node (Path (Source (C2)))));
                                 N : constant Name_Id :=
                                   Build_Unique_Name
                                     (Connections (Dst_Process_Impl),
                                      Connection_Prefix);
                                 Path : List_Id :=
                                   New_List (K_List_Id, No_Location);
                                 C : Node_Id;
                                 E : constant Node_Id :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 F : constant Node_Id :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 SN : Name_Id;
                              begin
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Name (Identifier (New_Thread_Subcomp)),
                                       Name (Identifier (New_Thread_Subcomp)),
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Dst_Port,
                                       Dst_Port,
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 Set_Path (E, Path);
                                 SN := Build_Name_From_Path (Path);
                                 Set_Identifier
                                   (E,
                                    Make_Identifier
                                      (No_Location,
                                       SN,
                                       SN,
                                       No_Node));

                                 Path := New_List (K_List_Id, No_Location);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Src_Subcomp,
                                       Src_Subcomp,
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 if Src_Port /= Src_Subcomp then
                                    C :=
                                      New_Node (K_Node_Container, No_Location);
                                    Set_Item
                                      (C,
                                       Make_Identifier
                                         (No_Location,
                                          Src_Port,
                                          Src_Port,
                                          No_Node));
                                    Append_Node_To_List (C, Path);
                                 end if;
                                 Set_Path (F, Path);
                                 SN := Build_Name_From_Path (Path);
                                 Set_Identifier
                                   (F,
                                    Make_Identifier
                                      (No_Location,
                                       SN,
                                       SN,
                                       No_Node));

                                 C :=
                                   Add_New_Connection
                                     (No_Location,
                                      Make_Identifier
                                        (No_Location,
                                         N,
                                         N,
                                         No_Node),
                                      Dst_Process_Impl,
                                      Category    => CT_Port_Connection,
                                      Source      => F,
                                      Destination => E);
                                 if No (C) then
                                    raise Program_Error;
                                 end if;

                                 --  Remove connection from destination thread
                                 --  to its process (always)

                                 Remove_Node_From_List
                                   (C2,
                                    Connections (Dst_Process_Impl));

                                 if not Keep then
                                    --  Check wheither their is others
                                    --  destinations for the source process
                                    --  port

                                    In_Proc :=
                                      Find_Distant_Destinations
                                        (P,
                                         System_Impl);

                                    --  Remove connection from source thread to
                                    --  its process

                                    if Distant_Nodes.Last (In_Proc) <= 1 then
                                       Distant_Nodes.Free (In_Proc);
                                       Remove_Node_From_List
                                         (Origins.Table (J).Node,
                                          Connections (Src_Process_Impl));
                                    end if;

                                 end if;
                              end;
                           end loop;

                           Distant_Nodes.Free (Thread_Origin);

                        else
                           --  the source belong to a process the
                           --  moved thread will *not* be moved to

                           --  1/ Create a new (in) port in dst_process

                           declare
                              E, F : Node_Id;
                              Path : List_Id :=
                                New_List (K_List_Id, No_Location);
                              C      : Node_Id;
                              SN     : Name_Id;
                              N1     : Name_Id;
                              N2     : Name_Id;
                              Proc_N : constant Name_Id :=
                                Name
                                  (Item
                                     (First_Node
                                        (ATN.Path
                                           (Source
                                              (Origins.Table (I).Node)))));
                              Port_N : constant Name_Id :=
                                Name
                                  (Item
                                     (Last_Node
                                        (ATN.Path
                                           (Source
                                              (Origins.Table (I).Node)))));
                              Dst_Port_Name : constant Name_Id :=
                                Name
                                  (Item
                                     (Last_Node
                                        (ATN.Path (Destination (Cnx)))));
                              NM     : Name_Id;
                              Data_E : Node_Id;
                              NSN    : Name_Id;
                           begin

                              --  We build a new port only once by
                              --  corresponding connection

                              if No (New_Port) then

                                 --  1/ Build the referenced data

                                 if Entity_Ref (Src) /= No_Node then
                                    NSN :=
                                      Name
                                        (Identifier
                                           (Namespace
                                              (Entity (Entity_Ref (Src)))));
                                    Data_E :=
                                      New_Node
                                        (K_Entity_Reference,
                                         No_Location);
                                    Set_Identifier
                                      (Data_E,
                                       Identifier (Entity_Ref (Src)));
                                    Set_Namespace_Identifier
                                      (Data_E,
                                       Make_Identifier
                                         (No_Location,
                                          NSN,
                                          NSN,
                                          No_Node));
                                 else
                                    Data_E := No_Node;
                                 end if;

                                 --  Build the port spec

                                 NM :=
                                   Build_Unique_Name
                                     (Features
                                        (Corresponding_Entity
                                           (Component_Type_Identifier
                                              (Dst_Process_Impl))),
                                      Port_N);
                                 New_Port :=
                                   Add_New_Port_Spec
                                     (Loc  => No_Location,
                                      Name =>
                                        Make_Identifier
                                          (No_Location,
                                           NM,
                                           NM,
                                           No_Node),
                                      Container =>
                                        Corresponding_Entity
                                          (Component_Type_Identifier
                                             (Dst_Process_Impl)),
                                      Is_In             => Is_In (Src),
                                      Is_Out            => Is_Out (Src),
                                      Is_Data           => Is_Data (Src),
                                      Is_Event          => Is_Event (Src),
                                      Is_Feature        => Is_Feature (Src),
                                      Is_Refinement     => False,
                                      Associated_Entity => Data_E);
                                 if No (New_Port) then
                                    raise Program_Error;
                                 end if;

                                 --  2/ Add connection from dst_process new
                                 --     port to the moved thread port

                                 N1 :=
                                   Build_Unique_Name
                                     (Connections (Dst_Process_Impl),
                                      Connection_Prefix);
                                 F :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Name (Identifier (New_Thread_Subcomp)),
                                       Name (Identifier (New_Thread_Subcomp)),
                                       New_Thread_Subcomp));
                                 Append_Node_To_List (C, Path);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       Dst_Port_Name,
                                       Dst_Port_Name,
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 Set_Path (F, Path);
                                 SN := Build_Name_From_Path (Path);
                                 Set_Identifier
                                   (F,
                                    Make_Identifier
                                      (No_Location,
                                       SN,
                                       SN,
                                       No_Node));

                                 E :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 C := New_Node (K_Node_Container, No_Location);
                                 Set_Item
                                   (C,
                                    Make_Identifier
                                      (No_Location,
                                       NM,
                                       NM,
                                       No_Node));
                                 Append_Node_To_List (C, Path);
                                 Set_Path (E, Path);
                                 Set_Identifier
                                   (E,
                                    Make_Identifier
                                      (No_Location,
                                       NM,
                                       NM,
                                       No_Node));

                                 C :=
                                   Add_New_Connection
                                     (No_Location,
                                      Make_Identifier
                                        (No_Location,
                                         N1,
                                         N1,
                                         No_Node),
                                      Dst_Process_Impl,
                                      Category    => CT_Port_Connection,
                                      Source      => E,
                                      Destination => F);
                                 if No (C) then
                                    raise Program_Error;
                                 end if;
                              else
                                 NM := Name (Identifier (New_Port));
                              end if;

                              --  3/ Add connection from the third process'
                              --     port to dst_process new_port

                              F := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Name (Identifier (Dst_Process)),
                                    Name (Identifier (Dst_Process)),
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM,
                                    NM,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (F, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (F,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              E := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Proc_N,
                                    Proc_N,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Port_N,
                                    Port_N,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (E, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (E,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              N2 :=
                                Build_Unique_Name
                                  (Connections (System_Impl),
                                   Connection_Prefix);
                              C :=
                                Add_New_Connection
                                  (No_Location,
                                   Make_Identifier
                                     (No_Location,
                                      N2,
                                      N2,
                                      No_Node),
                                   System_Impl,
                                   Category    => CT_Port_Connection,
                                   Source      => E,
                                   Destination => F);
                              if No (C) then
                                 raise Program_Error;
                              end if;
                           end;
                        end if;
                        if not Keep then
                           Remove_Node_From_List
                             (Origins.Table (I).Node,
                              Connections (System_Impl));
                        end if;
                     end loop;

                     --  Remove local connection in src_process

                     Remove_Node_From_List
                       (Cnx,
                        Connections (Src_Process_Impl));

                     Distant_Nodes.Free (Origins);

                  when K_Subcomponent =>
                     case Get_Category_Of_Component (Entity (Entity_Ref (Dst)))
                     is
                        when CC_Thread =>

                           --  Thread to thread connection in the source
                           --  process

                           declare
                              Src_Port : constant Node_Id :=
                                Corresponding_Entity
                                  (Item
                                     (Last_Node (Path (Destination (Cnx)))));
                              Dst_Port_Name : constant Name_Id :=
                                Name
                                  (Item
                                     (Last_Node
                                        (ATN.Path (Destination (Cnx)))));
                              Src_Port_Name : constant Name_Id :=
                                Name
                                  (Item (Last_Node (ATN.Path (Source (Cnx)))));
                              Src_Subcomp_Name : constant Name_Id :=
                                Name
                                  (Item
                                     (First_Node (ATN.Path (Source (Cnx)))));
                              E, F, C  : Node_Id;
                              Path     : List_Id;
                              NM1, NM2 : Name_Id;
                              SN, N    : Name_Id;
                              Node     : Node_Value;
                              Src_P    : Node_Id;
                              Dst_P    : Node_Id;
                              Data_E   : Node_Id;
                              NSN      : Name_Id;
                           begin
                              --  Build the referenced data

                              if Present (Entity_Ref (Src_Port)) then
                                 NSN :=
                                   Name
                                     (Identifier
                                        (Namespace
                                           (Entity (Entity_Ref (Src_Port)))));
                                 Data_E :=
                                   New_Node (K_Entity_Reference, No_Location);
                                 Set_Identifier
                                   (Data_E,
                                    Identifier (Entity_Ref (Src_Port)));
                                 Set_Namespace_Identifier
                                   (Data_E,
                                    Make_Identifier
                                      (No_Location,
                                       NSN,
                                       NSN,
                                       No_Node));
                              else
                                 Data_E := No_Node;
                              end if;

                              --  Add a new (in) port in src_process

                              NM1 :=
                                Build_Unique_Name
                                  (Features
                                     (Corresponding_Entity
                                        (Component_Type_Identifier
                                           (Src_Process_Impl))),
                                   Src_Port_Name);

                              Src_P :=
                                Add_New_Port_Spec
                                  (Loc  => No_Location,
                                   Name =>
                                     Make_Identifier
                                       (No_Location,
                                        NM1,
                                        NM1,
                                        No_Node),
                                   Container =>
                                     Corresponding_Entity
                                       (Component_Type_Identifier
                                          (Src_Process_Impl)),
                                   Is_In             => False,
                                   Is_Out            => True,
                                   Is_Data           => Is_Data (Src_Port),
                                   Is_Event          => Is_Event (Src_Port),
                                   Is_Feature        => Is_Feature (Src_Port),
                                   Is_Refinement     => False,
                                   Associated_Entity => Data_E);
                              if No (Src_P) then
                                 raise Program_Error;
                              end if;

                              --  Add a new port in dst_process

                              NM2 :=
                                Build_Unique_Name
                                  (Features
                                     (Corresponding_Entity
                                        (Component_Type_Identifier
                                           (Dst_Process_Impl))),
                                   Src_Port_Name);

                              Dst_P :=
                                Add_New_Port_Spec
                                  (Loc  => No_Location,
                                   Name =>
                                     Make_Identifier
                                       (No_Location,
                                        NM2,
                                        NM2,
                                        No_Node),
                                   Container =>
                                     Corresponding_Entity
                                       (Component_Type_Identifier
                                          (Dst_Process_Impl)),
                                   Is_In             => True,
                                   Is_Out            => False,
                                   Is_Data           => Is_Data (Src_Port),
                                   Is_Event          => Is_Event (Src_Port),
                                   Is_Feature        => Is_Feature (Src_Port),
                                   Is_Refinement     => False,
                                   Associated_Entity => Data_E);
                              if No (Dst_P) then
                                 raise Program_Error;
                              end if;

                              --  Create a connection between them

                              F := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Name (Identifier (Dst_Process)),
                                    Name (Identifier (Dst_Process)),
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM2,
                                    NM2,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (F, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (F,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              E := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Name (Identifier (Src_Process)),
                                    Name (Identifier (Src_Process)),
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM1,
                                    NM1,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (E, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (E,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              N :=
                                Build_Unique_Name
                                  (Connections (System_Impl),
                                   Connection_Prefix);
                              C :=
                                Add_New_Connection
                                  (No_Location,
                                   Make_Identifier
                                     (No_Location,
                                      N,
                                      N,
                                      No_Node),
                                   System_Impl,
                                   Category    => CT_Port_Connection,
                                   Source      => E,
                                   Destination => F);
                              if No (C) then
                                 raise Program_Error;
                              end if;

                              --  Add a connection from source thread
                              --  to src_process.new_port

                              E := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Src_Subcomp_Name,
                                    Src_Subcomp_Name,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Src_Port_Name,
                                    Src_Port_Name,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (E, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (E,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              F := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM1,
                                    NM1,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (F, Path);
                              Set_Identifier
                                (F,
                                 Make_Identifier
                                   (No_Location,
                                    NM1,
                                    NM1,
                                    No_Node));

                              N :=
                                Build_Unique_Name
                                  (Connections (Src_Process_Impl),
                                   Connection_Prefix,
                                   Cnt);
                              Cnt := Cnt + 1;
                              C   :=
                                New_Connection
                                  (No_Location,
                                   Make_Identifier
                                     (No_Location,
                                      N,
                                      N,
                                      No_Node),
                                   Category    => CT_Port_Connection,
                                   Source      => E,
                                   Destination => F);
                              if No (C) then
                                 raise Program_Error;
                              end if;
                              Node.Node := C;
                              Distant_Nodes.Append (To_Add_To_Src, Node);

                              --  Add a connection from dst_process.new_port
                              --  to moved_thread.port

                              E := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    NM2,
                                    NM2,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (E, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (E,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              F := New_Node (K_Entity_Reference, No_Location);
                              Path := New_List (K_List_Id, No_Location);
                              C    := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Name (Identifier (New_Thread_Subcomp)),
                                    Name (Identifier (New_Thread_Subcomp)),
                                    New_Thread_Subcomp));
                              Append_Node_To_List (C, Path);
                              C := New_Node (K_Node_Container, No_Location);
                              Set_Item
                                (C,
                                 Make_Identifier
                                   (No_Location,
                                    Dst_Port_Name,
                                    Dst_Port_Name,
                                    No_Node));
                              Append_Node_To_List (C, Path);
                              Set_Path (F, Path);
                              SN := Build_Name_From_Path (Path);
                              Set_Identifier
                                (F,
                                 Make_Identifier
                                   (No_Location,
                                    SN,
                                    SN,
                                    No_Node));

                              N :=
                                Build_Unique_Name
                                  (Connections (Dst_Process_Impl),
                                   Connection_Prefix);
                              C :=
                                Add_New_Connection
                                  (No_Location,
                                   Make_Identifier
                                     (No_Location,
                                      N,
                                      N,
                                      No_Node),
                                   Dst_Process_Impl,
                                   Category    => CT_Port_Connection,
                                   Source      => E,
                                   Destination => F);
                              if No (C) then
                                 raise Program_Error;
                              end if;
                           end;

                           --  Remove local connection in src_process

                           Remove_Node_From_List
                             (Cnx,
                              Connections (Src_Process_Impl));

                        when CC_Data =>
                           null;

                        when others =>
                           W_Line
                             (Get_Name_String (Name (Identifier (Src))) &
                              " : unexpected type");
                           Success := False;
                           return;
                     end case;

                  when others =>
                     W_Line
                       (Get_Name_String (Name (Identifier (Src))) &
                        " : unexpected type");
                     Success := False;
                     return;
               end case;
            end if;
         end;

         Cnx := Next_Node (Cnx);
      end loop;

      --  Actually append the connections

      for I in Distant_Nodes.First .. Distant_Nodes.Last (To_Add_To_Src) loop
         Success :=
           Ocarina.Builder.AADL.Components.Add_Connection
             (Src_Process_Impl,
              To_Add_To_Src.Table (I).Node);
         exit when not Success;
      end loop;
      Distant_Nodes.Free (To_Add_To_Src);

   end Manage_Connections;

   -----------------------------
   -- Clean_Obsolete_Features --
   -----------------------------

   function Clean_Obsolete_Features
     (System_Inst  : Node_Id;
      Process_Name : Name_Id) return Boolean
   is
      N              : Node_Id := No_Node;
      Srcs           : Distant_Nodes.Instance;
      Dsts           : Distant_Nodes.Instance;
      Keep           : Boolean;
      Component_Inst : Node_Id;
      Process_Inst   : Node_Id;
      Process        : Node_Id;
      Component      : Node_Id;
      System_Implem  : Node_Id;
      Modified       : Boolean := False;
   begin
      System_Implem := AIN.Corresponding_Declaration (System_Inst);
      if Present (Parent (System_Implem))
        and then Kind (Parent (System_Implem)) = K_Entity_Reference
      then
         --  In this case, the system is actually extended
         --  relevant subcomponents are in the original system

         System_Implem := Entity (Parent (System_Implem));
      end if;

      Component_Inst :=
        Search_Process_By_Name (Get_Name_String (Process_Name));
      if No (Component_Inst) then
         raise Program_Error
           with "process " & Get_Name_String (Process_Name) & " unknown";
      end if;
      Process_Inst := AIN.Parent_Subcomponent (Component_Inst);
      Component    :=
        AIN.Corresponding_Declaration
          (AIN.Corresponding_Instance (Process_Inst));
      Process := Corresponding_Entity (Component_Type_Identifier (Component));

      if not Is_Empty (Features (Process)) then
         N := First_Node (Features (Process));
      end if;
      while Present (N) loop
         case Kind (N) is
            when K_Port_Spec =>
               Keep := Is_Event (N);
               --  we do not delete event port as they can be refered by
               --  entrypoints
               --  FIXME : find actual port entrypoints (must use
               --  instances)

               --  Check if their is any source
               --  in the process

               if not Keep then
                  Distant_Nodes.Init (Srcs);
                  Srcs := Find_Distant_Sources (N, Component);
                  Keep := (Distant_Nodes.Last (Srcs) > 0);
                  Distant_Nodes.Free (Srcs);
               end if;

               --  Check if their is any source
               --  in the system

               if not Keep then
                  Distant_Nodes.Init (Srcs);
                  Srcs := Find_Distant_Sources (N, System_Implem);
                  Keep := (Distant_Nodes.Last (Srcs) > 0);
                  Distant_Nodes.Free (Srcs);
               end if;

               --  Check if their is any destination
               --  in the process

               if not Keep then
                  Distant_Nodes.Init (Dsts);
                  Dsts := Find_Distant_Destinations (N, Component);
                  Keep := (Distant_Nodes.Last (Dsts) > 0);
                  Distant_Nodes.Free (Dsts);
               end if;

               --  Check if their is any destination
               --  in another process

               if not Keep then
                  Distant_Nodes.Init (Dsts);
                  Dsts := Find_Distant_Destinations (N, System_Implem);
                  Keep := (Distant_Nodes.Last (Dsts) > 0);
                  Distant_Nodes.Free (Dsts);
               end if;

               if not Keep then
                  Remove_Node_From_List (N, Features (Process));
               end if;

            when others =>
               null;

         end case;

         Modified := Modified or else not Keep;

         N := Next_Node (N);
      end loop;

      return Modified;
   end Clean_Obsolete_Features;

end Ocarina.Transfo.Move;
