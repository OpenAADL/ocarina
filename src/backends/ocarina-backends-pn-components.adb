------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . P N . C O M P O N E N T S        --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.Namet;
with Ocarina.Backends.PN.Nodes;
with Ocarina.Backends.PN.Nutils;

with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Backends.Properties;
with Ocarina.AADL_Values;
with Ocarina.Instances.Queries;
with Ocarina.Backends.PN.Iutils;
with Ocarina.Backends.PN.Utils;
with Ocarina.Backends.Messages; use Ocarina.Backends.Messages;

with Ocarina.Backends.Utils;

with Ocarina.Backends.PN.Debug;

package body Ocarina.Backends.PN.Components is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package OPU renames Ocarina.Backends.PN.Nutils;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package OPN renames Ocarina.Backends.PN.Nodes;
   package OPND renames Ocarina.Backends.PN.Debug;

   package OAV renames Ocarina.AADL_Values;

   procedure PN_Process_Final_System
     (PN_Generated    : Node_Id;
      PN_P_Init_Proc  : PN_Init_Node;
      PN_P_New_Proc   : PN_New_Node;
      PN_A_Init_Proc  : PN_Init_Arc;
      PN_A_New_Proc   : PN_New_Node;
      PN_Dup_Arc_Proc : PN_Dup_Arc);

   function Process_Component_Instance
     (Instance     : Types.Node_Id;
      PN_Generated : Types.Node_Id) return Boolean;

   function Process_Thread_Instance
     (Aadl_Instance : Types.Node_Id;
      PN_Generated  : Types.Node_Id) return Boolean;

   procedure PN_Process_Thread_Pattern
     (Aadl_Instance  : Types.Node_Id;
      PN_Thread      : Types.Node_Id;
      PN_Generated   : Types.Node_Id;
      PN_P_Init_Proc : PN_Init_Node;
      PN_P_New_Proc  : PN_New_Node;
      PN_T_Init_Proc : PN_Init_Node;
      PN_T_New_Proc  : PN_New_Node;
      PN_A_Init_Proc : PN_Init_Arc;
      PN_A_New_Proc  : PN_New_Node);

   procedure PN_Thread_Skeleton
     (Aadl_Instance  : Types.Node_Id;
      PN_Thread      : Types.Node_Id;
      PN_Generated   : Types.Node_Id;
      PN_P_Init_Proc : PN_Init_Node;
      PN_P_New_Proc  : PN_New_Node;
      PN_T_Init_Proc : PN_Init_Node;
      PN_T_New_Proc  : PN_New_Node;
      PN_A_Init_Proc : PN_Init_Arc;
      PN_A_New_Proc  : PN_New_Node);

   procedure PN_Build_Spg_Par
     (Aadl_Instance  : Types.Node_Id;
      PN_Generated   : Types.Node_Id;
      Spg_Feat       : Types.Node_Id;
      PN_Spg_Call    : Types.Node_Id;
      In_Par         : Boolean := True;
      PN_P_Init_Proc : PN_Init_Node;
      PN_P_New_Proc  : PN_New_Node;
      PN_T_Init_Proc : PN_Init_Node;
      PN_T_New_Proc  : PN_New_Node;
      PN_A_Init_Proc : PN_Init_Arc;
      PN_A_New_Proc  : PN_New_Node);

   procedure PN_Build_Port
     (Aadl_Instance : Types.Node_Id;
      PN_Generated  : Types.Node_Id;
      PN_Thread     : Types.Node_Id;
      F             : Types.Value_Id);

   procedure PN_Build_Tpn_Port
     (Aadl_Instance : Node_Id;
      PN_Generated  : Types.Node_Id;
      Port_Instance : Node_Id;
      PN_Port       : Node_Id;
      Is_Data       : Boolean := True);

   procedure PN_Build_Cpn_Port
     (Aadl_Instance : Node_Id;
      PN_Generated  : Types.Node_Id;
      Port_Instance : Node_Id;
      PN_Port       : Node_Id;
      Is_Data       : Boolean := True);

   -----------------------------------
   -- Process_Architecture_Instance --
   -----------------------------------

   function Process_Architecture_Instance
     (Architecture_Instance : Types.Node_Id;
      F                     : Unsigned_Long_Long) return Types.Node_Id
   is
      use Ocarina.ME_AADL.AADL_Instances.Nodes;
      use Ocarina.Backends.PN.Iutils;
      use Ocarina.ME_AADL.AADL_Instances.Nutils;
      use Ocarina.AADL_Values;
      use Ocarina.Backends.PN.Nodes;

      Instance     : Node_Id;
      PN_Generated : Node_Id;
      Success : Boolean := False;
      pragma Warnings (Off, Success); --  XXX

      --  0 for CPN, 1 for TPN
      Formalism : constant Value_Id := New_Integer_Value (F);
   begin
      if Architecture_Instance = No_Node then
         return No_Node;
      end if;
      --  We get the root system of the AADL architecture
      Instance := Root_System (Architecture_Instance);

      --  initialization of the Petri Net top box
      PN_Generated := PN_Get_New_PN_Generated;
      PN_Init_PN_Generated (PN_Generated, Formalism);

      declare
         F : constant Value_Type :=
           Get_Value_Type (OPN.Formalism (PN_Generated));
      begin
         case F.IVal is
            when 0 =>
               --  CPN Pattern
               Set_Pn_Formalism_Specific_Informations
                 (PN_Generated,
                  PN_Get_New_CPN_Informations);

               if Instance /= No_Node and then PN_Generated /= No_Node then
                  Success :=
                    Process_Component_Instance (Instance, PN_Generated);
                  --  XXX ?
                  --                    if not Success then
                  --                       return No_Node;
                  --                  end if;
               end if;

               --  assembly and final processing
               PN_Process_Final_System
                 (PN_Generated,
                  PN_CPN_Init_Place'Access,
                  PN_CPN_Get_New_Place'Access,
                  PN_CPN_Init_Arc'Access,
                  PN_CPN_Get_New_Arc'Access,
                  PN_CPN_Duplicate_Arc'Access);
            when 1 =>
               --  TPN Pattern
               --  set specific informations
               Set_Pn_Formalism_Specific_Informations
                 (PN_Generated,
                  PN_Get_New_TPN_Informations);

               if Instance /= No_Node and then PN_Generated /= No_Node then
                  Success :=
                    Process_Component_Instance (Instance, PN_Generated);
                  --  XXX ?
                  --                    if not Success then
                  --                       return No_Node;
                  --                    end if;
               end if;

               --  assembly and final processing
               PN_Process_Final_System
                 (PN_Generated,
                  PN_TPN_Init_Place'Access,
                  PN_TPN_Get_New_Place'Access,
                  PN_TPN_Init_Arc'Access,
                  PN_TPN_Get_New_Arc'Access,
                  PN_TPN_Duplicate_Arc'Access);
            when others =>
               null;
         end case;
      end;

      return PN_Generated;
   end Process_Architecture_Instance;

   -------------------------------
   --  PN_Process_Final_System  --
   -------------------------------

   procedure PN_Process_Final_System
     (PN_Generated    : Node_Id;
      PN_P_Init_Proc  : PN_Init_Node;
      PN_P_New_Proc   : PN_New_Node;
      PN_A_Init_Proc  : PN_Init_Arc;
      PN_A_New_Proc   : PN_New_Node;
      PN_Dup_Arc_Proc : PN_Dup_Arc)

   is
      use OPN;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Namet;
      use Ocarina.Backends.Properties;
      use Ocarina.AADL_Values;
      use Ocarina.Backends.PN.Iutils;
      use Ocarina.Backends.Utils;

      use Ocarina.ME_AADL.AADL_Instances.Entities;
      use Ocarina.ME_AADL;

      Thread_Iter : Node_Id;
   begin

      Thread_Iter := OPN.First_Node (Pn_Subcomponents (Pn_Box (PN_Generated)));

      --  run across subcomponents
      while Present (Thread_Iter) loop

         if OPN.Kind (Thread_Iter) = K_Thread_Pattern then
            --  run across threads

            if OPN.Kind (Pn_Formalism_Specific_Informations (PN_Generated)) =
              K_TPN_Specific_Informations
            then

               --  manage processor
               declare
                  PN_Proc   : Node_Id;
                  Aadl_Proc : constant Node_Id :=
                    Get_Bound_Processor
                      (AIN.Corresponding_Instance
                         (Get_Container_Process (Th_Instance (Thread_Iter))));
                  Proc_Prio_Node : Node_Id;
               begin
                  if Aadl_Proc /= No_Node then

                     PN_Proc :=
                       Get_Handling (Aadl_Proc, By_Node, H_PN_Proc_Creation);

                     if PN_Proc = No_Node then

                        --  create processor
                        PN_Proc := PN_Get_New_Processor_Pattern;
                        PN_Init_Processor_Pattern (PN_Proc, Aadl_Proc);

                        --  add place in component
                        declare
                           P_Proc : Node_Id;
                        begin
                           P_Proc := PN_TPN_Get_New_Place;
                           PN_TPN_Init_Place
                             (P_Proc,
                              Aadl_Proc,
                              Get_String_Name ("_Processor"),
                              PN_Generated,
                              0);

                           Set_Tokens_Number (P_Proc, New_Integer_Value (1));
                           --  add place into component
                           Append_Node_To_List
                             (P_Proc,
                              Internal_Places (PN_Proc));
                        end;

                        Set_Handling
                          (Aadl_Proc,
                           By_Node,
                           H_PN_Proc_Creation,
                           PN_Proc);

                        --  add processor into components list
                        Append_Node_To_List
                          (PN_Proc,
                           Pn_Subcomponents (Pn_Box (PN_Generated)));
                     end if;

                     --  here we have the processor component
                     --  run across spgs to update connections

                     --  first, find dedicated entry in tpn_info to update
                     --  priorities
                     if Is_Empty
                         (Priorities
                            (Pn_Formalism_Specific_Informations
                               (PN_Generated)))
                     then
                        --  first entry in the list
                        Proc_Prio_Node := PN_Get_New_Processor_Priority;
                        PN_Init_Processor_Priority (Proc_Prio_Node, Aadl_Proc);
                        Append_Node_To_List
                          (Proc_Prio_Node,
                           Priorities
                             (Pn_Formalism_Specific_Informations
                                (PN_Generated)));
                     else
                        --  find it
                        Proc_Prio_Node :=
                          OPN.First_Node
                            (Priorities
                               (Pn_Formalism_Specific_Informations
                                  (PN_Generated)));
                        while Present (Proc_Prio_Node) loop
                           if P_Instance (Proc_Prio_Node) = Aadl_Proc then
                              --  leave loop
                              exit;
                           end if;
                           --  next
                           Proc_Prio_Node := OPN.Next_Node (Proc_Prio_Node);
                        end loop;
                     end if;
                     --  here we have proc struct
                     --  (needed to print priorities later)

                     if not Is_Empty (Call_Seq (Thread_Iter)) then
                        declare
                           Cs_Iter, Spg_Iter, Trans_Iter : Node_Id;
                           P_Proc                        : constant Node_Id :=
                             OPN.First_Node (Internal_Places (PN_Proc));
                        begin
                           Cs_Iter := OPN.First_Node (Call_Seq (Thread_Iter));
                           while Present (Cs_Iter) loop
                              if not Is_Empty (Spg_Call (Cs_Iter)) then
                                 Spg_Iter :=
                                   OPN.First_Node (Spg_Call (Cs_Iter));
                                 while Present (Spg_Iter) loop
                                    --  take processor at begin
                                    --  leave it at preemp1
                                    --  take it again at preemp2
                                    --  leave it at end

                                    Trans_Iter :=
                                      OPN.First_Node
                                        (Public_Interfaces (Spg_Iter));
                                    declare
                                       New_Arc : Node_Id;
                                    begin
                                       New_Arc := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (New_Arc,
                                          Th_Instance (Thread_Iter),
                                          P_Proc,
                                          Trans_Iter,
                                          0);

                                       Append_Node_To_List
                                         (New_Arc,
                                          Pn_Arcs_In (Trans_Iter));
                                    end;
                                    --  since it is begin,
                                    --  its priority needs to be
                                    --  recorded
                                    Set_Priority
                                      (Trans_Iter,
                                       New_Integer_Value
                                         (Get_Thread_Priority
                                            (Th_Instance (Thread_Iter))));
                                    Append_Trans_To_Processor
                                      (Trans_Iter,
                                       Proc_Prio_Node);
                                    -----
                                    Trans_Iter := OPN.Next_Node (Trans_Iter);
                                    declare
                                       New_Arc : Node_Id;
                                    begin
                                       New_Arc := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (New_Arc,
                                          Th_Instance (Thread_Iter),
                                          Trans_Iter,
                                          P_Proc,
                                          0);

                                       Append_Node_To_List
                                         (New_Arc,
                                          Pn_Arcs_Out (Trans_Iter));
                                    end;

                                    --  handle context switch
                                    Trans_Iter :=
                                      OPN.First_Node
                                        (Internal_Transitions (Spg_Iter));
                                    declare
                                       New_Arc : Node_Id;
                                    begin
                                       New_Arc := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (New_Arc,
                                          Th_Instance (Thread_Iter),
                                          Trans_Iter,
                                          P_Proc,
                                          0);

                                       Append_Node_To_List
                                         (New_Arc,
                                          Pn_Arcs_Out (Trans_Iter));
                                    end;
                                    --  since it is preemp1, its priority needs
                                    --  to be recorded
                                    Set_Priority
                                      (Trans_Iter,
                                       New_Integer_Value
                                         (Get_Thread_Priority
                                            (Th_Instance (Thread_Iter))));
                                    Append_Trans_To_Processor
                                      (Trans_Iter,
                                       Proc_Prio_Node);
                                    -----
                                    Trans_Iter := OPN.Next_Node (Trans_Iter);
                                    declare
                                       New_Arc : Node_Id;
                                    begin
                                       New_Arc := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (New_Arc,
                                          Th_Instance (Thread_Iter),
                                          P_Proc,
                                          Trans_Iter,
                                          0);

                                       Append_Node_To_List
                                         (New_Arc,
                                          Pn_Arcs_In (Trans_Iter));
                                    end;

                                    --  next
                                    Spg_Iter := OPN.Next_Node (Spg_Iter);
                                 end loop;    --  spg
                              end if;         --  has spg

                              --  next
                              Cs_Iter := OPN.Next_Node (Cs_Iter);
                           end loop;          --  cs
                        end;
                     end if;                  --  has call seq
                  end if;                        --  have processor properties
               end;
            end if;                     --  tpn for processors

            if OPN.Kind (Pn_Formalism_Specific_Informations (PN_Generated)) =
              K_TPN_Specific_Informations
            then
               --  TPN
               --  set hyperperiod
               Set_Hyperperiod
                 (Thread_Iter,
                  Hyperperiod
                    (Pn_Formalism_Specific_Informations (PN_Generated)));

               --  set hyperperiod values to hyperperiod place
               declare
                  Node_Iter : Node_Id;
               begin
                  if not Is_Empty (Internal_Places (Thread_Iter)) then
                     Node_Iter :=
                       OPN.First_Node (Internal_Places (Thread_Iter));
                     while Present (Node_Iter) loop

                        declare
                           S : constant String :=
                             Get_Name_String (Name (Identifier (Node_Iter)));
                           Token_Nb : Unsigned_Long_Long;
                           Hyper_Th : Value_Type;
                           Time_Th  : Time_Type;
                        begin
                           if S (S'Last - 10 .. S'Last) = "Hyperperiod" then
                              if Get_Thread_Dispatch_Protocol
                                  (Th_Instance (Thread_Iter)) =
                                Thread_Periodic
                                or else
                                  Get_Thread_Dispatch_Protocol
                                    (Th_Instance (Thread_Iter)) =
                                  Thread_Sporadic
                              then
                                 Time_Th :=
                                   Get_Thread_Period
                                     (Th_Instance (Thread_Iter));
                                 Hyper_Th :=
                                   Get_Value_Type (Hyperperiod (Thread_Iter));
                                 Token_Nb := Hyper_Th.IVal / Time_Th.T;

                                 Set_Tokens_Number
                                   (Node_Iter,
                                    New_Integer_Value (Token_Nb));
                              end if;
                           end if;
                        end;

                        Node_Iter := OPN.Next_Node (Node_Iter);
                     end loop;
                  end if;               --  internal places (thread)
               end;
            end if;                     --  TPN treatments (hyperperiod)

            --  COMMON PART
            --  manage call sequences
            if Get_Thread_Implementation_Kind (Th_Instance (Thread_Iter)) =
              Thread_With_Call_Sequence
            then

               --  manage port's pop / in param fusion
               declare
                  Beg_Node : Node_Id;   --  start node for CS
                  Pop_Node : Node_Id;   --  pop node for in port
                  Cs_Iter, Spg_Iter, Param_Iter : Node_Id;
               begin
                  Cs_Iter := OPN.First_Node (Call_Seq (Thread_Iter));
                  while Present (Cs_Iter) loop
                     --  run across CS
                     Spg_Iter := OPN.First_Node (Spg_Call (Cs_Iter));
                     while Present (Spg_Iter) loop
                        --  run across CS's Spg
                        Beg_Node :=
                          OPN.First_Node (Public_Interfaces (Spg_Iter));

                        if not Is_Empty (Param_In (Spg_Iter)) then
                           Param_Iter := OPN.First_Node (Param_In (Spg_Iter));
                           while Present (Param_Iter) loop
                              --  run across CS's Spg's In param

                              --  find dedicated in port
                              declare
                                 Port_Iter : Node_Id;
                              begin
                                 if not Is_Empty (In_Ports (Thread_Iter)) then
                                    Port_Iter :=
                                      OPN.First_Node (In_Ports (Thread_Iter));

                                    while Present (Port_Iter) loop
                                       --  run across in ports

                                       if not Is_Empty
                                           (Public_Interfaces (Port_Iter))
                                       then
                                          Pop_Node :=
                                            OPN.Next_Node
                                              (OPN.First_Node
                                                 (Public_Interfaces
                                                    (Port_Iter)));

                                          if Port_Instance (Port_Iter) =
                                            Par_Instance (Param_Iter)
                                          then
                                             --  match
                                             --  update arcs to
                                             --  delete port's pop
                                             declare
                                                Arc_Iter : Node_Id;
                                             begin
                                                --  look at in arcs
                                                Arc_Iter :=
                                                  OPN.First_Node
                                                    (Pn_Arcs_In (Pop_Node));
                                                while Present (Arc_Iter) loop

                                                   PN_Dup_Arc_Proc.all
                                                     (Arc_Iter,
                                                      Th_Instance
                                                        (Thread_Iter),
                                                      Beg_Node,
                                                      False);

                                                   --  next
                                                   Arc_Iter :=
                                                     OPN.Next_Node (Arc_Iter);
                                                end loop;

                                                --  look at out arcs
                                                Arc_Iter :=
                                                  OPN.First_Node
                                                    (Pn_Arcs_Out (Pop_Node));
                                                while Present (Arc_Iter) loop

                                                   PN_Dup_Arc_Proc.all
                                                     (Arc_Iter,
                                                      Th_Instance
                                                        (Thread_Iter),
                                                      Beg_Node,
                                                      True);

                                                   Arc_Iter :=
                                                     OPN.Next_Node (Arc_Iter);
                                                end loop;
                                             end;
                                             --  record where is
                                             --  pop interface now
                                             --  by_node cause
                                             --  name could fail...
                                             Set_Handling
                                               (Port_Iter,
                                                By_Node,
                                                H_PN_Port_Creation,
                                                Beg_Node);

                                             --  delete pop Node
                                             Delete_Node_From_List
                                               (Pop_Node,
                                                Public_Interfaces (Port_Iter));

                                          end if; --  match port / in param

                                       end if;
                                       --  next
                                       Port_Iter := OPN.Next_Node (Port_Iter);
                                    end loop;

                                 end if; --  have in ports

                              end;

                              --  next
                              Param_Iter := OPN.Next_Node (Param_Iter);
                           end loop;
                        end if;         --  have in param
                        --  next
                        Spg_Iter := OPN.Next_Node (Spg_Iter);
                     end loop;

                     --  next
                     Cs_Iter := OPN.Next_Node (Cs_Iter);
                  end loop;
               end;

               --  manage port's push / out param fusion
               declare
                  End_Node : Node_Id;   --  start node for CS
                  Push_Node : Node_Id;   --  push node for out port
                  Cs_Iter, Spg_Iter, Param_Iter : Node_Id;
               begin
                  Cs_Iter := OPN.First_Node (Call_Seq (Thread_Iter));
                  while Present (Cs_Iter) loop
                     --  run across CS
                     Spg_Iter := OPN.First_Node (Spg_Call (Cs_Iter));
                     while Present (Spg_Iter) loop
                        --  run across CS's Spg
                        End_Node :=
                          OPN.Next_Node
                            (OPN.First_Node (Public_Interfaces (Spg_Iter)));

                        if not Is_Empty (Param_Out (Spg_Iter)) then
                           Param_Iter := OPN.First_Node (Param_Out (Spg_Iter));
                           while Present (Param_Iter) loop
                              --  run across CS's Spg's Out param

                              --  find dedicated in port
                              declare
                                 Port_Iter : Node_Id;
                              begin
                                 if not Is_Empty (Out_Ports (Thread_Iter)) then
                                    Port_Iter :=
                                      OPN.First_Node (Out_Ports (Thread_Iter));

                                    while Present (Port_Iter) loop
                                       --  run across in ports

                                       Push_Node :=
                                         OPN.First_Node
                                           (Public_Interfaces (Port_Iter));
                                       if Push_Node /= No_Node then
                                          if Port_Instance (Port_Iter) =
                                            Par_Instance (Param_Iter)
                                          then
                                             --  match
                                             --  update arcs to
                                             --  delete port's pop
                                             declare
                                                Arc_Iter : Node_Id;
                                             begin
                                                --  look at in arcs
                                                Arc_Iter :=
                                                  OPN.First_Node
                                                    (Pn_Arcs_In (Push_Node));
                                                while Present (Arc_Iter) loop

                                                   PN_Dup_Arc_Proc.all
                                                     (Arc_Iter,
                                                      Th_Instance
                                                        (Thread_Iter),
                                                      End_Node,
                                                      False);

                                                   --  next
                                                   Arc_Iter :=
                                                     OPN.Next_Node (Arc_Iter);
                                                end loop;

                                                --  look at out arcs
                                                Arc_Iter :=
                                                  OPN.First_Node
                                                    (Pn_Arcs_Out (Push_Node));
                                                while Present (Arc_Iter) loop

                                                   PN_Dup_Arc_Proc.all
                                                     (Arc_Iter,
                                                      Th_Instance
                                                        (Thread_Iter),
                                                      End_Node,
                                                      True);

                                                   Arc_Iter :=
                                                     OPN.Next_Node (Arc_Iter);
                                                end loop;
                                             end;
                                             --  record where is
                                             --  push interface now
                                             --  by_node cause name
                                             --  could fail...
                                             if OPN.Next_Node (Spg_Iter) /=
                                               No_Node
                                             then
                                                --  push = next (begin node)
                                                --  since end will be deleted
                                                Set_Handling
                                                  (Port_Iter,
                                                   By_Node,
                                                   H_PN_Port_Creation,
                                                   OPN.First_Node
                                                     (Public_Interfaces
                                                        (OPN.Next_Node
                                                           (Spg_Iter))));
                                             else
                                                --  push = end node
                                                --  (last spg, no
                                                --  deletion)
                                                Set_Handling
                                                  (Port_Iter,
                                                   By_Node,
                                                   H_PN_Port_Creation,
                                                   End_Node);
                                             end if;

                                             --  delete push Node
                                             Delete_Node_From_List
                                               (Push_Node,
                                                Public_Interfaces (Port_Iter));

                                          end if; --  match port / out param
                                       end if;
                                       --  next
                                       Port_Iter := OPN.Next_Node (Port_Iter);
                                    end loop;

                                 end if; --  have out ports

                              end;

                              --  next
                              Param_Iter := OPN.Next_Node (Param_Iter);
                           end loop;
                        end if;         --  have out param
                        --  next
                        Spg_Iter := OPN.Next_Node (Spg_Iter);
                     end loop;

                     --  next
                     Cs_Iter := OPN.Next_Node (Cs_Iter);
                  end loop;
               end;

               --  manage CS steps fusion (end, begin (next) )
               declare
                  Cs_Iter, Spg_Iter, Spg_Next_Iter : Node_Id;
                  End_Node, Begin_Next_Node        : Node_Id;
               begin
                  Cs_Iter := OPN.First_Node (Call_Seq (Thread_Iter));
                  while Present (Cs_Iter) loop
                     --  run across CS
                     Spg_Iter := OPN.First_Node (Spg_Call (Cs_Iter));
                     while Present (Spg_Iter)
                       and then Present (OPN.Next_Node (Spg_Iter))
                     loop
                        --  run across CS's Spg
                        Spg_Next_Iter := OPN.Next_Node (Spg_Iter);
                        End_Node      :=
                          OPN.Next_Node
                            (OPN.First_Node (Public_Interfaces (Spg_Iter)));
                        Begin_Next_Node :=
                          OPN.First_Node (Public_Interfaces (Spg_Next_Iter));

                        --  update arcs to delete spg's "end_node"
                        declare
                           Arc_Iter : Node_Id;
                        begin
                           --  look at in arcs
                           Arc_Iter := OPN.First_Node (Pn_Arcs_In (End_Node));
                           while Present (Arc_Iter) loop

                              PN_Dup_Arc_Proc.all
                                (Arc_Iter,
                                 Th_Instance (Thread_Iter),
                                 Begin_Next_Node,
                                 False);

                              --  next
                              Arc_Iter := OPN.Next_Node (Arc_Iter);
                           end loop;

                           --  look at out arcs
                           Arc_Iter := OPN.First_Node (Pn_Arcs_Out (End_Node));
                           while Present (Arc_Iter) loop

                              PN_Dup_Arc_Proc.all
                                (Arc_Iter,
                                 Th_Instance (Thread_Iter),
                                 Begin_Next_Node,
                                 True);

                              --  next
                              Arc_Iter := OPN.Next_Node (Arc_Iter);
                           end loop;
                        end;
                        --  delete end_node
                        Delete_Node_From_List
                          (End_Node,
                           Public_Interfaces (Spg_Iter));

                        --  next
                        Spg_Iter := OPN.Next_Node (Spg_Iter);
                     end loop;

                     --  next
                     Cs_Iter := OPN.Next_Node (Cs_Iter);
                  end loop;
               end;

            end if;                     --  thread with CS kind

            --  here, we have compact compute box
            --  manage dispatch

            if Get_Thread_Dispatch_Protocol (Th_Instance (Thread_Iter)) =
              Thread_Aperiodic
              or else
                Get_Thread_Dispatch_Protocol (Th_Instance (Thread_Iter)) =
                Thread_Sporadic
            then

               declare
                  Wait_Node : Node_Id;
                  Port_Iter : Node_Id;
                  Pop_Node  : Node_Id;
               begin
                  --  connect wait_dispatch to each port's pop interface
                  --  since any port could trigger the dispatch
                  --  (in case of port_compute_entrypoint)
                  if not Is_Empty (In_Ports (Thread_Iter)) then
                     Port_Iter := OPN.First_Node (In_Ports (Thread_Iter));

                     Wait_Node :=
                       OPN.Next_Node
                         (First_Node (Internal_Places (Thread_Iter)));

                     while Present (Port_Iter) loop
                        --  run across in ports
                        Pop_Node :=
                          Get_Handling
                            (Port_Iter,
                             By_Node,
                             H_PN_Port_Creation);
                        if Pop_Node = No_Node then
                           --  this port have not been merged into a param
                           Pop_Node :=
                             OPN.Next_Node
                               (OPN.First_Node
                                  (Public_Interfaces (Port_Iter)));
                        end if;

                        --  here, we have the port's pop interface
                        declare
                           New_Arc : Node_Id;
                        begin
                           New_Arc := PN_A_New_Proc.all;
                           PN_A_Init_Proc
                             (New_Arc,
                              Th_Instance (Thread_Iter),
                              Wait_Node,
                              Pop_Node,
                              1);

                           Append_Node_To_List
                             (New_Arc,
                              Pn_Arcs_In (Pop_Node));
                        end;

                        --  next
                        Port_Iter := OPN.Next_Node (Port_Iter);
                     end loop;
                  end if;
               end;

            end if;                     --  aperiodic or sporadic thread

            --  manage clock for periodic or sporadic

            if Get_Thread_Dispatch_Protocol (Th_Instance (Thread_Iter)) =
              Thread_Periodic
            then
               --  manage periodic
               declare
                  Beg_Node   : Node_Id := No_Node;
                  Wait_Node  : Node_Id;
                  Clock_Node : Node_Id;
                  Node_Iter  : Node_Id;
               begin
                  --  connect wait_dispatch to first call sequence
                  --  first, find wait node
                  Wait_Node :=
                    OPN.Next_Node (First_Node (Internal_Places (Thread_Iter)));

                  --  find clock place
                  if not Is_Empty (Internal_Places (Thread_Iter)) then

                     if OPN.Kind
                         (Pn_Formalism_Specific_Informations (PN_Generated)) =
                       K_TPN_Specific_Informations
                     then

                        Node_Iter :=
                          OPN.First_Node (Internal_Places (Thread_Iter));
                        while Present (Node_Iter) loop

                           declare
                              S : constant String :=
                                Get_Name_String
                                  (Name (Identifier (Node_Iter)));
                           begin
                              if S (S'Last - 4 .. S'Last) = "Clock" then
                                 Clock_Node := Node_Iter;
                              end if;
                           end;

                           Node_Iter := OPN.Next_Node (Node_Iter);
                        end loop;

                     end if;

                     --  find begin_node
                     if not Is_Empty (Call_Seq (Thread_Iter)) then

                        Node_Iter := OPN.First_Node (Call_Seq (Thread_Iter));
                        if not Is_Empty (Spg_Call (Node_Iter)) then

                           Node_Iter := OPN.First_Node (Spg_Call (Node_Iter));
                           if not Is_Empty (Public_Interfaces (Node_Iter)) then

                              Beg_Node :=
                                OPN.First_Node (Public_Interfaces (Node_Iter));
                           end if;
                        end if;

                     end if;

                     --  add new arc for clock
                     if Present (Beg_Node)
                       and then
                         OPN.Kind
                           (Pn_Formalism_Specific_Informations
                              (PN_Generated)) =
                         K_TPN_Specific_Informations
                     then
                        declare
                           New_Arc : Node_Id;
                        begin
                           New_Arc := PN_TPN_Get_New_Arc;
                           PN_TPN_Init_Arc
                             (New_Arc,
                              Th_Instance (Thread_Iter),
                              Clock_Node,
                              Beg_Node,
                              0);
                           Append_Node_To_List
                             (New_Arc,
                              Pn_Arcs_In (Beg_Node));
                        end;
                     end if;

                     --  add new arc for wait node
                     if Present (Beg_Node) then
                        declare
                           New_Arc : Node_Id;
                        begin
                           New_Arc := PN_A_New_Proc.all;
                           PN_A_Init_Proc
                             (New_Arc,
                              Th_Instance (Thread_Iter),
                              Wait_Node,
                              Beg_Node,
                              1);

                           Append_Node_To_List
                             (New_Arc,
                              Pn_Arcs_In (Beg_Node));
                        end;
                     end if;               --  thread internal places for clock
                  end if;
               end;
            end if;                     --  periodic thread clock

            --  sporadic thread clock
            if Get_Thread_Dispatch_Protocol (Th_Instance (Thread_Iter)) =
              Thread_Sporadic
            then
               --  manage sporadic
               declare
                  Clock_Node : Node_Id;
                  Node_Iter  : Node_Id;
               begin
                  --  find clock place
                  if not Is_Empty (Internal_Places (Thread_Iter)) then
                     if OPN.Kind
                         (Pn_Formalism_Specific_Informations (PN_Generated)) =
                       K_TPN_Specific_Informations
                     then

                        Node_Iter :=
                          OPN.First_Node (Internal_Places (Thread_Iter));
                        while Present (Node_Iter) loop

                           declare
                              S : constant String :=
                                Get_Name_String
                                  (Name (Identifier (Node_Iter)));
                           begin
                              if S (S'Last - 4 .. S'Last) = "Clock" then
                                 Clock_Node := Node_Iter;
                              end if;
                           end;

                           Node_Iter := OPN.Next_Node (Node_Iter);
                        end loop;

                        declare
                           Port_Iter : Node_Id;
                           Pop_Node  : Node_Id;
                        begin
                           --  connect clock to each port's pop interface
                           --  since any port could trigger the dispatch
                           if not Is_Empty (In_Ports (Thread_Iter)) then
                              Port_Iter :=
                                OPN.First_Node (In_Ports (Thread_Iter));

                              while Present (Port_Iter) loop
                                 --  run across in ports
                                 Pop_Node :=
                                   Get_Handling
                                     (Port_Iter,
                                      By_Node,
                                      H_PN_Port_Creation);
                                 if Pop_Node = No_Node then
                                    --  this port have not been merged
                                    --  into a param
                                    Pop_Node :=
                                      OPN.Next_Node
                                        (OPN.First_Node
                                           (Public_Interfaces (Port_Iter)));
                                 end if;

                                 --  here, we have the port's pop interface
                                 declare
                                    New_Arc : Node_Id;
                                 begin
                                    New_Arc := PN_TPN_Get_New_Arc;
                                    PN_TPN_Init_Arc
                                      (New_Arc,
                                       Th_Instance (Thread_Iter),
                                       Clock_Node,
                                       Pop_Node,
                                       0);

                                    Append_Node_To_List
                                      (New_Arc,
                                       Pn_Arcs_In (Pop_Node));
                                 end;

                                 --  next
                                 Port_Iter := OPN.Next_Node (Port_Iter);
                              end loop;
                           end if;
                        end;
                     end if;
                  end if;               --  find clock place
               end;
            end if;                     --  sporadic thread clock

            --  delete _compute trans from public interfaces of thread
            --  it is now useless

            Delete_Node_From_List
              (OPN.First_Node (Public_Interfaces (Thread_Iter)),
               Public_Interfaces (Thread_Iter));

            --  now, _complete is the first node of public_interfaces

            --  manage thread interconnection adding place in pn_box
            --  here, for out ports only, and for evrey threads,
            --  we add an extra
            --  place in pn_interconnections, and connect it to pop's interface
            --  a second loop will be necessary (through in ports) to connect
            --  to in ports
            declare
               Port_Iter            : Node_Id;
               Interconnection_Node : Types.Node_Id;
               Pop_Node             : Node_Id;
            begin
               if not Is_Empty (Out_Ports (Thread_Iter)) then
                  Port_Iter := OPN.First_Node (Out_Ports (Thread_Iter));

                  while Present (Port_Iter) loop
                     --  create new node
                     Interconnection_Node := PN_P_New_Proc.all;
                     PN_P_Init_Proc
                       (Interconnection_Node,
                        Port_Instance (Port_Iter),
                        Get_String_Name ("_Bus"),
                        PN_Generated,
                        0);

                     --  set handling to the target port instance
                     --  for next loop
                     Set_Handling
                       (Interconnection_Node,
                        By_Node,
                        H_PN_Port_Creation,
                        Target_Instance (Port_Iter));

                     --  add place to pn_box
                     Append_Node_To_List
                       (Interconnection_Node,
                        Pn_Interconnections (Pn_Box (PN_Generated)));

                     --  find pop's interface
                     if Get_Handling
                         (Port_Iter,
                          By_Node,
                          H_PN_Port_Creation) /=
                       No_Node
                     then
                        --  this port have been merged into a param
                        Pop_Node :=
                          OPN.First_Node (Public_Interfaces (Port_Iter));

                        --  CPN specific
                        --  set handling to the source port push
                        --  for next loop
                        Set_Handling
                          (Interconnection_Node,
                           By_Node,
                           H_PN_Interconnection,
                           Get_Handling
                             (Port_Iter,
                              By_Node,
                              H_PN_Port_Creation));
                     --  end CPN specific
                     else
                        --  still two interfaces
                        Pop_Node :=
                          OPN.Next_Node
                            (OPN.First_Node (Public_Interfaces (Port_Iter)));
                        --  CPN specific
                        --  set handling to the source port push
                        --  for next loop
                        Set_Handling
                          (Interconnection_Node,
                           By_Node,
                           H_PN_Interconnection,
                           OPN.First_Node (Public_Interfaces (Port_Iter)));
                        --  end CPN specific
                     end if;

                     --  create arc
                     if OPN.Kind
                         (Pn_Formalism_Specific_Informations (PN_Generated)) =
                       K_TPN_Specific_Informations
                     then
                        declare
                           New_Arc : Node_Id;
                        begin
                           New_Arc := PN_A_New_Proc.all;
                           PN_A_Init_Proc
                             (New_Arc,
                              Th_Instance (Thread_Iter),
                              Pop_Node,
                              Interconnection_Node,
                              2);

                           Append_Node_To_List
                             (New_Arc,
                              Pn_Arcs_Out (Pop_Node));
                        end;
                     else
                        --  delete Pop
                        Delete_Node_From_List
                          (Pop_Node,
                           Public_Interfaces (Port_Iter));
                     end if;

                     --  next
                     Port_Iter := OPN.Next_Node (Port_Iter);
                  end loop;             --  out ports loop
               end if;                  --  have out ports
            end;                        --  run across out ports

            --  last step
            --  connect each CS "end_node" to wait_dispatch
            --  and then delete complete transition

            declare
               Complete_Node     : Node_Id;
               Wait_Node         : Node_Id;
               Cs_Iter, Spg_Iter : Node_Id;
               End_Node          : Node_Id;
            begin
               --  find wait_node
               Wait_Node :=
                 OPN.Next_Node
                   (OPN.First_Node (Internal_Places (Thread_Iter)));

               --  find complete node
               --  since we already have deleted compute_trans
               --  complete_trans is first node of public_interface
               Complete_Node :=
                 OPN.First_Node (Public_Interfaces (Thread_Iter));

               if not Is_Empty (Call_Seq (Thread_Iter)) then
                  Cs_Iter := OPN.First_Node (Call_Seq (Thread_Iter));

                  while Present (Cs_Iter) loop

                     if not Is_Empty (Spg_Call (Cs_Iter)) then
                        --  last spg call
                        Spg_Iter := OPN.Last_Node (Spg_Call (Cs_Iter));
                        --  end node is last public interface of spg call
                        if not Is_Empty (Public_Interfaces (Spg_Iter)) then
                           End_Node :=
                             OPN.Last_Node (Public_Interfaces (Spg_Iter));

                           --  make connection
                           declare
                              New_Arc : Node_Id;
                           begin
                              New_Arc := PN_A_New_Proc.all;
                              PN_A_Init_Proc
                                (New_Arc,
                                 Th_Instance (Thread_Iter),
                                 End_Node,
                                 Wait_Node,
                                 1);

                              Append_Node_To_List
                                (New_Arc,
                                 Pn_Arcs_Out (End_Node));
                           end;

                        end if;         --  public interface
                     end if;            --  spg call

                     --  next
                     Cs_Iter := OPN.Next_Node (Cs_Iter);
                  end loop;          --  for each CS
               end if;                  --  have cs

               --  delete complete_node
               Delete_Node_From_List
                 (Complete_Node,
                  Public_Interfaces (Thread_Iter));
            end;

         end if;                        --  thread subcomponent

         --  next
         Thread_Iter := OPN.Next_Node (Thread_Iter);
      end loop;                         --  next subcomponent

      -----------
      --  next loop through in ports to connect to interconnection
      --  used to merge all init_dispatch too
      Thread_Iter := OPN.First_Node (Pn_Subcomponents (Pn_Box (PN_Generated)));

      if No (Thread_Iter) then
         Display_Error
           ("Petri Net backend : " & "incomplete AADL model",
            True);
      end if;

      declare
         Init_Node : constant Node_Id :=
           OPN.First_Node (Internal_Transitions (Thread_Iter));
      begin
         --  run across subcomponents
         while Present (Thread_Iter) loop

            if OPN.Kind (Thread_Iter) = K_Thread_Pattern then

               --  merge init
               if OPN.First_Node (Internal_Transitions (Thread_Iter)) /=
                 Init_Node
               then
                  --  copy all in and out arcs
                  declare
                     Arc_Iter     : Node_Id;
                     Current_Init : constant Node_Id :=
                       OPN.First_Node (Internal_Transitions (Thread_Iter));
                  begin
                     --  look at in arcs
                     Arc_Iter := OPN.First_Node (Pn_Arcs_In (Current_Init));
                     while Present (Arc_Iter) loop

                        PN_Dup_Arc_Proc.all
                          (Arc_Iter,
                           Th_Instance (Thread_Iter),
                           Init_Node,
                           False);

                        --  next
                        Arc_Iter := OPN.Next_Node (Arc_Iter);
                     end loop;

                     --  look at out arcs
                     Arc_Iter := OPN.First_Node (Pn_Arcs_Out (Current_Init));
                     while Present (Arc_Iter) loop

                        PN_Dup_Arc_Proc.all
                          (Arc_Iter,
                           Th_Instance (Thread_Iter),
                           Init_Node,
                           True);

                        --  next
                        Arc_Iter := OPN.Next_Node (Arc_Iter);
                     end loop;

                     --  delete current init transition
                     Delete_Node_From_List
                       (Current_Init,
                        Internal_Transitions (Thread_Iter));
                  end;
               end if;                     --  update init
            end if;                     --  thread kind

            if OPN.Kind (Thread_Iter) = K_Thread_Pattern then
               --  run across threads

               declare
                  Port_Iter, Inter_Iter : Node_Id;
                  Interconnection_Node  : Node_Id := No_Node;
                  Push_Node             : Node_Id;
                  Handling_Node         : Node_Id;
               begin
                  if not Is_Empty (In_Ports (Thread_Iter)) then
                     Port_Iter := OPN.First_Node (In_Ports (Thread_Iter));

                     while Present (Port_Iter) loop
                        --  run through in ports

                        --  find interconnection place
                        if not Is_Empty
                            (Pn_Interconnections (Pn_Box (PN_Generated)))
                        then
                           Inter_Iter :=
                             OPN.First_Node
                               (Pn_Interconnections (Pn_Box (PN_Generated)));

                           while Present (Inter_Iter) loop

                              Handling_Node :=
                                Get_Handling
                                  (Inter_Iter,
                                   By_Node,
                                   H_PN_Port_Creation);

                              if Handling_Node = Port_Instance (Port_Iter) then
                                 --  match : pin port = target_port (inter)
                                 Interconnection_Node := Inter_Iter;
                                 --  exit;
                              end if;

                              --  next
                              Inter_Iter := OPN.Next_Node (Inter_Iter);
                           end loop;
                        end if;

                        --  find push node
                        if not Is_Empty (Public_Interfaces (Port_Iter)) then
                           Push_Node :=
                             OPN.First_Node (Public_Interfaces (Port_Iter));
                        end if;

                        if OPN.Kind
                            (Pn_Formalism_Specific_Informations
                               (PN_Generated)) =
                          K_TPN_Specific_Informations
                        then

                           --  create arc
                           declare
                              New_Arc : Node_Id;
                           begin
                              New_Arc := PN_A_New_Proc.all;
                              PN_A_Init_Proc
                                (New_Arc,
                                 Th_Instance (Thread_Iter),
                                 Interconnection_Node,
                                 Push_Node,
                                 3);

                              Append_Node_To_List
                                (New_Arc,
                                 Pn_Arcs_In (Push_Node));

                           end;
                        end if;

                        if OPN.Kind
                            (Pn_Formalism_Specific_Informations
                               (PN_Generated)) =
                          K_CPN_Specific_Informations
                        then
                           --  find ovf node
                           --  and connect to interconnection_node
                           declare
                              Ovf_Node : constant Node_Id :=
                                OPN.Next_Node (Push_Node);
                              --  only one next
                              --  because pop has
                              --  been deleted
                              Push_Source : Node_Id;
                           begin
                              --  get push from source port
                              --  merge it with both push and ovf
                              --  delete interconnection_node
                              Push_Source :=
                                Get_Handling
                                  (Interconnection_Node,
                                   By_Node,
                                   H_PN_Interconnection);
                              if Push_Source /= No_Node then
                                 declare
                                    Arc_Iter : Node_Id;
                                 begin
                                    --  look at in arcs
                                    Arc_Iter :=
                                      OPN.First_Node
                                        (Pn_Arcs_In (Push_Source));
                                    while Present (Arc_Iter) loop

                                       PN_Dup_Arc_Proc.all
                                         (Arc_Iter,
                                          Port_Instance (Port_Iter),
                                          Push_Node,
                                          False);

                                       --  next
                                       Arc_Iter := OPN.Next_Node (Arc_Iter);
                                    end loop;

                                    --  look at out arcs
                                    Arc_Iter :=
                                      OPN.First_Node
                                        (Pn_Arcs_Out (Push_Source));
                                    while Present (Arc_Iter) loop

                                       PN_Dup_Arc_Proc.all
                                         (Arc_Iter,
                                          Port_Instance (Port_Iter),
                                          Push_Node,
                                          True);

                                       --  next
                                       Arc_Iter := OPN.Next_Node (Arc_Iter);
                                    end loop;
                                 end;
                                 --  same for ovf
                                 if Ovf_Node /= No_Node then
                                    declare
                                       Arc_Iter : Node_Id;
                                    begin
                                       --  look at in arcs
                                       Arc_Iter :=
                                         OPN.First_Node
                                           (Pn_Arcs_In (Push_Source));
                                       while Present (Arc_Iter) loop

                                          PN_Dup_Arc_Proc.all
                                            (Arc_Iter,
                                             Port_Instance (Port_Iter),
                                             Ovf_Node,
                                             False);

                                          --  next
                                          Arc_Iter := OPN.Next_Node (Arc_Iter);
                                       end loop;

                                       --  look at out arcs
                                       Arc_Iter :=
                                         OPN.First_Node
                                           (Pn_Arcs_Out (Push_Source));
                                       while Present (Arc_Iter) loop

                                          PN_Dup_Arc_Proc.all
                                            (Arc_Iter,
                                             Port_Instance (Port_Iter),
                                             Ovf_Node,
                                             True);

                                          --  next
                                          Arc_Iter := OPN.Next_Node (Arc_Iter);
                                       end loop;
                                    end;
                                 end if;
                              end if;
                              --  delete interconnection_node
                              Delete_Node_From_List
                                (Interconnection_Node,
                                 Pn_Interconnections (Pn_Box (PN_Generated)));
                              --  delete push source
                              --  too complicated
                              Set_Handling
                                (Push_Source,
                                 By_Node,
                                 H_PN_To_Delete,
                                 Port_Iter);
                              --  impact on GMA can be negleted
                           end;
                        end if;

                        --  next
                        Port_Iter := OPN.Next_Node (Port_Iter);
                     end loop;
                  end if;                  --  have in ports
               end;

            end if;                        --  thread kind

            --  next
            Thread_Iter := OPN.Next_Node (Thread_Iter);
         end loop;                         --  next subcomponent
      end;
   end PN_Process_Final_System;

   --------------------------------
   -- Process_Component_Instance --
   --------------------------------

   function Process_Component_Instance
     (Instance     : Types.Node_Id;
      PN_Generated : Types.Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Instances.Entities;
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.ME_AADL.AADL_Instances.Nodes;
      use Ocarina.ME_AADL;
      use OPU;
      use Ocarina.Backends.Properties;

      Success   : Boolean := False;
      List_Node : Node_Id := No_Node;
   begin
      --  browse AADL instance tree
      if not AINU.Is_Empty (Subcomponents (Instance)) then

         List_Node := AIN.First_Node (Subcomponents (Instance));

         while List_Node /= No_Node loop
            case Get_Category_Of_Component (Corresponding_Instance (List_Node))
            is
               when CC_Process | CC_System =>

                  Success :=
                    Process_Component_Instance
                      (Corresponding_Instance (List_Node),
                       PN_Generated)
                    and then Success;

               when CC_Thread =>

                  Success :=
                    Process_Thread_Instance
                      (Corresponding_Instance (List_Node),
                       PN_Generated)

                    and then Success;
               when others =>
                  null;
            end case;

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Process_Component_Instance;

   -----------------------------
   -- Process_Thread_Instance --
   -----------------------------

   function Process_Thread_Instance
     (Aadl_Instance : Types.Node_Id;
      PN_Generated  : Types.Node_Id) return Boolean
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Iutils;
      use Ocarina.AADL_Values;

      Success : constant Boolean := True;

      PN_Thread : Node_Id;
   begin
      --  Get a new PN thread pattern box
      PN_Thread := PN_Get_New_Thread_Pattern;
      PN_Init_Thread_Pattern (PN_Thread, Aadl_Instance);

      --  Depending on formalism, compute thread pattern
      declare
         F : constant Value_Type := Get_Value_Type (Formalism (PN_Generated));
      begin
         case F.IVal is
            when 0 =>
               --  CPN Pattern
               PN_Process_Thread_Pattern
                 (Aadl_Instance,
                  PN_Thread,
                  PN_Generated,
                  PN_CPN_Init_Place'Access,
                  PN_CPN_Get_New_Place'Access,
                  PN_CPN_Init_Transition'Access,
                  PN_CPN_Get_New_Transition'Access,
                  PN_CPN_Init_Arc'Access,
                  PN_CPN_Get_New_Arc'Access);
            when 1 =>
               --  TPN Pattern
               PN_Process_Thread_Pattern
                 (Aadl_Instance,
                  PN_Thread,
                  PN_Generated,
                  PN_TPN_Init_Place'Access,
                  PN_TPN_Get_New_Place'Access,
                  PN_TPN_Init_Transition'Access,
                  PN_TPN_Get_New_Transition'Access,
                  PN_TPN_Init_Arc'Access,
                  PN_TPN_Get_New_Arc'Access);
            when others =>
               null;
         end case;
      end;

      --  create the standard thread pattern
      OPU.Append_Node_To_List
        (PN_Thread,
         Pn_Subcomponents (Pn_Box (PN_Generated)));

      return Success;
   end Process_Thread_Instance;

   -------------------------------------
   --  PN_Process_TPN_Thread_Pattern  --
   -------------------------------------

   procedure PN_Process_Thread_Pattern
     (Aadl_Instance  : Types.Node_Id;
      PN_Thread      : Types.Node_Id;
      PN_Generated   : Types.Node_Id;
      PN_P_Init_Proc : PN_Init_Node;
      PN_P_New_Proc  : PN_New_Node;
      PN_T_Init_Proc : PN_Init_Node;
      PN_T_New_Proc  : PN_New_Node;
      PN_A_Init_Proc : PN_Init_Arc;
      PN_A_New_Proc  : PN_New_Node)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Iutils;
      use Ocarina.Backends.PN.Utils;
      use OAV;
      use Ocarina.Backends.Properties;
      use Ocarina.Namet;
      use AIN;
      use OPND;

      V_Formalism : constant Value_Type :=
        Get_Value_Type (Formalism (PN_Generated));
   begin

      if Get_Thread_Implementation_Kind (Aadl_Instance) = Thread_Unknown then

         Display_Error
           ("Petri Net backend : " & "Thread Implementation Unknown",
            True);
      end if;

      --  build skeleton pattern
      PN_Thread_Skeleton
        (Aadl_Instance,
         PN_Thread,
         PN_Generated,
         PN_P_Init_Proc,
         PN_P_New_Proc,
         PN_T_Init_Proc,
         PN_T_New_Proc,
         PN_A_Init_Proc,
         PN_A_New_Proc);

      -------------------------
      --  build compute pattern

      if Get_Thread_Implementation_Kind (Aadl_Instance) =
        Thread_With_Call_Sequence
      then

         --  browse call sequences and store them
         declare
            Cs    : List_Id;
            PN_Cs : Node_Id := No_Node;
         begin
            --  build pn node for call sequence
            PN_Cs := PN_Get_New_Call_Seq_Pattern;
            PN_Init_Call_Seq_Pattern (PN_Cs, Aadl_Instance);

            if not AINU.Is_Empty (AIN.Calls (Aadl_Instance)) then
               --  get calls sequences for the instance thread
               Cs := AIN.Calls (Aadl_Instance);

               declare
                  Call_Iter   : Node_Id := AIN.First_Node (Cs);
                  Spg_call    : Node_Id;
                  PN_Spg_Call : Node_Id := No_Node;
               begin

                  while Present (Call_Iter) loop

                     if not AINU.Is_Empty
                         (AIN.Subprogram_Calls (Call_Iter))
                     then
                        --  get first subprogram call of the sequence
                        Spg_call :=
                          AIN.First_Node (AIN.Subprogram_Calls (Call_Iter));

                        if Present (Spg_call) then
                           --  build Pn node for spg
                           PN_Spg_Call := PN_Get_New_Spg_Pattern;
                           PN_Init_Spg_Pattern (PN_Spg_Call, Aadl_Instance);
                           --  Parent_Sequence (Spg_Call));

                        end if;
                        while Present (Spg_call) loop

                           declare
                              --  get corresponding subprogram aadl instance
                              Spg : constant Node_Id :=
                                AIN.Corresponding_Instance (Spg_call);
                           begin
                              --  check if subprogram instance has features
                              if not AINU.Is_Empty (AIN.Features (Spg)) then

                                 declare
                                    Spg_Feat : Node_Id :=
                                      AIN.First_Node (AIN.Features (Spg));
                                 begin
                                    while Present (Spg_Feat) loop
                                       if AIN.Kind (Spg_Feat) =
                                         AIN.K_Parameter_Instance
                                         and then AIN.Is_Out (Spg_Feat)
                                       then

                                          --  subprogram instance
                                          --  has out parameter
                                          if AINU.Length
                                              (AIN.Destinations (Spg_Feat)) =
                                            0
                                          then
                                             Display_Located_Error
                                               (AIN.Loc (Spg_Feat),
                                                "This OUT parameter " &
                                                "is not connected to" &
                                                " any destination",
                                                Fatal => True);
                                          elsif AINU.Length
                                              (AIN.Destinations (Spg_Feat)) >
                                            1
                                          then
                                             Display_Located_Error
                                               (AIN.Loc (Spg_Feat),
                                                "This OUT parameter " &
                                                "has too many destination",
                                                Fatal => True);
                                          end if;

                                          --  Here we have an OUT parameter
                                          --  with exactly one
                                          --  destination.

                                          --  build pn node for out parameter

                                          PN_Build_Spg_Par
                                            (Aadl_Instance,
                                             PN_Generated,
                                             Spg_Feat,
                                             PN_Spg_Call,
                                             False,
                                             PN_P_Init_Proc,
                                             PN_P_New_Proc,
                                             PN_T_Init_Proc,
                                             PN_T_New_Proc,
                                             PN_A_Init_Proc,
                                             PN_A_New_Proc);

                                       elsif AIN.Kind (Spg_Feat) =
                                         AIN.K_Parameter_Instance
                                         and then AIN.Is_In (Spg_Feat)
                                       then
                                          --  subprogram instance has
                                          --  in parameter

                                          --  subprogram instance
                                          --  has in parameter

                                          if AINU.Length
                                              (AIN.Sources (Spg_Feat)) =
                                            0
                                          then
                                             Display_Located_Error
                                               (AIN.Loc (Spg_Feat),
                                                "This IN parameter " &
                                                "is not connected to" &
                                                " any source",
                                                Fatal => True);
                                          elsif AINU.Length
                                              (AIN.Sources (Spg_Feat)) >
                                            1
                                          then
                                             Display_Located_Error
                                               (AIN.Loc (Spg_Feat),
                                                "This IN parameter " &
                                                "has too many sources",
                                                Fatal => True);
                                          end if;

                                          --  Here we have an IN parameter
                                          --  with exactly one
                                          --  destination.
                                          PN_Build_Spg_Par
                                            (Aadl_Instance,
                                             PN_Generated,
                                             Spg_Feat,
                                             PN_Spg_Call,
                                             True,
                                             PN_P_Init_Proc,
                                             PN_P_New_Proc,
                                             PN_T_Init_Proc,
                                             PN_T_New_Proc,
                                             PN_A_Init_Proc,
                                             PN_A_New_Proc);
                                       end if;

                                       --  next feature of current
                                       --  subprogram instance
                                       Spg_Feat := AIN.Next_Node (Spg_Feat);
                                    end loop;

                                    --  add places and transitions
                                    --  for subprogram pattern :
                                    --  transition begin
                                    --  place compute
                                    --  transition end

                                    declare
                                       P_PN_Spg_Comp    : Node_Id;
                                       T1_PN_Spg_Beg    : Node_Id;
                                       T2_PN_Spg_End    : Node_Id;
                                       P_W1, P_W2       : Node_Id;
                                       Preemp1, Preemp2 : Node_Id;
                                       A_PN_Spg_Call    : Node_Id;
                                    begin

                                       T1_PN_Spg_Beg := PN_T_New_Proc.all;
                                       PN_T_Init_Proc
                                         (T1_PN_Spg_Beg,
                                          Aadl_Instance,
                                          Get_String_Name ("_Begin"),
                                          PN_Generated,
                                          0);

                                       Append_Node_To_List
                                         (T1_PN_Spg_Beg,
                                          Public_Interfaces (PN_Spg_Call));

                                       T2_PN_Spg_End := PN_T_New_Proc.all;
                                       PN_T_Init_Proc
                                         (T2_PN_Spg_End,
                                          Aadl_Instance,
                                          Get_String_Name ("_End"),
                                          PN_Generated,
                                          0);

                                       Append_Node_To_List
                                         (T2_PN_Spg_End,
                                          Public_Interfaces (PN_Spg_Call));
                                       --
                                       Preemp1 := PN_T_New_Proc.all;
                                       PN_T_Init_Proc
                                         (Preemp1,
                                          Aadl_Instance,
                                          Get_String_Name ("_Preemp1"),
                                          PN_Generated,
                                          0);

                                       Append_Node_To_List
                                         (Preemp1,
                                          Internal_Transitions (PN_Spg_Call));

                                       Preemp2 := PN_T_New_Proc.all;
                                       PN_T_Init_Proc
                                         (Preemp2,
                                          Aadl_Instance,
                                          Get_String_Name ("_Preemp2"),
                                          PN_Generated,
                                          0);

                                       Append_Node_To_List
                                         (Preemp2,
                                          Internal_Transitions (PN_Spg_Call));
                                       --
                                       P_W1 := PN_P_New_Proc.all;
                                       PN_P_Init_Proc
                                         (P_W1,
                                          Aadl_Instance,
                                          Get_String_Name ("_Work1"),
                                          PN_Generated,
                                          0);
                                       Append_Node_To_List
                                         (P_W1,
                                          Internal_Places (PN_Spg_Call));

                                       P_W2 := PN_P_New_Proc.all;
                                       PN_P_Init_Proc
                                         (P_W2,
                                          Aadl_Instance,
                                          Get_String_Name ("_Work2"),
                                          PN_Generated,
                                          0);
                                       Append_Node_To_List
                                         (P_W2,
                                          Internal_Places (PN_Spg_Call));

                                       P_PN_Spg_Comp := PN_P_New_Proc.all;
                                       PN_P_Init_Proc
                                         (P_PN_Spg_Comp,
                                          Aadl_Instance,
                                          Get_String_Name ("_ContextSwitch"),
                                          PN_Generated,
                                          0);
                                       Append_Node_To_List
                                         (P_PN_Spg_Comp,
                                          Internal_Places (PN_Spg_Call));
                                       --
                                       --  now, build arcs
                                       --  beg -> W1 -> Preemp1 -> Context ->
                                       --  Preemp2 -> W2 -> End
                                       A_PN_Spg_Call := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (A_PN_Spg_Call,
                                          Aadl_Instance,
                                          T1_PN_Spg_Beg,
                                          P_W1,
                                          1);
                                       Append_Node_To_List
                                         (A_PN_Spg_Call,
                                          Pn_Arcs_Out (T1_PN_Spg_Beg));

                                       A_PN_Spg_Call := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (A_PN_Spg_Call,
                                          Aadl_Instance,
                                          P_W1,
                                          Preemp1,
                                          1);
                                       Append_Node_To_List
                                         (A_PN_Spg_Call,
                                          Pn_Arcs_In (Preemp1));

                                       A_PN_Spg_Call := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (A_PN_Spg_Call,
                                          Aadl_Instance,
                                          Preemp1,
                                          P_PN_Spg_Comp,
                                          1);
                                       Append_Node_To_List
                                         (A_PN_Spg_Call,
                                          Pn_Arcs_Out (Preemp1));

                                       A_PN_Spg_Call := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (A_PN_Spg_Call,
                                          Aadl_Instance,
                                          P_PN_Spg_Comp,
                                          Preemp2,
                                          1);
                                       Append_Node_To_List
                                         (A_PN_Spg_Call,
                                          Pn_Arcs_In (Preemp2));

                                       A_PN_Spg_Call := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (A_PN_Spg_Call,
                                          Aadl_Instance,
                                          Preemp2,
                                          P_W2,
                                          1);
                                       Append_Node_To_List
                                         (A_PN_Spg_Call,
                                          Pn_Arcs_Out (Preemp2));

                                       A_PN_Spg_Call := PN_A_New_Proc.all;
                                       PN_A_Init_Proc
                                         (A_PN_Spg_Call,
                                          Aadl_Instance,
                                          P_W2,
                                          T2_PN_Spg_End,
                                          1);
                                       Append_Node_To_List
                                         (A_PN_Spg_Call,
                                          Pn_Arcs_In (T2_PN_Spg_End));
                                    end;

                                    --  add spg_call to call_sequence
                                    Append_Node_To_List
                                      (PN_Spg_Call,
                                       OPN.Spg_Call (PN_Cs));
                                    -------------

                                 end;

                              end if;
                           end;

                           Spg_call := AIN.Next_Node (Spg_call);
                        end loop;

                     end if;

                     Call_Iter := AIN.Next_Node (Call_Iter);
                  end loop;

                  --------------

               end;
            end if;

            --  add call_sequence to thread_pattern
            Append_Node_To_List (PN_Cs, Call_Seq (PN_Thread));

         end;

      else
         --  thread with compute entrypoint
         --  OR
         --  thread with port compute entrypoint

         --  for both T_CE or T_PCE,
         --  build a single call sequence
         --  for T_PCE : all ports will be plugged on this call sequence
         --  for T_CE  : merge dispatch with the "begin" of this sequence

         declare
            PN_Cs : Node_Id := No_Node;
         begin
            --  build pn node for call sequence
            PN_Cs := PN_Get_New_Call_Seq_Pattern;
            PN_Init_Call_Seq_Pattern (PN_Cs, Aadl_Instance);

            declare
               P_PN_Spg_Comp    : Node_Id;
               T1_PN_Spg_Beg    : Node_Id;
               T2_PN_Spg_End    : Node_Id;
               P_W1, P_W2       : Node_Id;
               Preemp1, Preemp2 : Node_Id;
               A_PN_Spg_Call    : Node_Id;

               PN_Spg_Call : Node_Id := No_Node;
            begin
               PN_Spg_Call := PN_Get_New_Spg_Pattern;
               PN_Init_Spg_Pattern (PN_Spg_Call, Aadl_Instance);

               T1_PN_Spg_Beg := PN_T_New_Proc.all;
               PN_T_Init_Proc
                 (T1_PN_Spg_Beg,
                  Aadl_Instance,
                  Get_String_Name ("_Begin"),
                  PN_Generated,
                  0);

               Append_Node_To_List
                 (T1_PN_Spg_Beg,
                  Public_Interfaces (PN_Spg_Call));

               T2_PN_Spg_End := PN_T_New_Proc.all;
               PN_T_Init_Proc
                 (T2_PN_Spg_End,
                  Aadl_Instance,
                  Get_String_Name ("_End"),
                  PN_Generated,
                  0);

               Append_Node_To_List
                 (T2_PN_Spg_End,
                  Public_Interfaces (PN_Spg_Call));
               --
               Preemp1 := PN_T_New_Proc.all;
               PN_T_Init_Proc
                 (Preemp1,
                  Aadl_Instance,
                  Get_String_Name ("_Preemp1"),
                  PN_Generated,
                  0);

               Append_Node_To_List
                 (Preemp1,
                  Internal_Transitions (PN_Spg_Call));

               Preemp2 := PN_T_New_Proc.all;
               PN_T_Init_Proc
                 (Preemp2,
                  Aadl_Instance,
                  Get_String_Name ("_Preemp2"),
                  PN_Generated,
                  0);

               Append_Node_To_List
                 (Preemp2,
                  Internal_Transitions (PN_Spg_Call));
               --
               P_W1 := PN_P_New_Proc.all;
               PN_P_Init_Proc
                 (P_W1,
                  Aadl_Instance,
                  Get_String_Name ("_Work1"),
                  PN_Generated,
                  0);
               Append_Node_To_List (P_W1, Internal_Places (PN_Spg_Call));

               P_W2 := PN_P_New_Proc.all;
               PN_P_Init_Proc
                 (P_W2,
                  Aadl_Instance,
                  Get_String_Name ("_Work2"),
                  PN_Generated,
                  0);
               Append_Node_To_List (P_W2, Internal_Places (PN_Spg_Call));

               P_PN_Spg_Comp := PN_P_New_Proc.all;
               PN_P_Init_Proc
                 (P_PN_Spg_Comp,
                  Aadl_Instance,
                  Get_String_Name ("_ContextSwitch"),
                  PN_Generated,
                  0);
               Append_Node_To_List
                 (P_PN_Spg_Comp,
                  Internal_Places (PN_Spg_Call));
               --
               --  now, build arcs
               --  beg -> W1 -> Preemp1 -> Context ->
               --  Preemp2 -> W2 -> End
               A_PN_Spg_Call := PN_A_New_Proc.all;
               PN_A_Init_Proc
                 (A_PN_Spg_Call,
                  Aadl_Instance,
                  T1_PN_Spg_Beg,
                  P_W1,
                  1);
               Append_Node_To_List
                 (A_PN_Spg_Call,
                  Pn_Arcs_Out (T1_PN_Spg_Beg));

               A_PN_Spg_Call := PN_A_New_Proc.all;
               PN_A_Init_Proc (A_PN_Spg_Call, Aadl_Instance, P_W1, Preemp1, 1);
               Append_Node_To_List (A_PN_Spg_Call, Pn_Arcs_In (Preemp1));

               A_PN_Spg_Call := PN_A_New_Proc.all;
               PN_A_Init_Proc
                 (A_PN_Spg_Call,
                  Aadl_Instance,
                  Preemp1,
                  P_PN_Spg_Comp,
                  1);
               Append_Node_To_List (A_PN_Spg_Call, Pn_Arcs_Out (Preemp1));

               A_PN_Spg_Call := PN_A_New_Proc.all;
               PN_A_Init_Proc
                 (A_PN_Spg_Call,
                  Aadl_Instance,
                  P_PN_Spg_Comp,
                  Preemp2,
                  1);
               Append_Node_To_List (A_PN_Spg_Call, Pn_Arcs_In (Preemp2));

               A_PN_Spg_Call := PN_A_New_Proc.all;
               PN_A_Init_Proc (A_PN_Spg_Call, Aadl_Instance, Preemp2, P_W2, 1);
               Append_Node_To_List (A_PN_Spg_Call, Pn_Arcs_Out (Preemp2));

               A_PN_Spg_Call := PN_A_New_Proc.all;
               PN_A_Init_Proc
                 (A_PN_Spg_Call,
                  Aadl_Instance,
                  P_W2,
                  T2_PN_Spg_End,
                  1);
               Append_Node_To_List (A_PN_Spg_Call, Pn_Arcs_In (T2_PN_Spg_End));

               --  add spg_call to call_sequence
               Append_Node_To_List (PN_Spg_Call, OPN.Spg_Call (PN_Cs));
            end;

            --  add call_sequence to thread_pattern
            Append_Node_To_List (PN_Cs, Call_Seq (PN_Thread));

         end;
      end if;
      --------------------------
      --  build dispatch pattern
      --  only for TPN (no clock in CPN)

      if V_Formalism.IVal = 1 then
         --  if periodic or sporadic, add trigger pattern
         case Get_Thread_Dispatch_Protocol (Aadl_Instance) is
            when Thread_Periodic | Thread_Sporadic =>
               declare
                  type Clock_Pattern is array (Integer range <>) of Name_Id;
                  Clock_P_Pattern : constant Clock_Pattern :=
                    (Get_String_Name ("_Hyperperiod"),
                     Get_String_Name ("_Clock"));
                  Clock_T_Pattern : constant Clock_Pattern :=
                    (1 => Get_String_Name ("_Period_Event"));
                  P, T, A        : Types.Node_Id;
                  PN_Period      : Time_Type;
                  PN_Hyperperiod : Value_Type;
                  B              : Boolean := True;
               begin
                  for TN in Clock_T_Pattern'First .. Clock_T_Pattern'Last loop
                     T := PN_TPN_Get_New_Transition;
                     PN_TPN_Init_Transition
                       (T,
                        Aadl_Instance,
                        Clock_T_Pattern (TN),
                        PN_Generated,
                        0);
                     --  set guard
                     PN_Period := Get_Thread_Period (Aadl_Instance);
                     PN_TPN_Set_Guard
                       (T,
                        New_Integer_Value (PN_Period.T),
                        New_Integer_Value (PN_Period.T),
                        New_Integer_Value (0),        --  braces mode
                        New_Integer_Value (0));       --  priority

                     --  add new place transition thread box
                     Append_Node_To_List (T, Internal_Transitions (PN_Thread));
                  end loop;
                  for PN in Clock_P_Pattern'First .. Clock_P_Pattern'Last loop
                     P := PN_TPN_Get_New_Place;
                     PN_TPN_Init_Place
                       (P,
                        Aadl_Instance,
                        Clock_P_Pattern (PN),
                        PN_Generated,
                        0);
                     --  add new place into thread box
                     Append_Node_To_List (P, Internal_Places (PN_Thread));

                     --  update arcs for period_event transition
                     A := PN_TPN_Get_New_Arc;
                     if B then
                        PN_TPN_Init_Arc (A, Aadl_Instance, P, T, 0);
                        Append_Node_To_List (A, Pn_Arcs_In (T));
                        B := False;
                     else
                        PN_TPN_Init_Arc (A, Aadl_Instance, T, P, 0);
                        Append_Node_To_List (A, Pn_Arcs_Out (T));
                     end if;

                  end loop;

                  --  update hyperperiod value
                  PN_Hyperperiod :=
                    Get_Value_Type
                      (Hyperperiod
                         (Pn_Formalism_Specific_Informations (PN_Generated)));

                  PN_Hyperperiod :=
                    Get_Value_Type
                      (New_Integer_Value
                         (Ppcm (PN_Hyperperiod.IVal, PN_Period.T)));

                  Set_Hyperperiod
                    (Pn_Formalism_Specific_Informations (PN_Generated),
                     New_Integer_Value (PN_Hyperperiod.IVal));

               end;
            when Thread_Aperiodic =>
               --  here will hold dispatch ports connections
               --  since in TPN we deal with  quantitative analysis
               --  ports patterns stand to a simple place
               null;
            when others =>
               null;
         end case;
      end if;                           --  TPN for clock / hyperperiod

      -----------------------
      --  set initial marking

      --  set place initial marking
      declare
         P : Types.Node_Id;
      begin
         P := OPN.First_Node (Internal_Places (PN_Thread));
         --  Halted place
         if V_Formalism.IVal = 1 then
            Set_Tokens_Number (P, New_Integer_Value (1));
         else
            declare
               Marking_Node : constant Node_Id :=
                 New_Node (K_CPN_Marking_Token);
               Node_Iter : Node_Id;
               Count     : constant Value_Type :=
                 Get_Value_Type
                   (Threads_Count
                      (Pn_Formalism_Specific_Informations (PN_Generated)));
            begin
               if Count.IVal /= 0 then
                  Node_Iter :=
                    OPN.First_Node
                      (Threads_Ids
                         (Pn_Formalism_Specific_Informations (PN_Generated)));

                  while Present (Node_Iter) loop
                     if OPN.Aadl_Instance (Node_Iter) =
                       Th_Instance (PN_Thread)
                     then
                        exit;
                     end if;
                     --  next
                     Node_Iter := OPN.Next_Node (Node_Iter);
                  end loop;

                  if Node_Iter /= No_Node then
                     --  found
                     declare
                        Mark : constant Value_Type :=
                          Get_Value_Type (Pn_Id (Node_Iter));
                     begin
                        PN_Init_PN_Node
                          (Marking_Node,
                           No_Node,
                           Get_String_Name
                             (OAV.Image (New_Integer_Value (Mark.IVal))));
                        Append_Node_To_List
                          (Marking_Node,
                           Tokens (Marking (P)));
                     end;
                  else
                     declare
                        Th_Id_Node : constant Node_Id :=
                          New_Node (K_CPN_Aadl_Id);
                     begin
                        PN_Init_PN_Node
                          (Th_Id_Node,
                           No_Node,
                           Get_String_Name ("::"));
                        Set_Aadl_Instance
                          (Th_Id_Node,
                           Th_Instance (PN_Thread));
                        Set_Pn_Id
                          (Th_Id_Node,
                           New_Integer_Value (Count.IVal + 1));

                        Append_Node_To_List
                          (Th_Id_Node,
                           Threads_Ids
                             (Pn_Formalism_Specific_Informations
                                (PN_Generated)));

                        PN_Init_PN_Node
                          (Marking_Node,
                           No_Node,
                           Get_String_Name
                             (OAV.Image (New_Integer_Value (Count.IVal + 1))));

                        Append_Node_To_List
                          (Marking_Node,
                           Tokens (Marking (P)));

                        Set_Threads_Count
                          (Pn_Formalism_Specific_Informations (PN_Generated),
                           New_Integer_Value (Count.IVal + 1));
                     end;
                  end if;
               else
                  declare
                     Th_Id_Node : constant Node_Id := New_Node (K_CPN_Aadl_Id);
                  begin
                     PN_Init_PN_Node
                       (Th_Id_Node,
                        No_Node,
                        Get_String_Name ("::"));
                     Set_Aadl_Instance (Th_Id_Node, Th_Instance (PN_Thread));
                     Set_Pn_Id (Th_Id_Node, New_Integer_Value (1));

                     Append_Node_To_List
                       (Th_Id_Node,
                        Threads_Ids
                          (Pn_Formalism_Specific_Informations (PN_Generated)));

                     PN_Init_PN_Node
                       (Marking_Node,
                        No_Node,
                        Get_String_Name ("1"));
                     Append_Node_To_List (Marking_Node, Tokens (Marking (P)));

                     Set_Threads_Count
                       (Pn_Formalism_Specific_Informations (PN_Generated),
                        New_Integer_Value (1));
                  end;
               end if;

            end;
         end if;
      end;

      --  add dedicated variable in variable list
      if V_Formalism.IVal = 0 then
         declare
            Ded_Var : constant Node_Id :=
              New_Node (K_CPN_Formalism_Variable_Item);
         begin
            PN_Init_PN_Node
              (Ded_Var,
               No_Node,
               Get_String_Name ("x" & OPND.Image (Aadl_Instance)));
            Append_Node_To_List
              (Ded_Var,
               Variable_List
                 (OPN.First_Node
                    (Variables
                       (Pn_Formalism_Specific_Informations (PN_Generated)))));
         end;
      end if;
      --  process ports

      PN_Build_Port
        (Aadl_Instance,
         PN_Generated,
         PN_Thread,
         Formalism (PN_Generated));

   end PN_Process_Thread_Pattern;

   ------------------------
   --  PN_Build_Spg_Par  --
   ------------------------

   procedure PN_Build_Spg_Par
     (Aadl_Instance  : Types.Node_Id;
      PN_Generated   : Types.Node_Id;
      Spg_Feat       : Types.Node_Id;
      PN_Spg_Call    : Types.Node_Id;
      In_Par         : Boolean := True;
      PN_P_Init_Proc : PN_Init_Node;
      PN_P_New_Proc  : PN_New_Node;
      PN_T_Init_Proc : PN_Init_Node;
      PN_T_New_Proc  : PN_New_Node;
      PN_A_Init_Proc : PN_Init_Arc;
      PN_A_New_Proc  : PN_New_Node)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Iutils;
      use Ocarina.AADL_Values;
      use Ocarina.Backends.Properties;
      use Ocarina.Namet;
      use AIN;

      PN_Spg_Par    : Node_Id;
      Dest_F        : Node_Id;
      P_PN_Spg_Par  : Node_Id;
      T1_PN_Spg_Par : Node_Id;
      T2_PN_Spg_Par : Node_Id;
      A_PN_Spg_Par  : Node_Id;

      S : String := "In ";
   begin
      if not In_Par then
         S := "Out";
      end if;

      PN_Spg_Par := PN_Get_New_Spg_Par_Pattern;
      PN_Init_Spg_Par_Pattern (PN_Spg_Par, Spg_Feat);

      if not In_Par then
         Dest_F := AIN.Item (AIN.First_Node (AIN.Destinations (Spg_Feat)));
      else
         Dest_F := AIN.Item (AIN.First_Node (AIN.Sources (Spg_Feat)));
      end if;
      --  store aadl instance destination
      --  (thread, spg)

      Set_Par_Instance (PN_Spg_Par, Dest_F);

      --  make pn pattern for spg parameter
      --  one public transition in param,
      --  one public transition out param
      --  one local place param value
      --  it will be reduced at
      --  assembly phase

      T1_PN_Spg_Par := PN_T_New_Proc.all;
      PN_T_Init_Proc
        (T1_PN_Spg_Par,
         Dest_F,
         Get_String_Name ("_Push_P_" & S),
         PN_Generated,
         0);

      Append_Node_To_List (T1_PN_Spg_Par, Public_Interfaces (PN_Spg_Par));

      T2_PN_Spg_Par := PN_T_New_Proc.all;
      PN_T_Init_Proc
        (T2_PN_Spg_Par,
         Dest_F,
         Get_String_Name ("_Pop_P_" & S),
         PN_Generated,
         0);

      Append_Node_To_List (T2_PN_Spg_Par, Public_Interfaces (PN_Spg_Par));

      P_PN_Spg_Par := PN_P_New_Proc.all;
      PN_P_Init_Proc
        (P_PN_Spg_Par,
         Dest_F,
         Get_String_Name ("_Val_P_" & S),
         PN_Generated,
         0);
      Append_Node_To_List (P_PN_Spg_Par, Internal_Places (PN_Spg_Par));

      A_PN_Spg_Par := PN_A_New_Proc.all;
      PN_A_Init_Proc
        (A_PN_Spg_Par,
         Aadl_Instance,
         P_PN_Spg_Par,
         T2_PN_Spg_Par,
         2);
      Append_Node_To_List (A_PN_Spg_Par, Pn_Arcs_In (T2_PN_Spg_Par));

      A_PN_Spg_Par := PN_A_New_Proc.all;
      PN_A_Init_Proc
        (A_PN_Spg_Par,
         Aadl_Instance,
         T1_PN_Spg_Par,
         P_PN_Spg_Par,
         2);
      Append_Node_To_List (A_PN_Spg_Par, Pn_Arcs_Out (T1_PN_Spg_Par));

      --------------

      --  add pn_spg_par to spg_call
      if In_Par then
         Append_Node_To_List (PN_Spg_Par, Param_In (PN_Spg_Call));
      else
         Append_Node_To_List (PN_Spg_Par, Param_Out (PN_Spg_Call));
      end if;
      ---------------

   end PN_Build_Spg_Par;

   ---------------------
   --  PN_Build_Port  --
   ---------------------

   procedure PN_Build_Port
     (Aadl_Instance : Types.Node_Id;
      PN_Generated  : Types.Node_Id;
      PN_Thread     : Types.Node_Id;
      F             : Types.Value_Id)
   is

      use Ocarina.Namet;
      use AIN;
      use Ocarina.AADL_Values;
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.ME_AADL.AADL_Instances.Entities;
      use Ocarina.Backends.Properties;
      use Ocarina.Backends.PN.Iutils;

      Port_Iter : Node_Id;
      PN_Port   : Node_Id := No_Node;

      V_Formalism : constant Value_Type := Get_Value_Type (F);
   begin

      if not AINU.Is_Empty (Features (Aadl_Instance)) then
         Port_Iter := AIN.First_Node (Features (Aadl_Instance));

         while Port_Iter /= No_Node loop
            if Kind (Port_Iter) = K_Port_Spec_Instance then

               if Is_Event (Port_Iter) then
                  --  event port
                  PN_Port := PN_Get_New_ED_Port_Pattern;
                  PN_Init_ED_Port_Pattern (PN_Port, Port_Iter);

                  if Is_In (Port_Iter)
                    and then Get_Port_Compute_Entrypoint (Port_Iter) /= No_Node
                  then
                     Set_Has_CEP (PN_Port, True);
                  end if;

                  --  places and transition for port pattern
                  if V_Formalism.IVal = 1 then
                     --  TPN
                     PN_Build_Tpn_Port
                       (Aadl_Instance,
                        PN_Generated,
                        Port_Iter,
                        PN_Port,
                        False);
                  else
                     --  CPN
                     PN_Build_Cpn_Port
                       (Aadl_Instance,
                        PN_Generated,
                        Port_Iter,
                        PN_Port,
                        False);
                  end if;
               else
                  --  data port
                  PN_Port := PN_Get_New_D_Port_Pattern;
                  PN_Init_D_Port_Pattern (PN_Port, Port_Iter);

                  --  places and transition for port pattern
                  if V_Formalism.IVal = 1 then
                     --  TPN
                     PN_Build_Tpn_Port
                       (Aadl_Instance,
                        PN_Generated,
                        Port_Iter,
                        PN_Port,
                        True);
                  else
                     --  CPN
                     PN_Build_Cpn_Port
                       (Aadl_Instance,
                        PN_Generated,
                        Port_Iter,
                        PN_Port,
                        True);
                  end if;
               end if;

               --  port
               if Is_In (Port_Iter) then
                  --  in port
                  Append_Node_To_List (PN_Port, In_Ports (PN_Thread));
               else
                  --  out port
                  Append_Node_To_List (PN_Port, Out_Ports (PN_Thread));
               end if;
            end if;

            Port_Iter := AIN.Next_Node (Port_Iter);
         end loop;
      end if;
   end PN_Build_Port;

   -------------------------
   --  PN_Build_Tpn_Port  --
   -------------------------

   procedure PN_Build_Tpn_Port
     (Aadl_Instance : Node_Id;
      PN_Generated  : Types.Node_Id;
      Port_Instance : Node_Id;
      PN_Port       : Node_Id;
      Is_Data       : Boolean := True)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Iutils;
      use Ocarina.AADL_Values;
      use Ocarina.Backends.Properties;
      use Ocarina.Namet;
      use AIN;
      use Ocarina.Backends.Utils;
      use Ocarina.Instances.Queries;

      T_PN_Push       : Node_Id;
      T_PN_Pop        : Node_Id;
      P_PN_Store      : Node_Id;
      A_PN_Arc        : Node_Id;
      Source_Point    : Node_Id;
      Endpoint        : Node_Id;
      Ports           : List_Id;
      Build_Data_Port : Boolean := Is_Data;
   begin

      if Is_In (Port_Instance) then
         --  in port
         Endpoint := Port_Instance;
         Ports    := Get_Source_Ports (Port_Instance);
         if AINU.Is_Empty (Ports) then
            Display_Located_Error
              (AIN.Loc (Port_Instance),
               "This IN port is not connected to any destination",
               Fatal => True);
         else
            Source_Point := Item (AIN.First_Node (Ports));
         end if;

      else
         --  out port
         Source_Point := Port_Instance;
         Ports        := Get_Destination_Ports (Port_Instance);
         if AINU.Is_Empty (Ports) then
            Display_Located_Error
              (AIN.Loc (Port_Instance),
               "This OUT port is not connected to any destination",
               Fatal => True);
         else
            Endpoint := Item (AIN.First_Node (Ports));
         end if;

         --  for out data ports only
         --  to avoid endless fireable transition
         Build_Data_Port := False;
      end if;

      --  update attributes for later use
      Set_Target_Instance (PN_Port, Endpoint);
      Set_Source_Instance (PN_Port, Source_Point);

      --  build name
      Set_Str_To_Name_Buffer
        (Get_Name_String
           (Compute_Absolute_Name_Of_Entity (Aadl_Instance, Separator)));
      Add_Str_To_Name_Buffer ("_");

      Add_Str_To_Name_Buffer ("_Push_Port");
      T_PN_Push := PN_TPN_Get_New_Transition;
      PN_TPN_Init_Transition
        (T_PN_Push,
         Port_Instance,
         Name_Find,
         PN_Generated,
         0);

      Append_Node_To_List (T_PN_Push, Public_Interfaces (PN_Port));

      Set_Str_To_Name_Buffer
        (Get_Name_String
           (Compute_Absolute_Name_Of_Entity (Aadl_Instance, Separator)));
      Add_Str_To_Name_Buffer ("_");
      Add_Str_To_Name_Buffer ("_Pop_Port");
      T_PN_Pop := PN_TPN_Get_New_Transition;
      PN_TPN_Init_Transition
        (T_PN_Pop,
         Port_Instance,
         Name_Find,
         PN_Generated,
         0);

      Append_Node_To_List (T_PN_Pop, Public_Interfaces (PN_Port));

      --
      Set_Str_To_Name_Buffer
        (Get_Name_String
           (Compute_Absolute_Name_Of_Entity (Aadl_Instance, Separator)));
      Add_Str_To_Name_Buffer ("_");
      Add_Str_To_Name_Buffer ("_Store_Port");
      P_PN_Store := PN_TPN_Get_New_Place;
      PN_TPN_Init_Place
        (P_PN_Store,
         Port_Instance,
         Name_Find,
         PN_Generated,
         0);
      Append_Node_To_List (P_PN_Store, Internal_Places (PN_Port));

      --
      A_PN_Arc := PN_TPN_Get_New_Arc;
      PN_TPN_Init_Arc (A_PN_Arc, Aadl_Instance, T_PN_Push, P_PN_Store, 0);
      Append_Node_To_List (A_PN_Arc, Pn_Arcs_Out (T_PN_Push));

      A_PN_Arc := PN_TPN_Get_New_Arc;
      PN_TPN_Init_Arc (A_PN_Arc, Aadl_Instance, P_PN_Store, T_PN_Pop, 0);
      Append_Node_To_List (A_PN_Arc, Pn_Arcs_In (T_PN_Pop));

      if Build_Data_Port then
         --  add arcs
         A_PN_Arc := PN_TPN_Get_New_Arc;
         PN_TPN_Init_Arc (A_PN_Arc, Aadl_Instance, P_PN_Store, T_PN_Push, 0);
         Append_Node_To_List (A_PN_Arc, Pn_Arcs_In (T_PN_Push));

         A_PN_Arc := PN_TPN_Get_New_Arc;
         PN_TPN_Init_Arc (A_PN_Arc, Aadl_Instance, T_PN_Pop, P_PN_Store, 0);
         Append_Node_To_List (A_PN_Arc, Pn_Arcs_Out (T_PN_Pop));
         --  set marking
         Set_Tokens_Number (P_PN_Store, New_Integer_Value (1));
      end if;

   end PN_Build_Tpn_Port;

   -------------------------
   --  PN_Build_Cpn_Port  --
   -------------------------

   procedure PN_Build_Cpn_Port
     (Aadl_Instance : Node_Id;
      PN_Generated  : Types.Node_Id;
      Port_Instance : Node_Id;
      PN_Port       : Node_Id;
      Is_Data       : Boolean := True)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Iutils;
      use Ocarina.AADL_Values;
      use Ocarina.Backends.Properties;
      use Ocarina.Namet;
      use AIN;
      use Ocarina.Backends.Utils;
      use Ocarina.Instances.Queries;

      T_PN_Push : Node_Id;
      T_PN_Pop  : Node_Id;
      T_PN_Ovf  : Node_Id;

      A_PN_Arc     : Node_Id;
      Source_Point : Node_Id;
      Endpoint     : Node_Id;

      Build_Data_Port : constant Boolean := Is_Data;
   begin

      if Is_In (Port_Instance) then
         --  in port
         Endpoint     := Port_Instance;
         Source_Point :=
           Item (AIN.First_Node (Get_Source_Ports (Port_Instance)));
      else
         --  out port
         Source_Point := Port_Instance;
         Endpoint     :=
           Item (AIN.First_Node (Get_Destination_Ports (Port_Instance)));

      end if;

      --  update attributes for later use
      Set_Target_Instance (PN_Port, Endpoint);
      Set_Source_Instance (PN_Port, Source_Point);

      --  build name
      Set_Str_To_Name_Buffer
        (Get_Name_String
           (Compute_Absolute_Name_Of_Entity (Aadl_Instance, Separator)));
      --

      Add_Str_To_Name_Buffer ("_Push_Port");
      T_PN_Push := PN_CPN_Get_New_Transition;
      PN_CPN_Init_Transition
        (T_PN_Push,
         Port_Instance,
         Name_Find,
         PN_Generated,
         0);

      Append_Node_To_List (T_PN_Push, Public_Interfaces (PN_Port));

      Set_Str_To_Name_Buffer
        (Get_Name_String
           (Compute_Absolute_Name_Of_Entity (Aadl_Instance, Separator)));
      Add_Str_To_Name_Buffer ("_");
      Add_Str_To_Name_Buffer ("_Pop_Port");
      T_PN_Pop := PN_CPN_Get_New_Transition;
      PN_CPN_Init_Transition
        (T_PN_Pop,
         Port_Instance,
         Name_Find,
         PN_Generated,
         0);

      Append_Node_To_List (T_PN_Pop, Public_Interfaces (PN_Port));

      --

      if Build_Data_Port then
         declare
            P_PN_Store : Node_Id;
         begin
            if not Is_Out (Port_Instance) then
               Set_Str_To_Name_Buffer
                 (Get_Name_String
                    (Compute_Absolute_Name_Of_Entity
                       (Aadl_Instance,
                        Separator)));

               Add_Str_To_Name_Buffer ("_");
               Add_Str_To_Name_Buffer ("_Store_Port");
               P_PN_Store := PN_CPN_Get_New_Place;
               PN_CPN_Init_Place
                 (P_PN_Store,
                  Port_Instance,
                  Name_Find,
                  PN_Generated,
                  0);
               Append_Node_To_List (P_PN_Store, Internal_Places (PN_Port));

               --
               A_PN_Arc := PN_CPN_Get_New_Arc;
               PN_CPN_Init_Arc
                 (A_PN_Arc,
                  Aadl_Instance,
                  T_PN_Push,
                  P_PN_Store,
                  4);
               Append_Node_To_List (A_PN_Arc, Pn_Arcs_Out (T_PN_Push));

               A_PN_Arc := PN_CPN_Get_New_Arc;
               PN_CPN_Init_Arc
                 (A_PN_Arc,
                  Aadl_Instance,
                  P_PN_Store,
                  T_PN_Pop,
                  2);
               Append_Node_To_List (A_PN_Arc, Pn_Arcs_In (T_PN_Pop));

               --  add arcs for in
               --  because if we keep this pattern for out, then then due to
               --  bus_interconnection, there will be endless fireable
               --  transition.
               A_PN_Arc := PN_CPN_Get_New_Arc;
               PN_CPN_Init_Arc
                 (A_PN_Arc,
                  Aadl_Instance,
                  P_PN_Store,
                  T_PN_Push,
                  2);
               Append_Node_To_List (A_PN_Arc, Pn_Arcs_In (T_PN_Push));

               A_PN_Arc := PN_CPN_Get_New_Arc;
               PN_CPN_Init_Arc
                 (A_PN_Arc,
                  Aadl_Instance,
                  T_PN_Pop,
                  P_PN_Store,
                  2);
               Append_Node_To_List (A_PN_Arc, Pn_Arcs_Out (T_PN_Pop));
            end if;
         end;
      else
         --  event port

         if not Is_Out (Port_Instance) then

            --  need to add interface to handle overflow

            --  deafault policy = DropOldest
            --  default queue size = 1
            declare
               Q_Size     : Long_Long                   := 1;
               Ovf_Policy : constant Unsigned_Long_Long := 1;
            --  1 : DropOldest, 2 : DropNewest, 3 : Error
            begin
               Set_Str_To_Name_Buffer
                 (Get_Name_String
                    (Compute_Absolute_Name_Of_Entity
                       (Aadl_Instance,
                        Separator)));
               Add_Str_To_Name_Buffer ("_");
               case Ovf_Policy is
                  when 1 =>
                     Add_Str_To_Name_Buffer ("_DropOldest_Port");
                  when others =>
                     null;
               end case;
               T_PN_Ovf := PN_CPN_Get_New_Transition;
               PN_CPN_Init_Transition
                 (T_PN_Ovf,
                  Port_Instance,
                  Name_Find,
                  PN_Generated,
                  0);
               Append_Node_To_List (T_PN_Ovf, Public_Interfaces (PN_Port));

               --  build as many slots as needed according to queue_size

               --  update queue_size according to port_instance
               if Get_Queue_Size (Port_Instance) >= 0 then
                  --  only if specified by user
                  Q_Size := Get_Queue_Size (Port_Instance);
               end if;

               --  particular case : Q_Size = 1
               if Q_Size = 1 then
                  declare
                     Slot_Node, Empty_Node : Node_Id;
                  begin
                     Set_Str_To_Name_Buffer
                       (Get_Name_String
                          (Compute_Absolute_Name_Of_Entity
                             (Aadl_Instance,
                              Separator)));

                     Add_Str_To_Name_Buffer ("_");
                     Add_Str_To_Name_Buffer ("_Slot_Port");
                     Slot_Node := PN_CPN_Get_New_Place;
                     PN_CPN_Init_Place
                       (Slot_Node,
                        Port_Instance,
                        Name_Find,
                        PN_Generated,
                        0);
                     Append_Node_To_List
                       (Slot_Node,
                        Internal_Places (PN_Port));

                     Set_Str_To_Name_Buffer
                       (Get_Name_String
                          (Compute_Absolute_Name_Of_Entity
                             (Aadl_Instance,
                              Separator)));
                     Add_Str_To_Name_Buffer ("_");
                     Add_Str_To_Name_Buffer ("_Empty_Port");
                     Empty_Node := PN_CPN_Get_New_Place;
                     PN_CPN_Init_Place
                       (Empty_Node,
                        Port_Instance,
                        Name_Find,
                        PN_Generated,
                        1);
                     Append_Node_To_List
                       (Empty_Node,
                        Internal_Places (PN_Port));

                     --  make connections
                     A_PN_Arc := PN_CPN_Get_New_Arc;
                     PN_CPN_Init_Arc
                       (A_PN_Arc,
                        Aadl_Instance,
                        T_PN_Push,
                        Slot_Node,
                        4);
                     Append_Node_To_List (A_PN_Arc, Pn_Arcs_Out (T_PN_Push));

                     A_PN_Arc := PN_CPN_Get_New_Arc;
                     PN_CPN_Init_Arc
                       (A_PN_Arc,
                        Aadl_Instance,
                        Slot_Node,
                        T_PN_Pop,
                        2);
                     Append_Node_To_List (A_PN_Arc, Pn_Arcs_In (T_PN_Pop));

                     A_PN_Arc := PN_CPN_Get_New_Arc;
                     PN_CPN_Init_Arc
                       (A_PN_Arc,
                        Aadl_Instance,
                        Slot_Node,
                        T_PN_Ovf,
                        2);
                     Append_Node_To_List (A_PN_Arc, Pn_Arcs_In (T_PN_Ovf));

                     A_PN_Arc := PN_CPN_Get_New_Arc;
                     PN_CPN_Init_Arc
                       (A_PN_Arc,
                        Aadl_Instance,
                        T_PN_Ovf,
                        Slot_Node,
                        4);
                     Append_Node_To_List (A_PN_Arc, Pn_Arcs_Out (T_PN_Ovf));

                     --  manage empty slots
                     A_PN_Arc := PN_CPN_Get_New_Arc;
                     PN_CPN_Init_Arc
                       (A_PN_Arc,
                        Aadl_Instance,
                        T_PN_Pop,
                        Empty_Node,
                        0);
                     Append_Node_To_List (A_PN_Arc, Pn_Arcs_Out (T_PN_Pop));

                     A_PN_Arc := PN_CPN_Get_New_Arc;
                     PN_CPN_Init_Arc
                       (A_PN_Arc,
                        Aadl_Instance,
                        Empty_Node,
                        T_PN_Push,
                        0);
                     Append_Node_To_List (A_PN_Arc, Pn_Arcs_In (T_PN_Push));
                  end;
               else
                  while Q_Size > 0 loop
                     null;
                     --  next
                     Q_Size := Q_Size - 1;
                  end loop;
               end if;
            end;

         else
            --  out event port
            null;
            --  need no place
            --  to avoid unbounded places problems, and difficulties to delete
            --  from list
         end if;                        --  in event port
      end if;

   end PN_Build_Cpn_Port;

   --------------------------
   --  PN_Thread_Skeleton  --
   --------------------------

   procedure PN_Thread_Skeleton
     (Aadl_Instance  : Types.Node_Id;
      PN_Thread      : Types.Node_Id;
      PN_Generated   : Types.Node_Id;
      PN_P_Init_Proc : PN_Init_Node;
      PN_P_New_Proc  : PN_New_Node;
      PN_T_Init_Proc : PN_Init_Node;
      PN_T_New_Proc  : PN_New_Node;
      PN_A_Init_Proc : PN_Init_Arc;
      PN_A_New_Proc  : PN_New_Node)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Iutils;
      use Ocarina.AADL_Values;
      use Ocarina.Namet;

      type Thread_Pattern is array (Integer range <>) of Name_Id;

      P_Skeleton : constant Thread_Pattern :=
        (Get_String_Name ("_Halted"), Get_String_Name ("_Wait_For_Dispatch"));
      T_Interface_Skeleton : constant Thread_Pattern :=
        (Get_String_Name ("_Compute"), Get_String_Name ("_Complete"));
      T_Local_Skeleton : constant Thread_Pattern :=
        (1 => Get_String_Name ("_Init_Dispatch"));

      P, T, A : Types.Node_Id;
   begin
      --  local places
      for PN in P_Skeleton'First .. P_Skeleton'Last loop
         P := PN_P_New_Proc.all;
         PN_P_Init_Proc (P, Aadl_Instance, P_Skeleton (PN), PN_Generated, 0);
         --  add new place into thread box
         Append_Node_To_List (P, Internal_Places (PN_Thread));
      end loop;
      --  local transitions
      for TN in T_Local_Skeleton'First .. T_Local_Skeleton'Last loop
         T := PN_T_New_Proc.all;
         PN_T_Init_Proc
           (T,
            Aadl_Instance,
            T_Local_Skeleton (TN),
            PN_Generated,
            0);
         --  add new transition into thread box
         Append_Node_To_List (T, Internal_Transitions (PN_Thread));

         --  arcs in
         P := First_Node (Internal_Places (PN_Thread));
         if P /= No_Node then
            A := PN_A_New_Proc.all;
            PN_A_Init_Proc (A, Aadl_Instance, P, T, 5);
            Append_Node_To_List (A, Pn_Arcs_In (T));

            P := Next_Node (P);
            if P /= No_Node then
               --  arcs out
               A := PN_A_New_Proc.all;
               PN_A_Init_Proc (A, Aadl_Instance, T, P, 5);
               Append_Node_To_List (A, Pn_Arcs_Out (T));
            end if;
         end if;

      end loop;
      --  interfaces transitions
      for TN in T_Interface_Skeleton'First .. T_Interface_Skeleton'Last loop
         T := PN_T_New_Proc.all;
         PN_T_Init_Proc
           (T,
            Aadl_Instance,
            T_Interface_Skeleton (TN),
            PN_Generated,
            0);
         --  add new transition into thread box
         Append_Node_To_List (T, Public_Interfaces (PN_Thread));
      end loop;
      --  for T_Interface_Skeleton, arcs
      P := First_Node (Internal_Places (PN_Thread));
      if P /= No_Node then
         P := Next_Node (P);            --  wait_for_dispatch
         if P /= No_Node then
            T := First_Node (Public_Interfaces (PN_Thread));
            if T /= No_Node then
               --  compute
               A := PN_A_New_Proc.all;
               PN_A_Init_Proc (A, Aadl_Instance, P, T, 1);
               Append_Node_To_List (A, Pn_Arcs_In (T));

               T := Next_Node (T);
               --  complete
               A := PN_A_New_Proc.all;
               PN_A_Init_Proc (A, Aadl_Instance, T, P, 1);
               Append_Node_To_List (A, Pn_Arcs_Out (T));
            end if;
         end if;
      end if;
      ---

      --  XXX compilation purpose, to fix
      --  if PN_Generated = No_Node then
      --     null;
      --  end if;

   end PN_Thread_Skeleton;

end Ocarina.Backends.PN.Components;
