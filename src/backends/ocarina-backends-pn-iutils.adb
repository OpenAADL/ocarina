------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . P N . I U T I L S            --
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

with Ocarina.AADL_Values;
with Ocarina.Namet;
with Ocarina.Instances.Queries;

with Ocarina.Backends.PN.Nodes;
with Ocarina.Backends.PN.Nutils;

with Ocarina.ME_AADL.AADL_Instances.Nodes;

with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL;

with Ocarina.Backends.PN.Debug;
with Ocarina.Backends.Utils;

package body Ocarina.Backends.PN.Iutils is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package OPND renames Ocarina.Backends.PN.Debug;

   --------------------------
   -- PN_Init_PN_Generated --
   --------------------------

   procedure PN_Init_PN_Generated (G : Types.Node_Id; F : Types.Value_Id) is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

   begin

      --  init pn_box
      Set_Pn_Box (G, New_Node (K_Pn_Box));
      Set_Pn_Subcomponents (Pn_Box (G), New_List (K_List_Id));
      Set_Pn_Interconnections (Pn_Box (G), New_List (K_List_Id));

      case F is
         when 0 =>
            Set_Pn_Formalism_Specific_Informations
              (G,
               New_Node (K_CPN_Specific_Informations));
         when 1 =>
            Set_Pn_Formalism_Specific_Informations
              (G,
               New_Node (K_TPN_Specific_Informations));
         when others =>
            null;
      end case;
      Set_Formalism (G, F);

   end PN_Init_PN_Generated;

   -----------------------------
   -- PN_Get_New_PN_Generated --
   -----------------------------

   function PN_Get_New_PN_Generated return Types.Node_Id is
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;

      G : constant Node_Id := New_Node (K_Pn_Generated);
   begin

      Set_Pn_Box (G, No_Node);
      Set_Pn_Formalism_Specific_Informations (G, No_Node);
      Set_Formalism (G, New_Integer_Value (0));

      return G;
   end PN_Get_New_PN_Generated;

   ---------------------------------
   -- PN_Get_New_TPN_Informations --
   ---------------------------------

   function PN_Get_New_TPN_Informations return Types.Node_Id is
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;

      I : constant Node_Id := New_Node (K_TPN_Specific_Informations);
   begin
      Set_Th_Number (I, New_Integer_Value (0));
      Set_Hyperperiod (I, New_Integer_Value (1));
      Set_Priorities (I, New_List (K_List_Id));

      return I;
   end PN_Get_New_TPN_Informations;

   -----------------------------------
   -- PN_Get_New_Processor_Priority --
   -----------------------------------

   function PN_Get_New_Processor_Priority return Types.Node_Id is
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Nodes;

      PP : constant Node_Id := New_Node (K_TPN_Processor_Priority);
   begin
      return PP;
   end PN_Get_New_Processor_Priority;

   --------------------------------
   -- PN_Init_Processor_Priority --
   --------------------------------

   procedure PN_Init_Processor_Priority
     (PN_Proc : Types.Node_Id;
      Aadl_P  : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Nodes;
   begin
      Set_P_Instance (PN_Proc, Aadl_P);
      Set_Bounded_Trans (PN_Proc, New_List (K_List_Id));
   end PN_Init_Processor_Priority;

   -------------------------------
   -- Append_Trans_To_Processor --
   -------------------------------

   procedure Append_Trans_To_Processor
     (PN_T    : Types.Node_Id;
      PN_Proc : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;

   begin
      if PN_T /= No_Node and then PN_Proc /= No_Node then
         if OPN.Kind (PN_T) = K_TPN_Transition
           and then OPN.Kind (PN_Proc) = K_TPN_Processor_Priority
         then
            declare
               Node_Iter    : Node_Id;
               Current_Prio : Value_Type;
               Iter_Prio    : Value_Type;
            begin
               if Is_Empty (Bounded_Trans (PN_Proc)) then
                  declare
                     --  new trans to avoid
                     --  melting next_node
                     New_Trans : Node_Id;
                  begin
                     New_Trans := PN_TPN_Get_New_Transition;
                     Set_Priority (New_Trans, Priority (PN_T));
                     Set_Identifier (New_Trans, Identifier (PN_T));
                     Append_Node_To_List (New_Trans, Bounded_Trans (PN_Proc));
                  end;
               else
                  Node_Iter    := OPN.First_Node (Bounded_Trans (PN_Proc));
                  Current_Prio := Get_Value_Type (Priority (PN_T));
                  Iter_Prio    := Get_Value_Type (Priority (Node_Iter));
                  if Current_Prio.IVal >= Iter_Prio.IVal then
                     --  push in
                     declare
                        --  new trans to avoid
                        --  melting next_node
                        New_Trans : Node_Id;
                     begin
                        New_Trans := PN_TPN_Get_New_Transition;
                        Set_Priority (New_Trans, Priority (PN_T));
                        Set_Identifier (New_Trans, Identifier (PN_T));

                        Push_Node_Into_List
                          (New_Trans,
                           Bounded_Trans (PN_Proc));
                     end;
                  else
                     --  need to run across others
                     while Present (Node_Iter) loop
                        if Current_Prio.IVal < Iter_Prio.IVal then
                           --  here, we will have to do another step
                           declare
                              Next_Iter      : Node_Id;
                              Next_Iter_Prio : Value_Type;
                           begin
                              Next_Iter := OPN.Next_Node (Node_Iter);
                              if Next_Iter = No_Node then
                                 --  append here
                                 declare
                                    --  new trans to avoid
                                    --  melting next_node
                                    New_Trans : Node_Id;
                                 begin
                                    New_Trans := PN_TPN_Get_New_Transition;
                                    Set_Priority (New_Trans, Priority (PN_T));
                                    Set_Identifier
                                      (New_Trans,
                                       Identifier (PN_T));

                                    Append_Node_To_List
                                      (New_Trans,
                                       Bounded_Trans (PN_Proc));
                                    --  leave loop
                                    exit;
                                 end;
                              else
                                 Next_Iter_Prio :=
                                   Get_Value_Type (Priority (Next_Iter));
                                 if Current_Prio.IVal >=
                                   Next_Iter_Prio.IVal
                                 then
                                    --  insert in list
                                    declare
                                       --  new trans to avoid
                                       --  melting next_node
                                       New_Trans : Node_Id;
                                    begin
                                       New_Trans := PN_TPN_Get_New_Transition;
                                       Set_Priority
                                         (New_Trans,
                                          Priority (PN_T));
                                       Set_Identifier
                                         (New_Trans,
                                          Identifier (PN_T));

                                       Set_Next_Node (Node_Iter, New_Trans);
                                       Set_Next_Node (New_Trans, Next_Iter);
                                       --  leave loop
                                       exit;
                                    end;
                                 end if;
                              end if;
                           end;
                        end if;
                        --  next
                        Node_Iter := OPN.Next_Node (Node_Iter);
                     end loop;
                  end if;
               end if;
            end;

         end if;
      end if;
   end Append_Trans_To_Processor;

   ----------------------------------
   -- PN_Get_New_Processor_Pattern --
   ----------------------------------

   function PN_Get_New_Processor_Pattern return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      P : constant Node_Id := New_Node (K_Processor_Pattern);
   begin
      return P;
   end PN_Get_New_Processor_Pattern;

   -------------------------------
   -- PN_Init_Processor_Pattern --
   -------------------------------

   procedure PN_Init_Processor_Pattern
     (PN_Proc : Types.Node_Id;
      Aadl_P  : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.AADL_Values;

   begin

      if PN_Proc /= No_Node and then Aadl_P /= No_Node then
         PN_Init_PN_Component (PN_Proc, Aadl_P);

         Set_Proc_Instance (PN_Proc, Aadl_P);
      end if;
   end PN_Init_Processor_Pattern;

   -------------------------------
   -- PN_Get_New_Thread_Pattern --
   -------------------------------

   function PN_Get_New_Thread_Pattern return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      T : constant Node_Id := New_Node (K_Thread_Pattern);
   begin
      return T;
   end PN_Get_New_Thread_Pattern;

   ----------------------------
   -- PN_Init_Thread_Pattern --
   ----------------------------

   procedure PN_Init_Thread_Pattern
     (PN_T   : Types.Node_Id;
      Aadl_T : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.AADL_Values;

   begin

      if PN_T /= No_Node and then Aadl_T /= No_Node then
         PN_Init_PN_Component (PN_T, Aadl_T);

         Set_In_Ports (PN_T, New_List (K_List_Id));
         Set_Out_Ports (PN_T, New_List (K_List_Id));
         Set_Hyperperiod (PN_T, New_Integer_Value (0));
         Set_Call_Seq (PN_T, New_List (K_List_Id));
         Set_Th_Instance (PN_T, Aadl_T);
      end if;

   end PN_Init_Thread_Pattern;

   -----------------------------
   -- PN_Get_New_Port_Pattern --
   -----------------------------

   function PN_Get_New_Port_Pattern return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      P : constant Node_Id := New_Node (K_Port_Pattern);
   begin
      return P;
   end PN_Get_New_Port_Pattern;

   --------------------------
   -- PN_Init_Port_Pattern --
   --------------------------

   procedure PN_Init_Port_Pattern
     (PN_P   : Types.Node_Id;
      Aadl_P : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.AADL_Values;

   begin

      if PN_P /= No_Node and then Aadl_P /= No_Node then
         PN_Init_PN_Component (PN_P, Aadl_P);

         Set_Port_Instance (PN_P, Aadl_P);
         Set_Source_Instance (PN_P, No_Node);
         Set_Target_Instance (PN_P, No_Node);
      end if;

   end PN_Init_Port_Pattern;

   -------------------------------
   -- PN_Get_New_D_Port_Pattern --
   -------------------------------

   function PN_Get_New_D_Port_Pattern return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      DP : constant Node_Id := New_Node (K_Data_Port_Pattern);
   begin
      return DP;
   end PN_Get_New_D_Port_Pattern;

   ----------------------------
   -- PN_Init_D_Port_Pattern --
   ----------------------------

   procedure PN_Init_D_Port_Pattern
     (PN_DP   : Types.Node_Id;
      Aadl_DP : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.AADL_Values;

   begin

      if PN_DP /= No_Node and then Aadl_DP /= No_Node then
         PN_Init_Port_Pattern (PN_DP, Aadl_DP);
      end if;
   end PN_Init_D_Port_Pattern;

   --------------------------------
   -- PN_Get_New_ED_Port_Pattern --
   --------------------------------

   function PN_Get_New_ED_Port_Pattern return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      DEP : constant Node_Id := New_Node (K_Data_Event_Port_Pattern);
   begin
      return DEP;
   end PN_Get_New_ED_Port_Pattern;

   -----------------------------
   -- PN_Init_ED_Port_Pattern --
   -----------------------------

   procedure PN_Init_ED_Port_Pattern
     (PN_DEP   : Types.Node_Id;
      Aadl_DEP : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.AADL_Values;

   begin

      if PN_DEP /= No_Node and then Aadl_DEP /= No_Node then
         PN_Init_Port_Pattern (PN_DEP, Aadl_DEP);

         Set_Queue_Size (PN_DEP, New_Integer_Value (1));
         Set_Has_CEP (PN_DEP, False);
         Set_Dispatch_Port (PN_DEP, False);
      end if;
   end PN_Init_ED_Port_Pattern;

   ---------------------------------
   -- PN_Get_New_Call_Seq_Pattern --
   ---------------------------------

   function PN_Get_New_Call_Seq_Pattern return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      CS : constant Node_Id := New_Node (K_Call_Sequence_Pattern);
   begin
      return CS;

   end PN_Get_New_Call_Seq_Pattern;

   ------------------------------
   -- PN_Init_Call_Seq_Pattern --
   ------------------------------

   procedure PN_Init_Call_Seq_Pattern
     (PN_CS  : Types.Node_Id;
      Aadl_T : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
   begin

      if PN_CS /= No_Node and then Aadl_T /= No_Node then
         PN_Init_PN_Component (PN_CS, Aadl_T);

         Set_Spg_Call (PN_CS, New_List (K_List_Id));
      end if;

   end PN_Init_Call_Seq_Pattern;

   ----------------------------
   -- PN_Get_New_Spg_Pattern --
   ----------------------------

   function PN_Get_New_Spg_Pattern return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      SPG : constant Node_Id := New_Node (K_Subprogram_Call_Pattern);
   begin
      return SPG;
   end PN_Get_New_Spg_Pattern;

   -------------------------
   -- PN_Init_Spg_Pattern --
   -------------------------

   procedure PN_Init_Spg_Pattern
     (PN_Spg   : Types.Node_Id;
      Aadl_Spg : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
   begin
      if PN_Spg /= No_Node and then Aadl_Spg /= No_Node then
         PN_Init_PN_Component (PN_Spg, Aadl_Spg);

         Set_Param_In (PN_Spg, New_List (K_List_Id));
         Set_Param_Out (PN_Spg, New_List (K_List_Id));
      end if;
   end PN_Init_Spg_Pattern;

   --------------------------------
   -- PN_Get_New_Spg_Par_Pattern --
   --------------------------------

   function PN_Get_New_Spg_Par_Pattern return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      SPG_PAR : constant Node_Id := New_Node (K_Spg_Parameter_Pattern);
   begin
      return SPG_PAR;
   end PN_Get_New_Spg_Par_Pattern;

   -----------------------------
   -- PN_Init_Spg_Par_Pattern --
   -----------------------------

   procedure PN_Init_Spg_Par_Pattern
     (PN_Spg_Par   : Types.Node_Id;
      Aadl_Spg_Par : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
   begin
      if PN_Spg_Par /= No_Node and then Aadl_Spg_Par /= No_Node then
         PN_Init_PN_Component (PN_Spg_Par, Aadl_Spg_Par);

         Set_Par_Instance (PN_Spg_Par, Aadl_Spg_Par);
      end if;
   end PN_Init_Spg_Par_Pattern;

   --------------------------
   -- PN_Init_PN_Component --
   --------------------------

   procedure PN_Init_PN_Component
     (PN_C   : Types.Node_Id;
      Aadl_C : Types.Node_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Namet;

   begin

      if PN_C /= No_Node and then Aadl_C /= No_Node then
         Set_Str_To_Name_Buffer ("::");
         PN_Init_PN_Node (PN_C, Aadl_C, Name_Find);

         Set_Public_Interfaces (PN_C, New_List (K_List_Id));
         Set_Internal_Transitions (PN_C, New_List (K_List_Id));
         Set_Internal_Places (PN_C, New_List (K_List_Id));
      end if;

   end PN_Init_PN_Component;

   ---------------------
   -- PN_Init_PN_Node --
   ---------------------

   procedure PN_Init_PN_Node
     (PN_N   : Types.Node_Id;
      Aadl_N : Types.Node_Id;
      Name   : Types.Name_Id)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Namet;
      use Ocarina.Instances.Queries;

   begin
      if PN_N /= No_Node then
         PN_Init_Node (PN_N);
         if Aadl_N /= No_Node then
            Set_Str_To_Name_Buffer
              (Get_Name_String
                 (Compute_Absolute_Name_Of_Entity (Aadl_N, Separator)));
            Add_Str_To_Name_Buffer (Get_Name_String (Name));
         else
            Set_Str_To_Name_Buffer (Get_Name_String (Name));
         end if;

         Set_Identifier (PN_N, Make_Identifier (PN_N, Name_Find));

      end if;
   end PN_Init_PN_Node;

   ------------------
   -- PN_Init_Node --
   ------------------

   procedure PN_Init_Node (N : Types.Node_Id) is
      use Ocarina.Backends.PN.Nodes;

   begin
      if N /= No_Node then
         Set_Next_Node (N, No_Node);
      end if;
   end PN_Init_Node;

   -------------------------
   --  PN_TPN_Init_Place  --
   -------------------------

   procedure PN_TPN_Init_Place
     (P    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id;
      PN_G : Types.Node_Id;
      M    : Unsigned_Long_Long)
   is
      pragma Unreferenced (M);

      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;

   begin
      if P /= No_Node and then PN_G /= No_Node then
         PN_Init_Place (P, Aadl, Name);

         Set_Tokens_Number (P, New_Integer_Value (0));
         --  compilation
--           if M = 0 then
--              null;
--           end if;
      end if;
   end PN_TPN_Init_Place;

   -------------------
   -- PN_Init_Place --
   -------------------

   procedure PN_Init_Place
     (P    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id)
   is
   begin
      if P /= No_Node then
         PN_Init_PN_Node (P, Aadl, Name);
      end if;
   end PN_Init_Place;

   --------------------------
   -- PN_TPN_Get_New_Place --
   --------------------------

   function PN_TPN_Get_New_Place return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      P : constant Types.Node_Id := New_Node (K_TPN_Place);
   begin
      return P;
   end PN_TPN_Get_New_Place;

   ----------------------------
   -- PN_TPN_Init_Transition --
   ----------------------------

   procedure PN_TPN_Init_Transition
     (T    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id;
      PN_G : Types.Node_Id;
      M    : Unsigned_Long_Long)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;
      use Ocarina.Backends.PN.Nutils;
      pragma Unreferenced (M);
   begin
      if T /= No_Node and then PN_G /= No_Node then
         PN_Init_Transition (T, Aadl, Name);

         --  default : immediate transitions
         Set_Guard (T, New_Node (K_TPN_Guard));
         Set_Lower_Value (Guard (T), New_Integer_Value (0));
         Set_Higher_Value (Guard (T), New_Integer_Value (-1));
         Set_Braces_Mode (Guard (T), New_Integer_Value (2));

         Set_Priority (T, New_Integer_Value (0));

         --  compilation
--           if M = 0 then
--              raise Program_Error;   --  XXX
--           end if;
      end if;
   end PN_TPN_Init_Transition;

   ------------------------
   -- PN_Init_Transition --
   ------------------------

   procedure PN_Init_Transition
     (T    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id)
   is
      pragma Assert (Present (T));
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

   begin
      PN_Init_PN_Node (T, Aadl, Name);
      Set_Pn_Arcs_In (T, New_List (K_List_Id));
      Set_Pn_Arcs_Out (T, New_List (K_List_Id));
   end PN_Init_Transition;

   -------------------------------
   -- PN_TPN_Get_New_Transition --
   -------------------------------

   function PN_TPN_Get_New_Transition return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      P : constant Types.Node_Id := New_Node (K_TPN_Transition);
   begin
      return P;
   end PN_TPN_Get_New_Transition;

   ----------------------
   -- PN_TPN_Set_Guard --
   ----------------------

   procedure PN_TPN_Set_Guard
     (T           : Types.Node_Id;
      Low, Up     : Types.Value_Id;
      Braces_Mode : Types.Value_Id;
      Priority    : Types.Value_Id)
   is
      pragma Assert (Present (T));
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;

   begin
      Set_Lower_Value (Guard (T), Low);
      Set_Higher_Value (Guard (T), Up);
      Set_Braces_Mode (Guard (T), Braces_Mode);
      Set_Priority (T, Priority);
   end PN_TPN_Set_Guard;

   ------------------------
   -- PN_TPN_Get_New_Arc --
   ------------------------

   function PN_TPN_Get_New_Arc return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      A : constant Types.Node_Id := New_Node (K_TPN_Arc);
   begin
      return A;
   end PN_TPN_Get_New_Arc;

   --------------------------
   -- PN_TPN_Duplicate_Arc --
   --------------------------

   procedure PN_TPN_Duplicate_Arc
     (A        : Types.Node_Id;
      A_Inst   : Types.Node_Id;
      Endpoint : Types.Node_Id;
      From     : Boolean := False)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;
      use Ocarina.Namet;
      use Ocarina.Instances.Queries;
      use Ocarina.Backends.PN.Nutils;

      New_Arc : Types.Node_Id;
   begin
      New_Arc := PN_TPN_Get_New_Arc;
      if From then
         PN_TPN_Init_Arc (New_Arc, A_Inst, Endpoint, Pn_To (A));
         Append_Node_To_List (New_Arc, Pn_Arcs_Out (Endpoint));
      else
         PN_TPN_Init_Arc (New_Arc, A_Inst, Pn_From (A), Endpoint);
         Append_Node_To_List (New_Arc, Pn_Arcs_In (Endpoint));
      end if;
   end PN_TPN_Duplicate_Arc;

   ---------------------
   -- PN_TPN_Init_Arc --
   ---------------------

   procedure PN_TPN_Init_Arc
     (A    : Types.Node_Id;
      Aadl : Types.Node_Id;
      From : Types.Node_Id      := No_Node;
      To   : Types.Node_Id      := No_Node;
      K    : Unsigned_Long_Long := 0)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;
      use Ocarina.Namet;
      use Ocarina.Instances.Queries;

   begin
      if A /= No_Node then
         Set_Str_To_Name_Buffer ("_a_");
         PN_Init_Arc (A, Aadl, From, To, Name_Find);

         Set_is_Priority (A, False);
         Set_Valuation (A, New_Integer_Value (K));
      end if;
   end PN_TPN_Init_Arc;

   -----------------
   -- PN_Init_Arc --
   -----------------

   procedure PN_Init_Arc
     (A    : Types.Node_Id;
      Aadl : Types.Node_Id;
      From : Types.Node_Id;
      To   : Types.Node_Id;
      Name : Types.Name_Id)
   is
      use Ocarina.Backends.PN.Nodes;

   begin

      if A /= No_Node then
         PN_Init_PN_Node (A, Aadl, Name);

         Set_Pn_From (A, From);
         Set_Pn_To (A, To);

      end if;

   end PN_Init_Arc;

   ---------------------------------
   -- PN_Get_New_CPN_Informations --
   ---------------------------------

   function PN_Get_New_CPN_Informations return Types.Node_Id is
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;
      use Ocarina.Namet;

      I : constant Node_Id := New_Node (K_CPN_Specific_Informations);
   begin

      Set_Classes (I, New_Node (K_CPN_Formalism_Classes));
      Set_Class_List (Classes (I), New_List (K_List_Id));
      --  two default classes : threads_id and messages
      declare
         Th_Ids : constant Node_Id :=
           New_Node (K_CPN_Formalism_Class_Item_Range);
         Msg_T : constant Node_Id :=
           New_Node (K_CPN_Formalism_Class_Item_Enum);
      begin
         PN_Init_PN_Node (Th_Ids, No_Node, Get_String_Name ("Threads_Ids"));
         Append_Node_To_List (Th_Ids, Class_List (Classes (I)));

         PN_Init_PN_Node (Msg_T, No_Node, Get_String_Name ("Msg_Types"));
         Set_Enum (Msg_T, New_List (K_List_Id));
         declare
            Class_Iter : constant Node_Id :=
              New_Node (K_CPN_Formalism_Class_Item_Enum_Item);
         begin
            PN_Init_PN_Node (Class_Iter, No_Node, Get_String_Name ("msg"));
            Append_Node_To_List (Class_Iter, Enum (Msg_T));
         end;

         Append_Node_To_List (Msg_T, Class_List (Classes (I)));
      end;
      ----
      Set_Domains (I, New_List (K_List_Id));
      --  default domain messages = <th_id x msg_type>
      Append_Node_To_List (New_Node (K_CPN_Formalism_Domains), Domains (I));
      PN_Init_PN_Node
        (OPN.First_Node (Domains (I)),
         No_Node,
         Get_String_Name ("mess"));
      Set_Domain_List (OPN.First_Node (Domains (I)), New_List (K_List_Id));

      declare
         Dom_Th_Id : constant Node_Id :=
           New_Node (K_CPN_Formalism_Class_Item_Range);
         Dom_Msg : constant Node_Id :=
           New_Node (K_CPN_Formalism_Class_Item_Enum_Item);
      begin
         PN_Init_PN_Node
           (Dom_Th_Id,
            No_Node,
            Name (Identifier (OPN.First_Node (Class_List (Classes (I))))));
         Append_Node_To_List
           (Dom_Th_Id,
            Domain_List (OPN.First_Node (Domains (I))));

         PN_Init_PN_Node
           (Dom_Msg,
            No_Node,
            Name
              (Identifier
                 (OPN.Next_Node (OPN.First_Node (Class_List (Classes (I)))))));
         Append_Node_To_List
           (Dom_Msg,
            Domain_List (OPN.First_Node (Domains (I))));
      end;
      ----
      Set_Variables (I, New_List (K_List_Id));
      --  init for default classes
      declare
         Class_Iter : Node_Id := OPN.First_Node (Class_List (Classes (I)));
         Var_Node   : Node_Id;
      begin
         while Present (Class_Iter) loop
            Var_Node := New_Node (K_CPN_Formalism_Variables);
            Set_Class_Type (Var_Node, Class_Iter);
            Set_Variable_List (Var_Node, New_List (K_List_Id));

            Append_Node_To_List (Var_Node, Variables (I));
            --  next
            Class_Iter := OPN.Next_Node (Class_Iter);
         end loop;
      end;
      --  default variables are x and y in th_ids, and m in Msg
      declare
         X : constant Node_Id := New_Node (K_CPN_Formalism_Variable_Item);
         Y : constant Node_Id := New_Node (K_CPN_Formalism_Variable_Item);
         M : constant Node_Id := New_Node (K_CPN_Formalism_Variable_Item);
      begin
         PN_Init_PN_Node (X, No_Node, Get_String_Name ("x"));
         PN_Init_PN_Node (Y, No_Node, Get_String_Name ("y"));
         PN_Init_PN_Node (M, No_Node, Get_String_Name ("m"));

         Append_Node_To_List
           (X,
            Variable_List (OPN.First_Node (Variables (I))));
         Append_Node_To_List
           (Y,
            Variable_List (OPN.First_Node (Variables (I))));
         Append_Node_To_List
           (M,
            Variable_List (OPN.Next_Node (OPN.First_Node (Variables (I)))));
      end;

      ----
      Set_Threads_Count (I, New_Integer_Value (0));
      Set_Threads_Ids (I, New_List (K_List_Id));
      Set_Ports_Ids (I, New_List (K_List_Id));

      return I;
   end PN_Get_New_CPN_Informations;

   ----------------------------
   --  PN_CPN_Get_New_Place  --
   ----------------------------

   function PN_CPN_Get_New_Place return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      P : constant Types.Node_Id := New_Node (K_CPN_Place);
   begin
      return P;
   end PN_CPN_Get_New_Place;

   -------------------------
   --  PN_CPN_Init_Place  --
   -------------------------

   procedure PN_CPN_Init_Place
     (P    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id;
      PN_G : Types.Node_Id;
      M    : Unsigned_Long_Long)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;
      use Ocarina.Namet;
      use Ocarina.AADL_Values;
      use Ocarina.ME_AADL.AADL_Instances.Entities;
      use Ocarina.ME_AADL;

   begin
      --  when Aadl is a thread, then the place holds thread id
      --  when it is a port, then the place holds a message type

      --  0 : event -> mess || data -> mess -> data marking
      --  1 : event -> marked uncolored
      PN_Init_Place (P, Aadl, Name);

      Set_Marking (P, New_Node (K_CPN_Marking));
      Set_Tokens (Marking (P), New_List (K_List_Id));
      Set_Nb_T (P, New_Integer_Value (0));

      case AIN.Kind (Aadl) is
         when AIN.K_Port_Spec_Instance =>
            if M = 0 then
               --  mess domain
               Set_Domain
                 (P,
                  OPN.First_Node
                    (Domains (Pn_Formalism_Specific_Informations (PN_G))));
               if not AIN.Is_Event (Aadl) and then AIN.Is_In (Aadl) then
                  --  data marking
                  declare
                     V_Node : Node_Id := New_Node (K_CPN_Marking_Token);
                  begin
                     PN_Init_PN_Node
                       (V_Node,
                        No_Node,
                        Get_String_Name ("data"));
                     Append_Node_To_List (V_Node, Tokens (Marking (P)));

                     V_Node := New_Node (K_CPN_Marking_Token);
                     PN_Init_PN_Node
                       (V_Node,
                        No_Node,
                        Get_String_Name ("0")); --  no th_id
                     Append_Node_To_List (V_Node, Tokens (Marking (P)));

                  end;

               end if;
            else
               --  uncolored
               Set_Domain (P, No_Node);
               --  set marking to 1
               Set_Nb_T (P, New_Integer_Value (1));
            end if;

         when others =>
            --  test for threads
            if Get_Category_Of_Component (Aadl) = CC_Thread then
               --  th_ids class
               Set_Domain
                 (P,
                  OPN.First_Node
                    (Class_List
                       (Classes (Pn_Formalism_Specific_Informations (PN_G)))));
               --  marking is set afterward
            end if;
      end case;
   end PN_CPN_Init_Place;

   -------------------------------
   -- PN_CPN_Get_New_Transition --
   -------------------------------

   function PN_CPN_Get_New_Transition return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      T : constant Types.Node_Id := New_Node (K_CPN_Transition);
   begin
      return T;
   end PN_CPN_Get_New_Transition;

   ----------------------------
   -- PN_CPN_Init_Transition --
   ----------------------------

   procedure PN_CPN_Init_Transition
     (T    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id;
      PN_G : Types.Node_Id;
      M    : Unsigned_Long_Long)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;
      use Ocarina.Backends.PN.Nutils;
      pragma Unreferenced (M);

   begin
      if T /= No_Node and then PN_G /= No_Node then
         PN_Init_Transition (T, Aadl, Name);

         --  default : immediate transitions
         Set_Guards (T, New_List (K_List_Id));

         --  compilation
--           if M = 0 then
--              raise Program_Error; --  XXX
--         end if;
      end if;
   end PN_CPN_Init_Transition;

   ------------------------
   -- PN_CPN_Get_New_Arc --
   ------------------------

   function PN_CPN_Get_New_Arc return Types.Node_Id is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.Backends.PN.Nutils;

      A : constant Types.Node_Id := New_Node (K_CPN_Arc);
   begin
      return A;
   end PN_CPN_Get_New_Arc;

   ---------------------
   -- PN_CPN_Init_Arc --
   ---------------------

   procedure PN_CPN_Init_Arc
     (A    : Types.Node_Id;
      Aadl : Types.Node_Id;
      From : Types.Node_Id := No_Node;
      To   : Types.Node_Id := No_Node;
      K    : Unsigned_Long_Long)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;
      use Ocarina.Namet;
      use Ocarina.Instances.Queries;
      use Ocarina.Backends.PN.Nutils;
      use OPND;
      use Ocarina.Backends.Utils;

      Val : Node_Id := New_Node (K_CPN_Arc_Valuation);
   begin

      if A /= No_Node then
         Set_Str_To_Name_Buffer ("_a_");
         PN_Init_Arc (A, Aadl, From, To, Name_Find);
         Set_Valuations (A, New_List (K_List_Id));

         case K is
            when 0 =>
               --  no valuation
               PN_Init_PN_Node (Val, No_Node, Get_String_Name ("1"));
               Set_Is_Colored (Val, False);
               Append_Node_To_List (Val, Valuations (A));
            when 1 =>
               --  from thread to thread
               PN_Init_PN_Node (Val, No_Node, Get_String_Name ("x"));
               Set_Is_Colored (Val, True);
               Append_Node_To_List (Val, Valuations (A));
            when 2 =>
               --  from port to thread or port to interconnection
               PN_Init_PN_Node (Val, No_Node, Get_String_Name ("y"));
               Set_Is_Colored (Val, True);
               Append_Node_To_List (Val, Valuations (A));
               Val := New_Node (K_CPN_Arc_Valuation);
               PN_Init_PN_Node (Val, No_Node, Get_String_Name ("m"));
               Set_Is_Colored (Val, True);
               Append_Node_To_List (Val, Valuations (A));
            when 3 =>
               --  from interconnection to port
               PN_Init_PN_Node (Val, No_Node, Get_String_Name ("x"));
               Set_Is_Colored (Val, True);
               Append_Node_To_List (Val, Valuations (A));
               Val := New_Node (K_CPN_Arc_Valuation);
               PN_Init_PN_Node (Val, No_Node, Get_String_Name ("m"));
               Set_Is_Colored (Val, True);
               Append_Node_To_List (Val, Valuations (A));
            when 4 =>
               --  from thread to port
               PN_Init_PN_Node (Val, No_Node, Get_String_Name ("x"));
               Set_Is_Colored (Val, True);
               Append_Node_To_List (Val, Valuations (A));
               Val := New_Node (K_CPN_Arc_Valuation);
               PN_Init_PN_Node (Val, No_Node, Get_String_Name ("msg"));
               Set_Is_Colored (Val, True);
               Append_Node_To_List (Val, Valuations (A));
            when 5 =>
               --  thread specific valuation
               --  find variable
               declare
                  Val_Handle : Node_Id :=
                    Get_Handling (Aadl, By_Node, H_PN_Cpn_Var);
               begin

                  if Val_Handle = No_Node then
                     PN_Init_PN_Node
                       (Val,
                        No_Node,
                        Get_String_Name ("x" & OPND.Image (Aadl)));
                     Set_Is_Colored (Val, True);
                     Set_Handling (Aadl, By_Node, H_PN_Cpn_Var, Val);
                     Val_Handle := Val;
                  end if;

                  Append_Node_To_List (Val_Handle, Valuations (A));
               end;
            when others =>
               null;
         end case;
      end if;
   end PN_CPN_Init_Arc;

   --------------------------
   -- PN_CPN_Duplicate_Arc --
   --------------------------

   procedure PN_CPN_Duplicate_Arc
     (A        : Types.Node_Id;
      A_Inst   : Types.Node_Id;
      Endpoint : Types.Node_Id;
      From     : Boolean := False)
   is
      use Ocarina.Backends.PN.Nodes;
      use Ocarina.AADL_Values;
      use Ocarina.Namet;
      use Ocarina.Instances.Queries;
      use Ocarina.Backends.PN.Nutils;

      New_Arc : Types.Node_Id;
   begin
      New_Arc := PN_CPN_Get_New_Arc;
      if From then
         PN_CPN_Init_Arc
           (New_Arc,
            A_Inst,
            Endpoint,
            Pn_To (A),
            0);           --  changed right after
         Set_Valuations (New_Arc, Valuations (A));

         Append_Node_To_List (New_Arc, Pn_Arcs_Out (Endpoint));
      else
         PN_CPN_Init_Arc
           (New_Arc,
            A_Inst,
            Pn_From (A),
            Endpoint,
            0);           --  changed right after
         Set_Valuations (New_Arc, Valuations (A));

         Append_Node_To_List (New_Arc, Pn_Arcs_In (Endpoint));
      end if;
   end PN_CPN_Duplicate_Arc;

end Ocarina.Backends.PN.Iutils;
