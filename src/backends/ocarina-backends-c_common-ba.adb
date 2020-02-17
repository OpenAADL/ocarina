------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . C _ C O M M O N . B A          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2019 ESA & ISAE.                       --
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
with Locations;
with Utils;
with Ocarina.Backends.Utils;

with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.Properties;
with Ocarina.Backends.C_Common.Mapping;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;
with Ocarina.Backends.Helper;
with Ocarina.Analyzer.AADL_BA;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.C_Common.Types;

package body Ocarina.Backends.C_Common.BA is

   use Ocarina.Analyzer.AADL_BA;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Namet;
   use Locations;
   use Ocarina.Backends.Helper;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.C_Tree.Nodes;
   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.PO_HI_C.Runtime;
   use Ocarina.ME_AADL;

   package AAN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package BATN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   package BANu renames Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   package CV renames Ocarina.Backends.C_Values;

   --  function Get_Instances_Of_Component_Type
   --    (Root : Node_Id; E : Node_Id) return Node_Id;

   function Map_Used_Type (N : Node_Id) return Node_Id;

   procedure Map_C_Many_Transitions_Of_A_Thread
     (S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   function Compute_Nb_States
     (S            : Node_Id) return Unsigned_Long_Long;

   procedure Map_BA_States_To_C_Types
     (S            : Node_Id);

   function Compute_Nb_Trans_Stemmed_From_A_Specific_Complete_Stat
     (BA             : Node_Id;
      Complete_State : Node_Id) return Unsigned_Long_Long;

   procedure Fill_Nb_Dispatch_Triggers_Of_Each_Transition_Array
     (S               : Node_Id;
      BA              : Node_Id;
      Complete_State  : Node_Id;
      Index_Comp_Stat : Unsigned_Long_Long;
      WStatements     : List_Id);

   procedure Fill_Dispatch_Triggers_Of_All_Transitions_Array
     (S               : Node_Id;
      BA              : Node_Id;
      Complete_State  : Node_Id;
      Index_Comp_Stat : Unsigned_Long_Long;
      WStatements     : List_Id);

   function Nb_All_Dispatch_Triggers_From_A_Specific_Complete_Stat
     (BA             : Node_Id;
      Complete_State : Node_Id) return Unsigned_Long_Long;

   function Max_Dispatch_Triggers_Per_Trans_From_A_Specific_Complete_Stat
     (BA             : Node_Id;
      Complete_State : Node_Id) return Unsigned_Long_Long;

   procedure Make_States_Initialization_Function
     (S            : Node_Id);

   procedure Make_Update_Next_Complete_State_Function
     (S            : Node_Id);

   function Search_State_Kind
     (BA        : Node_Id;
      State_Idt : Node_Id) return Ocarina.Types.Byte;

   function Find_Index_In_States_Array
     (BA        : Node_Id;
      State_Idt : Node_Id) return Unsigned_Long_Long;

   procedure Update_Current_State
     (E                : Node_Id;
      BA               : Node_Id;
      Transition_Node  : Node_Id;
      Stats            : List_Id);

   function Map_C_Transition_Node
     (Node             : Node_Id;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id;
      Else_St          : List_Id) return List_Id;

   function Map_C_On_Dispatch_Transition_Node
     (Node             : Node_Id;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id;
      Index_Transition : in out Unsigned_Long_Long) return List_Id;

   procedure Map_C_A_List_Of_Transitions
     (S                         : Node_Id;
      BA                        : Node_Id;
      Otherwise_Transition_Node : Node_Id;
      Sub_Transition_List       : List_Id;
      WDeclarations             : List_Id;
      WStatements               : List_Id);

   procedure Map_C_A_List_Of_On_Dispatch_Transitions
     (S                            : Node_Id;
      BA                           : Node_Id;
      Sub_Transition_List          : List_Id;
      WDeclarations                : List_Id;
      WStatements                  : List_Id);

   procedure Examine_Current_State_Until_Reaching_Complete_State
     (S                         : Node_Id;
      BA                        : Node_Id;
      WDeclarations             : List_Id;
      WStatements               : List_Id);

   procedure Make_BA_Initialization_Function
     (S            : Node_Id);

   procedure Map_C_Implementation_of_BA_Body_Function
     (S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Make_BA_Body_Function_For_Periodic_Thread
     (S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Make_BA_Body_Function_For_Sporadic_Thread
     (S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   function Map_C_State_Kind_Name
     (State_Kind : Ocarina.Types.Byte) return Node_Id;

   procedure Map_C_Behavior_Action_Block
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Map_C_Behav_Acts
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      WStatements  : List_Id);

   procedure Map_C_Behavior_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   function Map_C_Elsif_stat
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id;
      Else_st      : Node_Id) return List_Id;

   procedure Map_C_If_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Map_C_For_or_ForAll_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Map_C_While_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Map_C_Communication_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Make_Send_Output_Port
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id);

   procedure Make_Put_Value_On_port
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Make_Output_Port_Name
     (Node         : Node_Id;
      S            : Node_Id;
      Statements   : List_Id);

   function Map_C_Data_Component_Reference
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Map_C_BA_Name
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Map_C_Target
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id) return Node_Id;

   procedure Map_C_Assignment_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   function Evaluate_BA_Value_Expression
     (Node                 : Node_Id;
      Is_Out_parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id;

   function Evaluate_BA_Relation
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id;

   function Evaluate_BA_Operator (Node : Node_Id) return Operator_Type;

   function Evaluate_BA_Simple_Expression
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id;

   function Evaluate_BA_Term
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id;

   function Evaluate_BA_Factor
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id;

   function Evaluate_BA_Value
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id;

   function Evaluate_BA_Integer_Value
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id) return Node_Id;

   function Evaluate_BA_Literal (Node : Node_Id) return Node_Id;

   procedure Make_Intermediate_Variable_Declaration
     (N            : Name_Id;
      Used_Type    : Node_Id;
      Declarations : List_Id);

   function Get_Port_Spec_Instance
     (Node             : Node_Id;
      Parent_Component : Node_Id) return Node_Id;

   function Get_Subcomponent_Data_Instance
     (Node             : Node_Id;
      Parent_Component : Node_Id) return Node_Id;

   function Evaluate_BA_Value_Variable
     (Node             : Node_Id;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Property_Constant
     (Node                 : Node_Id;
      Is_Out_parameter     : Boolean := False;
      Subprogram_Root      : Node_Id;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id;

   function Make_Call_Parameter_For_Get_Count_and_Next_Value
     (Node : Node_Id;
      S    : Node_Id) return List_Id;

   function Make_Get_Count_of_Port
     (Node             : Node_Id;
      S                : Node_Id) return Node_Id;

   procedure Make_Next_Value_of_Port
     (Node             : Node_Id;
      S                : Node_Id;
      Statements       : List_Id);

   function Make_Request_Variable_Name_From_Port_Name
     (Port_Name : Name_Id) return Name_Id;

   procedure Make_Request_Variable_Declaration
     (Declarations : List_Id;
      Port_Name    : Name_Id);

   function Make_Get_Value_of_Port
     (Node             : Node_Id;
      Subprogram_Root  : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Identifier
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id;

   function Evaluate_BA_Boolean_Literal (Node : Node_Id) return Node_Id;

   --     -------------------------------------
   --     -- Get_Instances_Of_Component_Type --
   --     -------------------------------------
   --
   --     function Get_Instances_Of_Component_Type
   --       (Root : Node_Id; E : Node_Id) return Node_Id
   --     is
   --        use type Aan.Node_Kind;
   --
   --        pragma Assert
   --          (AAN.Kind (E) = Aan.K_Component_Type
   --           or else AAN.Kind (E) = Aan.K_Component_Implementation
   --           or else AAN.Kind (E) = Aan.K_Feature_Group_Type);
   --
   --        Fs : constant Ocarina.ME_AADL.AADL_Instances.Nutils.Node_Array
   --          := Features_Of (Root);
   --     begin
   --        for F of Fs loop
   --           if AIN.Corresponding_Declaration
   --             (AIN.Corresponding_Instance (F)) = E
   --           then
   --              return AIN.Corresponding_Instance (F);
   --           end if;
   --
   --        end loop;
   --
   --        --  raise Program_Error;
   --        return No_Node;
   --
   --     end Get_Instances_Of_Component_Type;

   ------------------------------
   -- Is_To_Make_Init_Sequence --
   ------------------------------

   function Is_To_Make_Init_Sequence (S : Node_Id) return Boolean
   is
      BA     : Node_Id;
      State  : Node_Id;
      Result : Boolean := False;
   begin
      BA := Get_Behavior_Specification (S);
      State := BATN.First_Node (BATN.States (BA));

      while Present (State) loop

         Result := Behavior_State_Kind'Val (BATN.State_Kind (State))
           = BSK_Initial;

         exit when Result;
         State := BATN.Next_Node (State);
      end loop;
      return Result;
   end Is_To_Make_Init_Sequence;

   --------------------------------
   -- Get_Behavior_Specification --
   --------------------------------

   function Get_Behavior_Specification
     (S : Node_Id) return Node_Id is
      D, BA  : Node_Id;
   begin
      D := AIN.First_Node (AIN.Annexes (S));
      BA := No_Node;
      while No (BA) loop
         if (Standard.Utils.To_Upper (AIN.Display_Name (AIN.Identifier (D))) =
               Standard.Utils.To_Upper (Get_String_Name
                                          ("behavior_specification")))
           and then Present (AIN.Corresponding_Annex (D))
         then
            BA := AIN.Corresponding_Annex (D);
            return BA;
         end if;
         D := AIN.Next_Node (D);
      end loop;

      raise Program_Error;

      return No_Node;
   end Get_Behavior_Specification;

   -------------------
   -- Map_Used_Type --
   -------------------

   function Map_Used_Type (N : Node_Id) return Node_Id
   is
      Data_Instance : Node_Id;
   begin
      Data_Instance := AAN.Default_Instance (N);

      if No (AIN.Backend_Node (AIN.Identifier (Data_Instance))) then
         Ocarina.Backends.C_Common.Types.Header_File.Visit (Data_Instance);
      end if;

      return Map_C_Data_Type_Designator (Data_Instance);

   end Map_Used_Type;

   ------------------------------
   -- Map_C_Behavior_Variables --
   ------------------------------

   procedure Map_C_Behavior_Variables (S            : Node_Id;
                                       Declarations : List_Id)
   is
      BA, P, T      : Node_Id;
   begin

      BA := Get_Behavior_Specification (S);
      if not BANu.Is_Empty (BATN.Variables (BA)) then
         P := BATN.First_Node (BATN.Variables (BA));
         loop
            T := BATN.First_Node (BATN.Identifiers (P));
            loop
               if Present (BATN.Corresponding_Declaration
                           (BATN.Classifier_Ref (P)))
                 and then Present (AAN.Default_Instance
                                   (BATN.Corresponding_Declaration
                                      (BATN.Classifier_Ref (P))))
               then

                  CTU.Append_Node_To_List
                    (CTU.Make_Variable_Declaration
                       (Defining_Identifier => CTU.Make_Defining_Identifier
                            (BATN.Display_Name (T)),
                        Used_Type =>  Map_Used_Type
                          (BATN.Corresponding_Declaration
                               (BATN.Classifier_Ref (P)))),
                     Declarations);

               else
                  if not BANu.Is_Empty (BATN.Package_Name
                                        (BATN.Classifier_Ref (P)))
                  then
                     Display_Error
                       (" The mapping of BA variable type ("
                        & Get_Name_String (BATN.Display_Name
                          (BATN.Full_Identifier (BATN.Classifier_Ref (P))))
                        & ") at " & Image (BATN.Loc (BATN.Classifier_Ref (P)))
                        & ", is not yet supported",
                        Fatal => True);
                  else
                     if Present (BATN.Component_Impl (BATN.Classifier_Ref (P)))
                     then
                        Display_Error
                          (" The mapping of BA variable type ("
                           & Get_Name_String
                             (Standard.Utils.Remove_Prefix_From_Name
                                  ("%ba%", BATN.Name (BATN.Component_Type
                                   (BATN.Classifier_Ref (P)))))
                           & "."
                           & Get_Name_String
                             (Standard.Utils.Remove_Prefix_From_Name
                                  ("%ba%", BATN.Name (BATN.Component_Impl
                                   (BATN.Classifier_Ref (P)))))
                           & ") at "
                           & Image (BATN.Loc (BATN.Classifier_Ref (P)))
                           & ", is not yet supported",
                           Fatal => True);
                     else
                        Display_Error
                          (" The mapping of BA variable type ("
                           & Get_Name_String
                             (Standard.Utils.Remove_Prefix_From_Name
                                  ("%ba%", BATN.Name (BATN.Component_Type
                                   (BATN.Classifier_Ref (P)))))
                           & ") at "
                           & Image (BATN.Loc (BATN.Classifier_Ref (P)))
                           & ", is not yet supported",
                           Fatal => True);
                     end if;
                  end if;
               end if;

               T := BATN.Next_Node (T);
               exit when No (T);
            end loop;
            P := BATN.Next_Node (P);
            exit when No (P);
         end loop;
      end if;

   end Map_C_Behavior_Variables;

   --------------------------------
   -- Map_C_Behavior_Transitions --
   --------------------------------

   procedure Map_C_Behavior_Transitions (S            : Node_Id;
                                         Declarations : List_Id;
                                         Statements   :  List_Id)
   is
      BA                 : Node_Id;
      behav_transition   : Node_Id;
      Transition_Node    : Node_Id;
   begin

      BA := Get_Behavior_Specification (S);

      if not BANu.Is_Empty (BATN.Transitions (BA)) then

         behav_transition := BATN.First_Node (BATN.Transitions (BA));
         Transition_Node := BATN.Transition (behav_transition);

         if BANu.Length (BATN.Transitions (BA)) = 1 then
            if AINU.Is_Thread (S) then
               Map_C_Behavior_Variables (S, Declarations);
            end if;

            if Present (BATN.Behavior_Action_Block (Transition_Node)) then
               if BATN.Kind (Transition_Node) =
                 BATN.K_Execution_Behavior_Transition
               then
                  --  For an AADL subprogram or thread with BA that includes
                  --  a unique transition, the Behavior_Action_Block
                  --  of this transition is mapped into C-statements.
                  Map_C_Behavior_Action_Block
                    (BATN.Behavior_Action_Block (Transition_Node),
                     S, Declarations, Statements);

                  if AINU.Is_Thread (S) then
                     Map_C_Implementation_of_BA_Body_Function
                       (S, Declarations, Statements);
                  end if;

               else
                  --  i.e. Kind (Transition_Node) = K_Mode_Transition
                  --  We do not support Mode transition in a subprogram
                  --
                  Display_Error
                    ("Mode Transition is not supported",
                     Fatal => True);
               end if;
            end if;
         else
            --  For an AADL subprogram with BA, many transitions
            --  are not supported.
            if AINU.Is_Subprogram (S) then
               Display_Error
                 ("Many transitions in the BA of a"
                  & "subprogram are not supported",
                  Fatal => True);
            elsif AINU.Is_Thread (S) then
               if Get_Thread_Dispatch_Protocol (S) = Thread_Periodic or else
                 Get_Thread_Dispatch_Protocol (S) = Thread_Sporadic
               then
                  Map_C_Many_Transitions_Of_A_Thread
                    (S, Declarations, Statements);
               else
                  Display_Error
                    ("The mapping of many BA transitions is only "
                     & " supported of periodic or sporadic threads",
                     Fatal => True);
               end if;
            end if;
         end if;

      end if;

   end Map_C_Behavior_Transitions;

   ----------------------------------------
   -- Compute_Nb_On_Dispatch_Transitions --
   -----------------------------------------

   function Compute_Nb_On_Dispatch_Transitions
     (S : Node_Id) return Unsigned_Long_Long
   is
      BA                        : Node_Id;
      behav_transition          : Node_Id;
      Transition_Node           : Node_Id;
      Nb_Dispatch_Transitions   : Unsigned_Long_Long;
   begin

      BA := Get_Behavior_Specification (S);

      Nb_Dispatch_Transitions := 0;

      --  /** compute the number of all « on dispatch » transitions
      --  that stems from the state "List_Node"
      --  i.e. have « BATN.Display_Name (List_Node) » as source state.
      --
      if not BANu.Is_Empty (BATN.Transitions (BA)) then
         Behav_Transition := BATN.First_Node (BATN.Transitions (BA));
         while Present (Behav_Transition) loop
            Transition_Node := BATN.Transition (Behav_Transition);

            if BATN.Kind (Transition_Node) =
              BATN.K_Execution_Behavior_Transition
              and then
                Present (BATN.Behavior_Condition (Transition_Node))
                and then
                  Present (BATN.Condition
                           (BATN.Behavior_Condition
                              (Transition_Node)))
              and then
                BATN.Kind
                  (BATN.Condition
                     (Behavior_Condition (Transition_Node)))
                = BATN.K_Dispatch_Condition_Thread
            then
               Nb_Dispatch_Transitions := Nb_Dispatch_Transitions + 1;
            end if;
            Behav_Transition := BATN.Next_Node (Behav_Transition);
         end loop;
      end if;

      return Nb_Dispatch_Transitions;

   end Compute_Nb_On_Dispatch_Transitions;

   ---------------------------------------------------------
   -- Compute_Max_Dispatch_Transitions_Per_Complete_State --
   ---------------------------------------------------------

   function Compute_Max_Dispatch_Transitions_Per_Complete_State
     (S : Node_Id) return Unsigned_Long_Long
   is
      BA                        : Node_Id;
      State                     : Node_Id;
      List_Node                 : Node_Id;
      behav_transition          : Node_Id;
      Transition_Node           : Node_Id;
      Source                    : Node_Id;
      Max_Dispatch_Transitions  : Unsigned_Long_Long := 0;
      Nb_Dispatch_Transitions   : Unsigned_Long_Long;
   begin

      BA := Get_Behavior_Specification (S);

      State := BATN.First_Node (BATN.States (BA));

      while Present (State) loop
         if Behavior_State_Kind'Val (BATN.State_Kind (State))
           = BSK_Initial_Complete
           or else Behavior_State_Kind'Val (BATN.State_Kind (State))
             = BSK_Initial_Complete_Final
           or else Behavior_State_Kind'Val (BATN.State_Kind (State))
             = BSK_Complete
           or else Behavior_State_Kind'Val (BATN.State_Kind (State))
             = BSK_Complete_Final
         then
            List_Node := BATN.First_Node (BATN.Identifiers (State));

            while Present (List_Node) loop

               Nb_Dispatch_Transitions := 0;

               --  /** compute the number of all « on dispatch » transitions
               --  that stems from the state "List_Node"
               --  i.e. have « BATN.Display_Name (List_Node) » as source state.
               --
               if not BANu.Is_Empty (BATN.Transitions (BA)) then
                  Behav_Transition := BATN.First_Node (BATN.Transitions (BA));
                  while Present (Behav_Transition) loop
                     Transition_Node := BATN.Transition (Behav_Transition);

                     if BATN.Kind (Transition_Node) =
                       BATN.K_Execution_Behavior_Transition
                       and then
                         Present (BATN.Behavior_Condition (Transition_Node))
                         and then
                           Present (BATN.Condition
                                    (BATN.Behavior_Condition
                                       (Transition_Node)))
                       and then
                         BATN.Kind
                           (BATN.Condition
                              (Behavior_Condition (Transition_Node)))
                         = BATN.K_Dispatch_Condition_Thread
                     then
                        if BANu.Length (BATN.Sources (Transition_Node)) = 1
                        then
                           Source := BATN.First_Node
                             (BATN.Sources (Transition_Node));
                           if  (Standard.Utils.To_Upper
                                (BATN.Display_Name (List_Node)) =
                                  Standard.Utils.To_Upper
                                    (BATN.Display_Name (Source)))
                           then
                              Nb_Dispatch_Transitions :=
                                Nb_Dispatch_Transitions + 1;
                           end if;
                        end if;
                     end if;
                     Behav_Transition := BATN.Next_Node (Behav_Transition);
                  end loop;
               end if;

               if Nb_Dispatch_Transitions > Max_Dispatch_Transitions then
                  Max_Dispatch_Transitions := Nb_Dispatch_Transitions;
               end if;

               List_Node := BATN.Next_Node (List_Node);
            end loop;
         end if;

         State := BATN.Next_Node (State);
      end loop;

      return Max_Dispatch_Transitions;

   end Compute_Max_Dispatch_Transitions_Per_Complete_State;

   -----------------------------------------------------------
   -- Compute_Max_Dispatch_Triggers_Per_Dispatch_Transition --
   -----------------------------------------------------------

   function Compute_Max_Dispatch_Triggers_Per_Dispatch_Transition
     (S : Node_Id) return Unsigned_Long_Long
   is
      BA                        : Node_Id;
      behav_transition          : Node_Id;
      Node                      : Node_Id;
      Dispatch_Conjunction_Node : Node_Id;
      Dipatch_Trigger_Event     : Node_Id;
      Max_Dispatch_Triggers     : Unsigned_Long_Long := 0;
      Nb_Dispatch_Triggers      : Unsigned_Long_Long;
   begin

      BA := Get_Behavior_Specification (S);

      if not BANu.Is_Empty (BATN.Transitions (BA)) then

         behav_transition := BATN.First_Node (BATN.Transitions (BA));

         while Present (Behav_Transition) loop

            Node := BATN.Transition (Behav_Transition);

            if BATN.Kind (Node) = BATN.K_Execution_Behavior_Transition
              and then Present (BATN.Behavior_Condition (Node))
              and then Present (BATN.Condition
                                (BATN.Behavior_Condition (Node)))
              and then
                BATN.Kind (BATN.Condition (Behavior_Condition (Node)))
              = BATN.K_Dispatch_Condition_Thread
              and then Present
                (BATN.Dispatch_Trigger_Condition
                   (BATN.Condition
                      (BATN.Behavior_Condition (Node))))
              and then not BANu.Is_Empty
                (BATN.Dispatch_Conjunction
                   (BATN.Dispatch_Trigger_Condition
                      (BATN.Condition
                           (BATN.Behavior_Condition (Node)))))
            then
               Dispatch_Conjunction_Node :=
                 BATN.First_Node
                   (Dispatch_Conjunction
                      (Dispatch_Trigger_Condition (BATN.Condition
                       (BATN.Behavior_Condition (Node)))));

               while Present (Dispatch_Conjunction_Node) loop

                  if not BANu.Is_Empty
                    (Dispatch_Triggers (Dispatch_Conjunction_Node))
                  then
                     Dipatch_Trigger_Event := BATN.First_Node
                       (Dispatch_Triggers (Dispatch_Conjunction_Node));

                     Nb_Dispatch_Triggers := 0;

                     while Present (Dipatch_Trigger_Event) loop

                        Nb_Dispatch_Triggers := Nb_Dispatch_Triggers + 1;

                        Dipatch_Trigger_Event := BATN.Next_Node
                          (Dipatch_Trigger_Event);
                     end loop;

                     if Nb_Dispatch_Triggers > Max_Dispatch_Triggers then
                        Max_Dispatch_Triggers := Nb_Dispatch_Triggers;
                     end if;

                  end if;

                  Dispatch_Conjunction_Node := BATN.Next_Node
                    (Dispatch_Conjunction_Node);
               end loop;

            end if;
            behav_transition := BATN.Next_Node (behav_transition);
         end loop;
      end if;

      return Max_Dispatch_Triggers;

   end Compute_Max_Dispatch_Triggers_Per_Dispatch_Transition;

   --------------------------------------
   -- Create_Enum_Type_Of_States_Names --
   --------------------------------------

   procedure Create_Enum_Type_Of_States_Names (S : Node_Id)
   is
      BA                         : Node_Id;
      State, List_Node           : Node_Id;
      N                          : Node_Id;
      State_Enumerator_List      : List_Id;
      State_Identifier           : Unsigned_Long_Long;
      Thread_Instance_Name       : Name_Id;
      E                          : constant Node_Id :=
        AIN.Parent_Subcomponent (S);
   begin

      BA := Get_Behavior_Specification (S);

      --  1) Create an enumeration type declaration
      --  called __po_hi_<<thread_instance_name>>_state_name_t
      --  that enumerates all states in the BA of the thread
      --  typedef enum
      --  {
      --     s0, s1, s2, s3
      --  } __po_hi__<<thread_instance_name>>__state_name_t;

      State_Enumerator_List := New_List (CTN.K_Enumeration_Literals);

      State := BATN.First_Node (BATN.States (BA));
      State_Identifier := 0;

      while Present (State) loop

         if not BANu.Is_Empty (BATN.Identifiers (State)) then
            List_Node := BATN.First_Node (BATN.Identifiers (State));

            while Present (List_Node) loop

               N := CTU.Make_Defining_Identifier
                 (BATN.Display_Name (List_Node));

--                 N := Make_Expression
--                   (CTU.Make_Defining_Identifier
--                      (BATN.Display_Name (List_Node)),
--                    Op_Equal,
--                    Make_Literal
--                      (CV.New_Int_Value (State_Identifier, 0, 10)));

               Append_Node_To_List (N, State_Enumerator_List);
               State_Identifier := State_Identifier + 1;

               List_Node := BATN.Next_Node (List_Node);
            end loop;
         end if;

         State := BATN.Next_Node (State);
      end loop;

      Thread_Instance_Name := Map_Thread_Port_Variable_Name (S);
      N :=
        Message_Comment
          ("For each state in the BA of the thread instance << " &
             Get_Name_String (Thread_Instance_Name) &
             " >> add an enumerator");
      Append_Node_To_List (N, CTN.Declarations (Current_File));

      N :=
        Make_Full_Type_Declaration
          (Defining_Identifier => Make_Defining_Identifier
             (Map_C_Variable_Name (E, State_Name_T => True)),
           Type_Definition => Make_Enum_Aggregate (State_Enumerator_List));
      Append_Node_To_List (N, CTN.Declarations (Current_File));

   end Create_Enum_Type_Of_States_Names;

   -----------------------
   -- Create_State_Type --
   -----------------------

   procedure Create_State_Type (S : Node_Id)
   is
      N                          : Node_Id;
      State_Struct               : List_Id;
      E                          : constant Node_Id :=
        AIN.Parent_Subcomponent (S);
   begin

      --  3)
      --  typedef struct
      --  {
      --    __po_hi_<<thread_instance_name>>_state_name_t name;
      --    __po_hi_state_kind_t kind;
      --  } __po_hi_<<thread_instance_name>>_state_t;

      State_Struct := New_List (CTN.K_Enumeration_Literals);

      N :=
        Make_Member_Declaration
          (Defining_Identifier => Make_Defining_Identifier
             (MN (M_Name)),
           Used_Type           => Make_Defining_Identifier
             (Map_C_Variable_Name (E, State_Name_T => True)));
      Append_Node_To_List (N, State_Struct);

      N :=
        Make_Member_Declaration
          (Defining_Identifier => Make_Defining_Identifier
             (MN (M_kind)),
           Used_Type           => RE (RE_State_Kind_T));
      Append_Node_To_List (N, State_Struct);

      if Get_Thread_Dispatch_Protocol (S) = Thread_Sporadic
        and then Compute_Nb_On_Dispatch_Transitions (S) > 1
      then
         N :=
           Make_Member_Declaration
             (Defining_Identifier => Make_Defining_Identifier
                (MN (M_States_Attributes)),
                  Used_Type           => RE (RE_Ba_Automata_State_T));
         Append_Node_To_List (N, State_Struct);
      end if;

      N :=
        Make_Full_Type_Declaration
          (Defining_Identifier => Make_Defining_Identifier
             (Map_C_Variable_Name (E, State_T => True)),
           Type_Definition     =>
             Make_Struct_Aggregate (Members => State_Struct));
      Append_Node_To_List (N, CTN.Declarations (Current_File));

   end Create_State_Type;

   -----------------------
   -- Compute_Nb_States --
   -----------------------

   function Compute_Nb_States
     (S            : Node_Id) return Unsigned_Long_Long
   is
      BA                         : Node_Id;
      State, List_Node           : Node_Id;
      Nb_States                  : Unsigned_Long_Long;
   begin

      BA := Get_Behavior_Specification (S);

      State := BATN.First_Node (BATN.States (BA));
      Nb_States := 0;

      while Present (State) loop

         if not BANu.Is_Empty (BATN.Identifiers (State)) then
            List_Node := BATN.First_Node (BATN.Identifiers (State));

            while Present (List_Node) loop

               Nb_States := Nb_States + 1;

               List_Node := BATN.Next_Node (List_Node);
            end loop;
         end if;

         State := BATN.Next_Node (State);
      end loop;

      return Nb_States;
   end Compute_Nb_States;

   ------------------------------
   -- Map_BA_States_To_C_Types --
   ------------------------------

   procedure Map_BA_States_To_C_Types
     (S            : Node_Id)
   is
      N                          : Node_Id;
      Nb_States                  : constant Unsigned_Long_Long :=
        Compute_Nb_States (S);
      E                          : constant Node_Id :=
        AIN.Parent_Subcomponent (S);
   begin

      --  4)
      --  #define __po_hi_<<thread_instance_name>>_nb_states <<Nb_States>>

      N :=
        Make_Define_Statement
          (Defining_Identifier => Make_Defining_Identifier
             (Map_C_Define_Name (E, Nb_States => True)),
           Value               =>
             Make_Literal
               (CV.New_Int_Value (Nb_States, 1, 10)));
      Append_Node_To_List (N, CTN.Declarations (Current_File));

      --  5)
      --  __po_hi_<<thread_instance_name>>_state_t
      --       __po_hi_<<thread_instance_name>>_states_array
      --               [__po_hi_<<thread_instance_name>>_nb_states];

      N :=
        Make_Variable_Declaration
          (Defining_Identifier =>
             Make_Array_Declaration
               (Defining_Identifier =>
                      Make_Defining_Identifier
                  (Map_C_Variable_Name (E, States_Array => True)),
                Array_Size =>
                  Make_Defining_Identifier
                    (Map_C_Define_Name (E, Nb_States => True))),
           Used_Type =>
             Make_Defining_Identifier
               (Map_C_Variable_Name (E, State_T => True)));
      Append_Node_To_List (N, CTN.Declarations (Current_File));

      --  6)
      --  __po_hi_<<thread_instance_name>>_state_t
      --       __po_hi_<<thread_instance_name>>_current_state
      N :=
        Make_Variable_Declaration
          (Defining_Identifier =>
                Make_Defining_Identifier
                  (Map_C_Variable_Name (E, Current_State => True)),
           Used_Type =>
             Make_Defining_Identifier
               (Map_C_Variable_Name (E, State_T => True)));
      Append_Node_To_List (N, CTN.Declarations (Current_File));

   end Map_BA_States_To_C_Types;

   ---------------------------
   -- Map_C_State_Kind_Name --
   ---------------------------

   function Map_C_State_Kind_Name
     (State_Kind : Ocarina.Types.Byte) return Node_Id
   is
      Result : Node_Id;
   begin
      case Behavior_State_Kind'Val (State_Kind) is
         when BSK_Initial                => Result := RE (RE_Initial);
         when BSK_Initial_Complete       =>
            Result := RE (RE_Initial_Complete);
         when BSK_Initial_Complete_Final =>
            Result := RE (RE_Initial_Complete_Final);
         when BSK_Initial_Final          =>
            Result := RE (RE_Initial_Final);
         when BSK_Complete               => Result := RE (RE_Complete);
         when BSK_Complete_Final         =>
            Result := RE (RE_Complete_Final);
         when BSK_Final                  => Result := RE (RE_Final);
         when BSK_No_Kind                => Result := RE (RE_Execution);
         when others                     => raise Program_Error;
      end case;
      return Result;
   end Map_C_State_Kind_Name;

   -------------------------------------------------------------------
   -- Max_Dispatch_Triggers_Per_Trans_From_A_Specific_Complete_Stat --
   -------------------------------------------------------------------

   function Max_Dispatch_Triggers_Per_Trans_From_A_Specific_Complete_Stat
     (BA             : Node_Id;
      Complete_State : Node_Id) return Unsigned_Long_Long
   is
      Nb_Dispatch_Triggers_Per_Trans  : Unsigned_Long_Long := 0;
      Max_Dispatch_Triggers_Per_Trans : Unsigned_Long_Long := 0;
      behav_transition                : Node_Id;
      Node                            : Node_Id;
      Source                          : Node_Id;
      Dispatch_Conjunction_Node       : Node_Id;
      Dispatch_Trigger_Event          : Node_Id;
   begin

      --  /** compute the number of all « dispatch triggers » of all
      --  on dispatch transitions transitions that stem from the state
      --  "Complete_State"
      --  i.e. have « BATN.Display_Name (Complete_State) » as source state.
      --
      if not BANu.Is_Empty (BATN.Transitions (BA)) then
         Behav_Transition := BATN.First_Node (BATN.Transitions (BA));
         while Present (Behav_Transition) loop
            Node := BATN.Transition (Behav_Transition);

            if BATN.Kind (Node) =
              BATN.K_Execution_Behavior_Transition
              and then
                Present (BATN.Behavior_Condition (Node))
                and then
                  Present (BATN.Condition
                           (BATN.Behavior_Condition
                              (Node)))
              and then
                BATN.Kind
                  (BATN.Condition
                     (Behavior_Condition (Node)))
                = BATN.K_Dispatch_Condition_Thread
            then
               if BANu.Length (BATN.Sources (Node)) = 1
               then
                  Source := BATN.First_Node
                    (BATN.Sources (Node));
                  if  (Standard.Utils.To_Upper
                       (BATN.Display_Name (Complete_State)) =
                         Standard.Utils.To_Upper
                           (BATN.Display_Name (Source)))
                  then
                     Nb_Dispatch_Triggers_Per_Trans := 0;
                     if Present (BATN.Dispatch_Trigger_Condition
                                 (BATN.Condition
                                    (BATN.Behavior_Condition (Node))))
                       and then not BANu.Is_Empty
                         (BATN.Dispatch_Conjunction
                            (BATN.Dispatch_Trigger_Condition
                               (BATN.Condition
                                    (BATN.Behavior_Condition (Node)))))
                     then
                        if BANu.Length
                          (Dispatch_Conjunction
                             (Dispatch_Trigger_Condition (BATN.Condition
                              (BATN.Behavior_Condition (Node))))) = 1
                        then

                           Dispatch_Conjunction_Node :=
                             BATN.First_Node
                               (Dispatch_Conjunction
                                  (Dispatch_Trigger_Condition (BATN.Condition
                                   (BATN.Behavior_Condition (Node)))));

                           if not BANu.Is_Empty
                             (Dispatch_Triggers (Dispatch_Conjunction_Node))
                           then
                              Dispatch_Trigger_Event := BATN.First_Node
                                (Dispatch_Triggers
                                   (Dispatch_Conjunction_Node));

                              while Present (Dispatch_Trigger_Event) loop
                                 Nb_Dispatch_Triggers_Per_Trans :=
                                   Nb_Dispatch_Triggers_Per_Trans + 1;
                                 Dispatch_Trigger_Event :=
                                   BATN.Next_Node (Dispatch_Trigger_Event);
                              end loop;

                              if Nb_Dispatch_Triggers_Per_Trans >
                                Max_Dispatch_Triggers_Per_Trans
                              then
                                 Max_Dispatch_Triggers_Per_Trans :=
                                   Nb_Dispatch_Triggers_Per_Trans;
                              end if;
                           end if;
                        else
                           Display_Error
                             ("The code generation for many "
                              & "Dispatch Conjunction is not yet supported.",
                              Fatal => True);
                        end if;
                     end if;
                  end if;
               end if;
            end if;
            Behav_Transition := BATN.Next_Node (Behav_Transition);
         end loop;
      end if;
      return Max_Dispatch_Triggers_Per_Trans;
   end Max_Dispatch_Triggers_Per_Trans_From_A_Specific_Complete_Stat;

   -----------------------------------------------------
   -- Fill_Dispatch_Triggers_Of_All_Transitions_Array --
   -----------------------------------------------------

   procedure Fill_Dispatch_Triggers_Of_All_Transitions_Array
     (S               : Node_Id;
      BA              : Node_Id;
      Complete_State  : Node_Id;
      Index_Comp_Stat : Unsigned_Long_Long;
      WStatements     : List_Id)
   is
      E                              : constant Node_Id :=
        AIN.Parent_Subcomponent (S);
      Index_Dispatch_Triggers        : Unsigned_Long_Long := -1;
      behav_transition               : Node_Id;
      Node                           : Node_Id;
      Source                         : Node_Id;
      Dispatch_Conjunction_Node      : Node_Id;
      Dispatch_Trigger_Event         : Node_Id;
      N                              : Node_Id;
   begin
      --  6) fill the array __po_hi_consumer_states_array[0].
      --  state_attributes.dispatch_triggers_of_all_transitions[0]
      --  = LOCAL_PORT (consumer, c1);
      if not BANu.Is_Empty (BATN.Transitions (BA)) then
         Behav_Transition := BATN.First_Node (BATN.Transitions (BA));
         while Present (Behav_Transition) loop
            Node := BATN.Transition (Behav_Transition);

            if BATN.Kind (Node) =
              BATN.K_Execution_Behavior_Transition
              and then
                Present (BATN.Behavior_Condition (Node))
                and then
                  Present (BATN.Condition
                           (BATN.Behavior_Condition
                              (Node)))
              and then
                BATN.Kind
                  (BATN.Condition
                     (Behavior_Condition (Node)))
                = BATN.K_Dispatch_Condition_Thread
            then
               if BANu.Length (BATN.Sources (Node)) = 1
               then
                  Source := BATN.First_Node
                    (BATN.Sources (Node));
                  if  (Standard.Utils.To_Upper
                       (BATN.Display_Name (Complete_State)) =
                         Standard.Utils.To_Upper
                           (BATN.Display_Name (Source)))
                  then
                     if Present (BATN.Dispatch_Trigger_Condition
                                 (BATN.Condition
                                    (BATN.Behavior_Condition (Node))))
                       and then not BANu.Is_Empty
                         (BATN.Dispatch_Conjunction
                            (BATN.Dispatch_Trigger_Condition
                               (BATN.Condition
                                    (BATN.Behavior_Condition (Node)))))
                     then
                        if BANu.Length
                          (Dispatch_Conjunction
                             (Dispatch_Trigger_Condition (BATN.Condition
                              (BATN.Behavior_Condition (Node))))) = 1
                        then

                           Dispatch_Conjunction_Node :=
                             BATN.First_Node
                               (Dispatch_Conjunction
                                  (Dispatch_Trigger_Condition (BATN.Condition
                                   (BATN.Behavior_Condition (Node)))));

                           if not BANu.Is_Empty
                             (Dispatch_Triggers (Dispatch_Conjunction_Node))
                           then
                              Dispatch_Trigger_Event := BATN.First_Node
                                (Dispatch_Triggers
                                   (Dispatch_Conjunction_Node));

                              while Present (Dispatch_Trigger_Event) loop
                                 Index_Dispatch_Triggers :=
                                   Index_Dispatch_Triggers + 1;

                                 N := Make_Assignment_Statement
                                (Variable_Identifier => Make_Member_Designator
                                 (Defining_Identifier =>
                                   Make_Member_Designator
                                    (Defining_Identifier =>
                                     Make_Array_Declaration
                                      (Defining_Identifier =>
                                       Make_Defining_Identifier
                                 (MN (M_Dispatch_Triggers_Of_All_Transitions)),
                                       Array_Size => Make_Literal
                                         (CV.New_Int_Value
                                           (Index_Dispatch_Triggers, 0, 10))),
                                   Aggregate_Name  => Make_Defining_Identifier
                                      (MN (M_States_Attributes))),
                                  Aggregate_Name      => Make_Array_Declaration
                                   (Defining_Identifier =>
                                     Make_Defining_Identifier
                               (Map_C_Variable_Name (E, States_Array => True)),
                                       Array_Size => Make_Literal
                                      (CV.New_Int_Value
                                              (Index_Comp_Stat, 0, 10)))),
                                 Expression  =>
                                   Make_Call_Profile
                                    (RE (RE_Local_Port),
                                      Make_List_Id
                                      (Make_Defining_Identifier
                                        (Map_Thread_Port_Variable_Name (S)),
                                       Make_Defining_Identifier
                                           (BATN.Display_Name
                                                (Dispatch_Trigger_Event)))));

                                 CTU.Append_Node_To_List (N, WStatements);

                                 Dispatch_Trigger_Event :=
                                   BATN.Next_Node (Dispatch_Trigger_Event);
                              end loop;
                           end if;
                        else
                           Display_Error
                             ("The code generation for many "
                              & "Dispatch Conjunction is not yet supported.",
                              Fatal => True);
                        end if;
                     end if;
                  end if;
               end if;
            end if;
            Behav_Transition := BATN.Next_Node (Behav_Transition);
         end loop;
      end if;

   end Fill_Dispatch_Triggers_Of_All_Transitions_Array;

   --------------------------------------------------------
   -- Fill_Nb_Dispatch_Triggers_Of_Each_Transition_Array --
   --------------------------------------------------------

   procedure Fill_Nb_Dispatch_Triggers_Of_Each_Transition_Array
     (S               : Node_Id;
      BA              : Node_Id;
      Complete_State  : Node_Id;
      Index_Comp_Stat : Unsigned_Long_Long;
      WStatements     : List_Id)
   is
      E                              : constant Node_Id :=
        AIN.Parent_Subcomponent (S);
      Index_Transition               : Unsigned_Long_Long := -1;
      Nb_Dispatch_Triggers_Per_Trans : Unsigned_Long_Long := 0;
      behav_transition               : Node_Id;
      Node                           : Node_Id;
      Source                         : Node_Id;
      Dispatch_Conjunction_Node      : Node_Id;
      Dispatch_Trigger_Event         : Node_Id;
      N                              : Node_Id;
   begin

      if not BANu.Is_Empty (BATN.Transitions (BA)) then
         Behav_Transition := BATN.First_Node (BATN.Transitions (BA));
         while Present (Behav_Transition) loop
            Node := BATN.Transition (Behav_Transition);

            if BATN.Kind (Node) =
              BATN.K_Execution_Behavior_Transition
              and then
                Present (BATN.Behavior_Condition (Node))
                and then
                  Present (BATN.Condition
                           (BATN.Behavior_Condition
                              (Node)))
              and then
                BATN.Kind
                  (BATN.Condition
                     (Behavior_Condition (Node)))
                = BATN.K_Dispatch_Condition_Thread
            then
               if BANu.Length (BATN.Sources (Node)) = 1
               then
                  Source := BATN.First_Node
                    (BATN.Sources (Node));
                  if  (Standard.Utils.To_Upper
                       (BATN.Display_Name (Complete_State)) =
                         Standard.Utils.To_Upper
                           (BATN.Display_Name (Source)))
                  then
                     Nb_Dispatch_Triggers_Per_Trans := 0;
                     Index_Transition := Index_Transition + 1;
                     if Present (BATN.Dispatch_Trigger_Condition
                                 (BATN.Condition
                                    (BATN.Behavior_Condition (Node))))
                       and then not BANu.Is_Empty
                         (BATN.Dispatch_Conjunction
                            (BATN.Dispatch_Trigger_Condition
                               (BATN.Condition
                                    (BATN.Behavior_Condition (Node)))))
                     then
                        if BANu.Length
                          (Dispatch_Conjunction
                             (Dispatch_Trigger_Condition (BATN.Condition
                              (BATN.Behavior_Condition (Node))))) = 1
                        then

                           Dispatch_Conjunction_Node :=
                             BATN.First_Node
                               (Dispatch_Conjunction
                                  (Dispatch_Trigger_Condition (BATN.Condition
                                   (BATN.Behavior_Condition (Node)))));

                           if not BANu.Is_Empty
                             (Dispatch_Triggers (Dispatch_Conjunction_Node))
                           then
                              Dispatch_Trigger_Event := BATN.First_Node
                                (Dispatch_Triggers
                                   (Dispatch_Conjunction_Node));

                              while Present (Dispatch_Trigger_Event) loop
                                 Nb_Dispatch_Triggers_Per_Trans :=
                                   Nb_Dispatch_Triggers_Per_Trans + 1;
                                 Dispatch_Trigger_Event :=
                                   BATN.Next_Node (Dispatch_Trigger_Event);
                              end loop;

                              N := Make_Assignment_Statement
                                (Variable_Identifier => Make_Member_Designator
                                 (Defining_Identifier =>
                                   Make_Member_Designator
                                    (Defining_Identifier =>
                                     Make_Array_Declaration
                                      (Defining_Identifier =>
                                       Make_Defining_Identifier
                              (MN (M_Nb_Dispatch_Triggers_Of_Each_Transition)),
                                       Array_Size => Make_Literal
                                         (CV.New_Int_Value
                                              (Index_Transition, 0, 10))),
                                   Aggregate_Name  => Make_Defining_Identifier
                                      (MN (M_States_Attributes))),
                                  Aggregate_Name      => Make_Array_Declaration
                                   (Defining_Identifier =>
                                     Make_Defining_Identifier
                               (Map_C_Variable_Name (E, States_Array => True)),
                                       Array_Size => Make_Literal
                                      (CV.New_Int_Value
                                              (Index_Comp_Stat, 0, 10)))),
                                 Expression          =>
                                   Make_Literal
                                    (CV.New_Int_Value
                                     (Nb_Dispatch_Triggers_Per_Trans, 0, 10)));

                              CTU.Append_Node_To_List (N, WStatements);
                           end if;
                        else
                           Display_Error
                             ("The code generation for many "
                              & "Dispatch Conjunction is not yet supported.",
                              Fatal => True);
                        end if;
                     end if;
                  end if;
               end if;
            end if;
            Behav_Transition := BATN.Next_Node (Behav_Transition);
         end loop;
      end if;

   end Fill_Nb_Dispatch_Triggers_Of_Each_Transition_Array;

   ------------------------------------------------------------
   -- Nb_All_Dispatch_Triggers_From_A_Specific_Complete_Stat --
   ------------------------------------------------------------

   function Nb_All_Dispatch_Triggers_From_A_Specific_Complete_Stat
     (BA             : Node_Id;
      Complete_State : Node_Id) return Unsigned_Long_Long
   is
      Nb_All_Dispatch_Triggers  : Unsigned_Long_Long := 0;
      behav_transition          : Node_Id;
      Node                      : Node_Id;
      Source                    : Node_Id;
      Dispatch_Conjunction_Node : Node_Id;
      Dispatch_Trigger_Event    : Node_Id;
   begin

      --  /** compute the number of all « dispatch triggers » of all
      --  on dispatch transitions transitions that stem from the state
      --  "Complete_State"
      --  i.e. have « BATN.Display_Name (Complete_State) » as source state.
      --
      if not BANu.Is_Empty (BATN.Transitions (BA)) then
         Behav_Transition := BATN.First_Node (BATN.Transitions (BA));
         while Present (Behav_Transition) loop
            Node := BATN.Transition (Behav_Transition);

            if BATN.Kind (Node) =
              BATN.K_Execution_Behavior_Transition
              and then
                Present (BATN.Behavior_Condition (Node))
                and then
                  Present (BATN.Condition
                           (BATN.Behavior_Condition
                              (Node)))
              and then
                BATN.Kind
                  (BATN.Condition
                     (Behavior_Condition (Node)))
                = BATN.K_Dispatch_Condition_Thread
            then
               if BANu.Length (BATN.Sources (Node)) = 1
               then
                  Source := BATN.First_Node
                    (BATN.Sources (Node));
                  if  (Standard.Utils.To_Upper
                       (BATN.Display_Name (Complete_State)) =
                         Standard.Utils.To_Upper
                           (BATN.Display_Name (Source)))
                  then
                     if Present (BATN.Dispatch_Trigger_Condition
                                 (BATN.Condition
                                    (BATN.Behavior_Condition (Node))))
                       and then not BANu.Is_Empty
                         (BATN.Dispatch_Conjunction
                            (BATN.Dispatch_Trigger_Condition
                               (BATN.Condition
                                    (BATN.Behavior_Condition (Node)))))
                     then
                        if BANu.Length
                          (Dispatch_Conjunction
                             (Dispatch_Trigger_Condition (BATN.Condition
                              (BATN.Behavior_Condition (Node))))) = 1
                        then

                           Dispatch_Conjunction_Node :=
                             BATN.First_Node
                               (Dispatch_Conjunction
                                  (Dispatch_Trigger_Condition (BATN.Condition
                                   (BATN.Behavior_Condition (Node)))));

                           if not BANu.Is_Empty
                             (Dispatch_Triggers (Dispatch_Conjunction_Node))
                           then
                              Dispatch_Trigger_Event := BATN.First_Node
                                (Dispatch_Triggers
                                   (Dispatch_Conjunction_Node));

                              while Present (Dispatch_Trigger_Event) loop
                                 Nb_All_Dispatch_Triggers :=
                                   Nb_All_Dispatch_Triggers + 1;
                                 Dispatch_Trigger_Event :=
                                   BATN.Next_Node (Dispatch_Trigger_Event);
                              end loop;
                           end if;
                        else
                           Display_Error
                             ("The code generation for many "
                              & "Dispatch Conjunction is not yet supported.",
                              Fatal => True);
                        end if;
                     end if;
                  end if;
               end if;
            end if;
            Behav_Transition := BATN.Next_Node (Behav_Transition);
         end loop;
      end if;
      return Nb_All_Dispatch_Triggers;
   end Nb_All_Dispatch_Triggers_From_A_Specific_Complete_Stat;

   ------------------------------------------------------------
   -- Compute_Nb_Trans_Stemmed_From_A_Specific_Complete_Stat --
   ------------------------------------------------------------

   function Compute_Nb_Trans_Stemmed_From_A_Specific_Complete_Stat
     (BA             : Node_Id;
      Complete_State : Node_Id) return Unsigned_Long_Long
   is
      Nb_Dispatch_Transitions   : Unsigned_Long_Long := 0;
      behav_transition          : Node_Id;
      Transition_Node           : Node_Id;
      Source                    : Node_Id;
   begin

      --  /** compute the number of all « on dispatch » transitions
      --  that stems from the state "Complete_State"
      --  i.e. have « BATN.Display_Name (Complete_State) » as source state.
      --
      if not BANu.Is_Empty (BATN.Transitions (BA)) then
         Behav_Transition := BATN.First_Node (BATN.Transitions (BA));
         while Present (Behav_Transition) loop
            Transition_Node := BATN.Transition (Behav_Transition);

            if BATN.Kind (Transition_Node) =
              BATN.K_Execution_Behavior_Transition
              and then
                Present (BATN.Behavior_Condition (Transition_Node))
                and then
                  Present (BATN.Condition
                           (BATN.Behavior_Condition
                              (Transition_Node)))
              and then
                BATN.Kind
                  (BATN.Condition
                     (Behavior_Condition (Transition_Node)))
                = BATN.K_Dispatch_Condition_Thread
            then
               if BANu.Length (BATN.Sources (Transition_Node)) = 1
               then
                  Source := BATN.First_Node
                    (BATN.Sources (Transition_Node));
                  if  (Standard.Utils.To_Upper
                       (BATN.Display_Name (Complete_State)) =
                         Standard.Utils.To_Upper
                           (BATN.Display_Name (Source)))
                  then
                     Nb_Dispatch_Transitions :=
                       Nb_Dispatch_Transitions + 1;
                  end if;
               end if;
            end if;
            Behav_Transition := BATN.Next_Node (Behav_Transition);
         end loop;
      end if;
      return Nb_Dispatch_Transitions;
   end Compute_Nb_Trans_Stemmed_From_A_Specific_Complete_Stat;

   -----------------------------------------
   -- Make_Update_Next_Complete_State_Function --
   -----------------------------------------

   procedure Make_Update_Next_Complete_State_Function
     (S            : Node_Id)
   is
      N                          : Node_Id;
      E                          : constant Node_Id :=
        AIN.Parent_Subcomponent (S);
      WStatements : constant List_Id := New_List (CTN.K_Statement_List);
      Q : Name_Id;

   begin

      --  next_complete_state->nb_of_all_dispatch_events =
      --  __po_hi_consumer_current_state.state_attributes.
      --  nb_of_all_dispatch_events;
      --  next_complete_state->nb_transitions =
      --  __po_hi_consumer_current_state.state_attributes.nb_transitions;
      --
      --  for(int i=0;i<next_complete_state->nb_of_all_dispatch_events;i++)
      --  {
      --    next_complete_state->dispatch_triggers_of_all_transitions[i] =
      --     __po_hi_consumer_current_state.state_attributes.
      --       dispatch_triggers_of_all_transitions[i];
      --  }
      --  for(int i=0;i<next_complete_state->nb_transitions;i++)
      --  {
      --    next_complete_state->nb_dispatch_triggers_of_each_transition[i] =
      --      __po_hi_consumer_current_state.state_attributes.
      --        nb_dispatch_triggers_of_each_transition[i];
      --  }

      N := Make_Assignment_Statement
        (Variable_Identifier => Make_Member_Designator
           (Defining_Identifier => Make_Defining_Identifier
                (MN (M_Nb_Of_All_Dispatch_Events)),
            Aggregate_Name      => Make_Defining_Identifier
              (VN (V_Next_Complete_State)),
            Is_Pointer          => True),
         Expression  => Make_Member_Designator
           (Defining_Identifier => Make_Member_Designator
                (Defining_Identifier => Make_Defining_Identifier
                     (MN (M_Nb_Of_All_Dispatch_Events)),
                 Aggregate_Name      => Make_Defining_Identifier
                   (MN (M_States_Attributes))),
            Aggregate_Name      => Make_Defining_Identifier
              (Map_C_Variable_Name (E, Current_State => True))));

      CTU.Append_Node_To_List (N, WStatements);

      N := Make_Assignment_Statement
        (Variable_Identifier => Make_Member_Designator
           (Defining_Identifier => Make_Defining_Identifier
                (MN (M_Nb_Transitions)),
            Aggregate_Name      => Make_Defining_Identifier
              (VN (V_Next_Complete_State)),
            Is_Pointer          => True),
         Expression  => Make_Member_Designator
           (Defining_Identifier => Make_Member_Designator
                (Defining_Identifier => Make_Defining_Identifier
                     (MN (M_Nb_Transitions)),
                 Aggregate_Name      => Make_Defining_Identifier
                   (MN (M_States_Attributes))),
            Aggregate_Name      => Make_Defining_Identifier
              (Map_C_Variable_Name (E, Current_State => True))));

      CTU.Append_Node_To_List (N, WStatements);

      Set_Str_To_Name_Buffer ("i");
      Q := Name_Find;

      N := CTU.Make_For_Statement
        (Pre_Cond   => Make_Variable_Declaration
           (Defining_Identifier => Make_Defining_Identifier (Q),
            Used_Type           => Make_Defining_Identifier (TN (T_Int)),
            Value               => Make_Literal (CV.New_Int_Value (0, 0, 10))),
         Condition  => CTU.Make_Expression
           (Left_Expr  => Make_Defining_Identifier
                (Q),
            Operator   => CTU.Op_Less,
            Right_Expr => Make_Member_Designator
              (Defining_Identifier => Make_Defining_Identifier
                   (MN (M_Nb_Of_All_Dispatch_Events)),
               Aggregate_Name      => Make_Defining_Identifier
                 (VN (V_Next_Complete_State)),
               Is_Pointer          => True)),
         Post_Cond  =>  CTU.Make_Expression
           (Left_Expr  => Make_Defining_Identifier (Q),
            Operator   => CTU.Op_Plus_Plus,
            Right_Expr => No_Node),
         Statements =>
           Make_List_Id
             (Make_Assignment_Statement
                  (Variable_Identifier => Make_Member_Designator
                     (Defining_Identifier => Make_Array_Declaration
                        (Defining_Identifier => Make_Defining_Identifier
                             (MN (M_Dispatch_Triggers_Of_All_Transitions)),
                         Array_Size => Make_Defining_Identifier (Q)),
                      Aggregate_Name      => Make_Defining_Identifier
                        (VN (V_Next_Complete_State)),
                      Is_Pointer          => True),
                   Expression  => Make_Member_Designator
                     (Defining_Identifier => Make_Member_Designator
                        (Defining_Identifier => Make_Array_Declaration
                             (Defining_Identifier => Make_Defining_Identifier
                                (MN (M_Dispatch_Triggers_Of_All_Transitions)),
                              Array_Size => Make_Defining_Identifier (Q)),
                         Aggregate_Name      => Make_Defining_Identifier
                           (MN (M_States_Attributes))),
                      Aggregate_Name      => Make_Defining_Identifier
                        (Map_C_Variable_Name (E, Current_State => True))))));

      CTU.Append_Node_To_List (N, WStatements);

      N := CTU.Make_For_Statement
        (Pre_Cond   => Make_Variable_Declaration
           (Defining_Identifier => Make_Defining_Identifier (Q),
            Used_Type           => Make_Defining_Identifier (TN (T_Int)),
            Value               => Make_Literal (CV.New_Int_Value (0, 0, 10))),
         Condition  => CTU.Make_Expression
           (Left_Expr  => Make_Defining_Identifier
                (Q),
            Operator   => CTU.Op_Less,
            Right_Expr => Make_Member_Designator
              (Defining_Identifier => Make_Defining_Identifier
                   (MN (M_Nb_Transitions)),
               Aggregate_Name      => Make_Defining_Identifier
                 (VN (V_Next_Complete_State)),
               Is_Pointer          => True)),
         Post_Cond  =>  CTU.Make_Expression
           (Left_Expr  => Make_Defining_Identifier (Q),
            Operator   => CTU.Op_Plus_Plus,
            Right_Expr => No_Node),
         Statements =>
           Make_List_Id
             (Make_Assignment_Statement
                  (Variable_Identifier => Make_Member_Designator
                     (Defining_Identifier => Make_Array_Declaration
                        (Defining_Identifier => Make_Defining_Identifier
                             (MN (M_Nb_Dispatch_Triggers_Of_Each_Transition)),
                         Array_Size => Make_Defining_Identifier (Q)),
                      Aggregate_Name      => Make_Defining_Identifier
                        (VN (V_Next_Complete_State)),
                      Is_Pointer          => True),
                   Expression  => Make_Member_Designator
                     (Defining_Identifier => Make_Member_Designator
                        (Defining_Identifier => Make_Array_Declaration
                           (Defining_Identifier => Make_Defining_Identifier
                             (MN (M_Nb_Dispatch_Triggers_Of_Each_Transition)),
                              Array_Size => Make_Defining_Identifier (Q)),
                         Aggregate_Name      => Make_Defining_Identifier
                           (MN (M_States_Attributes))),
                      Aggregate_Name      => Make_Defining_Identifier
                        (Map_C_Variable_Name (E, Current_State => True))))));

      CTU.Append_Node_To_List (N, WStatements);

      N := Make_Function_Implementation
        (Specification => Make_Specification_Of_BA_Related_Function
           (S, Update_Next_Complete_State => True),
         Declarations  => No_List,
         Statements    => WStatements);

      Append_Node_To_List (N, CTN.Declarations (Current_File));

   end Make_Update_Next_Complete_State_Function;

   -----------------------------------------
   -- Make_States_Initialization_Function --
   -----------------------------------------

   procedure Make_States_Initialization_Function
     (S            : Node_Id)
   is
      BA                         : Node_Id;
      State, List_Node           : Node_Id;
      N                          : Node_Id;
      State_Identifier           : Unsigned_Long_Long;
      E                          : constant Node_Id :=
        AIN.Parent_Subcomponent (S);
      P      : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (S);
      WStatements : constant List_Id := New_List (CTN.K_Statement_List);
      Initial_State_Index        : Unsigned_Long_Long;
      Nb_States : constant Unsigned_Long_Long := Compute_Nb_States (S);
      Complete_States_Index : array (1 .. Nb_States) of Unsigned_Long_Long;
      Nb_Complete_States    : Unsigned_Long_Long := 0;
      i                     : Unsigned_Long_Long;
      Nb_On_Dispatch_Transitions : constant Unsigned_Long_Long :=
        Compute_Nb_On_Dispatch_Transitions (S);
   begin

      BA := Get_Behavior_Specification (S);

      --  7)
      --  void <<thread_name>>_states_initialization (void)
      --  {
      --  /* fill the __po_hi_producer_states_array with corresponding
      --  states */
      --  __po_hi_producer_states_array[0].name = s0;
      --  __po_hi_producer_states_array[0].kind = __po_hi_initial;
      --
      --  __po_hi_producer_states_array[1].name = s1;
      --  __po_hi_producer_states_array[1].kind = __po_hi_execution;
      --
      --  __po_hi_producer_states_array[2].name = s2;
      --  __po_hi_producer_states_array[2].kind = __po_hi_execution;
      --
      --  __po_hi_producer_states_array[3].name = s3;
      --  __po_hi_producer_states_array[3].kind = __po_hi_complete_final;

      --  /* initialize the __po_hi_producer_current_state with */
      --  /* the initial state */

      --  __po_hi_producer_current_state = __po_hi_producer_states_array[0];
      --  }

      N := Message_Comment ("fill the array "
                            & Get_Name_String
                              (Map_C_Variable_Name (E, States_Array => True))
                            & " with corresponding states");

      CTU.Append_Node_To_List (N, WStatements);

      State := BATN.First_Node (BATN.States (BA));
      State_Identifier := 0;

      while Present (State) loop

         if not BANu.Is_Empty (BATN.Identifiers (State)) then
            List_Node := BATN.First_Node (BATN.Identifiers (State));

            while Present (List_Node) loop

               CTU.Append_Node_To_List
                 (Make_Assignment_Statement
                    (Variable_Identifier => Make_Member_Designator
                         (Defining_Identifier => Make_Defining_Identifier
                              (MN (M_Name)),
                          Aggregate_Name      => Make_Array_Declaration
                            (Defining_Identifier =>
                                   Make_Defining_Identifier
                               (Map_C_Variable_Name (E, States_Array => True)),
                             Array_Size => Make_Literal
                               (CV.New_Int_Value (State_Identifier, 0, 10)))),
                     Expression          => CTU.Make_Defining_Identifier
                       (BATN.Display_Name (List_Node))),
                  WStatements);

               CTU.Append_Node_To_List
                 (Make_Assignment_Statement
                    (Variable_Identifier => Make_Member_Designator
                         (Defining_Identifier => Make_Defining_Identifier
                              (MN (M_Kind)),
                          Aggregate_Name      => Make_Array_Declaration
                            (Defining_Identifier =>
                                   Make_Defining_Identifier
                               (Map_C_Variable_Name (E, States_Array => True)),
                             Array_Size => Make_Literal
                               (CV.New_Int_Value (State_Identifier, 0, 10)))),
                     Expression          => Map_C_State_Kind_Name
                       (State_Kind (State))),
                  WStatements);

               if Behavior_State_Kind'Val (State_Kind (State)) = BSK_Initial
                 or else Behavior_State_Kind'Val (State_Kind (State)) =
                   BSK_Initial_Complete
                   or else Behavior_State_Kind'Val (State_Kind (State)) =
                     BSK_Initial_Final
                   or else Behavior_State_Kind'Val (State_Kind (State)) =
                       BSK_Initial_Complete_Final
               then
                  Initial_State_Index := State_Identifier;
               end if;

               if Behavior_State_Kind'Val (BATN.State_Kind (State))
                 = BSK_Initial_Complete
                 or else Behavior_State_Kind'Val (BATN.State_Kind (State))
                   = BSK_Initial_Complete_Final
                 or else Behavior_State_Kind'Val (BATN.State_Kind (State))
                   = BSK_Complete
                 or else Behavior_State_Kind'Val (BATN.State_Kind (State))
                   = BSK_Complete_Final
               then
                  Nb_Complete_States := Nb_Complete_States + 1;
                  Complete_States_Index (Nb_Complete_States) :=
                    State_Identifier;
               end if;

               State_Identifier := State_Identifier + 1;
               List_Node := BATN.Next_Node (List_Node);
            end loop;
         end if;

         State := BATN.Next_Node (State);
      end loop;

      if P = Thread_Sporadic and then
        Nb_On_Dispatch_Transitions > 1
      then

         i := 0;

         N := Message_Comment
           ("For each complete state initialize nb_transitions,"
            & " nb_dispatch_triggers_of_each_transition,"
            & " dispatch_triggers_of_all_transitions"
            & " and nb_of_all_dispatch_events");

         CTU.Append_Node_To_List (N, WStatements);

         State := BATN.First_Node (BATN.States (BA));

         while Present (State) loop

            if Behavior_State_Kind'Val (BATN.State_Kind (State))
              = BSK_Initial_Complete
              or else Behavior_State_Kind'Val (BATN.State_Kind (State))
                = BSK_Initial_Complete_Final
              or else Behavior_State_Kind'Val (BATN.State_Kind (State))
                = BSK_Complete
              or else Behavior_State_Kind'Val (BATN.State_Kind (State))
                = BSK_Complete_Final
            then
               List_Node := BATN.First_Node (BATN.Identifiers (State));

               while Present (List_Node) loop
                  i := i + 1;
                  --  /* complete state S0 */
                  N := Message_Comment
                    ("Complete state "
                     & Get_Name_String (BATN.Display_Name (List_Node)));
                  CTU.Append_Node_To_List (N, WStatements);

                  --  1)  __po_hi_consumer_states_array[0].
                  --   state_attributes.nb_transitions = 1;

                  N := Make_Assignment_Statement
                   (Variable_Identifier => Make_Member_Designator
                      (Defining_Identifier => Make_Member_Designator
                           (Defining_Identifier => Make_Defining_Identifier
                            (MN (M_Nb_Transitions)),
                            Aggregate_Name      => Make_Defining_Identifier
                            (MN (M_States_Attributes))),
                        Aggregate_Name      => Make_Array_Declaration
                          (Defining_Identifier =>
                               Make_Defining_Identifier
                             (Map_C_Variable_Name (E, States_Array => True)),
                           Array_Size => Make_Literal
                             (CV.New_Int_Value
                                  (Complete_States_Index (i), 0, 10)))),
                     Expression  => Make_Literal
                       (CV.New_Int_Value
                      (Compute_Nb_Trans_Stemmed_From_A_Specific_Complete_Stat
                                    (BA, List_Node), 0, 10)));

                  CTU.Append_Node_To_List (N, WStatements);

                  --  2) __po_hi_consumer_states_array[0].
                  --  state_attributes.nb_of_all_dispatch_events = 1;

                  N := Make_Assignment_Statement
                   (Variable_Identifier => Make_Member_Designator
                      (Defining_Identifier => Make_Member_Designator
                           (Defining_Identifier => Make_Defining_Identifier
                            (MN (M_Nb_Of_All_Dispatch_Events)),
                            Aggregate_Name      => Make_Defining_Identifier
                            (MN (M_States_Attributes))),
                        Aggregate_Name      => Make_Array_Declaration
                          (Defining_Identifier =>
                               Make_Defining_Identifier
                             (Map_C_Variable_Name (E, States_Array => True)),
                           Array_Size => Make_Literal
                             (CV.New_Int_Value
                                  (Complete_States_Index (i), 0, 10)))),
                     Expression  => Make_Literal
                       (CV.New_Int_Value
                      (Nb_All_Dispatch_Triggers_From_A_Specific_Complete_Stat
                                    (BA, List_Node), 0, 10)));

                  CTU.Append_Node_To_List (N, WStatements);

                  --  3) __po_hi_consumer_states_array[0].state_attributes.
                  --  nb_dispatch_triggers_of_each_transition =
                  --  (__po_hi_int32_t *)malloc( sizeof(__po_hi_int32_t) * 1);

                  N := Make_Assignment_Statement
                    (Variable_Identifier => Make_Member_Designator
                      (Defining_Identifier => Make_Member_Designator
                           (Defining_Identifier => Make_Defining_Identifier
                            (MN (M_Nb_Dispatch_Triggers_Of_Each_Transition)),
                            Aggregate_Name      => Make_Defining_Identifier
                            (MN (M_States_Attributes))),
                        Aggregate_Name      => Make_Array_Declaration
                          (Defining_Identifier =>
                               Make_Defining_Identifier
                             (Map_C_Variable_Name (E, States_Array => True)),
                           Array_Size => Make_Literal
                             (CV.New_Int_Value
                                  (Complete_States_Index (i), 0, 10)))),
                     Expression          => Make_Type_Conversion
                       (Subtype_Mark => Make_Pointer_Type
                            (RE (RE_Int32_T)),
                        Expression   => Make_Call_Profile
                          (Make_Defining_Identifier (FN (F_Malloc)),
                           Make_List_Id
                             (Make_Expression
                                  (Left_Expr  => Make_Call_Profile
                                     (Make_Defining_Identifier (FN (F_Sizeof)),
                                        Make_List_Id
                                          (RE (RE_Int32_T))),
                                   Operator   => Op_Asterisk,
                                   Right_Expr =>
                                     Make_Literal
                                       (CV.New_Int_Value
                 (Max_Dispatch_Triggers_Per_Trans_From_A_Specific_Complete_Stat
                                    (BA, List_Node), 0, 10)))))));

                  CTU.Append_Node_To_List (N, WStatements);

                  --  4) __po_hi_consumer_states_array[0].state_attributes.
                  --  dispatch_triggers_of_all_transitions =
                  --   (__po_hi_local_port_t *)malloc
                  --   ( sizeof(__po_hi_local_port_t) * 1);

                  N := Make_Assignment_Statement
                    (Variable_Identifier => Make_Member_Designator
                      (Defining_Identifier => Make_Member_Designator
                           (Defining_Identifier => Make_Defining_Identifier
                            (MN (M_Dispatch_Triggers_Of_All_Transitions)),
                            Aggregate_Name      => Make_Defining_Identifier
                            (MN (M_States_Attributes))),
                        Aggregate_Name      => Make_Array_Declaration
                          (Defining_Identifier =>
                               Make_Defining_Identifier
                             (Map_C_Variable_Name (E, States_Array => True)),
                           Array_Size => Make_Literal
                             (CV.New_Int_Value
                                  (Complete_States_Index (i), 0, 10)))),
                     Expression          => Make_Type_Conversion
                       (Subtype_Mark => Make_Pointer_Type
                            (RE (RE_Local_Port_T)),
                        Expression   => Make_Call_Profile
                          (Make_Defining_Identifier (FN (F_Malloc)),
                           Make_List_Id
                             (Make_Expression
                                  (Left_Expr  => Make_Call_Profile
                                     (Make_Defining_Identifier (FN (F_Sizeof)),
                                        Make_List_Id
                                          (RE (RE_Local_Port_T))),
                                   Operator   => Op_Asterisk,
                                   Right_Expr =>
                                     Make_Literal
                                       (CV.New_Int_Value
                        (Nb_All_Dispatch_Triggers_From_A_Specific_Complete_Stat
                                    (BA, List_Node), 0, 10)))))));

                  CTU.Append_Node_To_List (N, WStatements);

                  --  5) fill the array __po_hi_consumer_states_array[0].
                  --  state_attributes.nb_dispatch_triggers_of_each_transition

                  Fill_Nb_Dispatch_Triggers_Of_Each_Transition_Array
                    (S, BA, List_Node, Complete_States_Index (i), WStatements);

                  --  6) fill the array __po_hi_consumer_states_array[0].
                  --  state_attributes.dispatch_triggers_of_all_transitions[0]
                  --  = LOCAL_PORT (consumer, c1);

                  Fill_Dispatch_Triggers_Of_All_Transitions_Array
                    (S, BA, List_Node, Complete_States_Index (i), WStatements);

                  List_Node := BATN.Next_Node (List_Node);
               end loop;
            end if;

            State := BATN.Next_Node (State);
         end loop;

      end if;

      N := Message_Comment ("Initialize the current state of the thread "
                            & Get_Name_String
                              (Map_C_Variable_Name (E, Current_State => True))
                            & " with the initial state");

      CTU.Append_Node_To_List (N, WStatements);

      CTU.Append_Node_To_List
        (Make_Assignment_Statement
           (Variable_Identifier => Make_Defining_Identifier
                (Map_C_Variable_Name (E, Current_State => True)),
            Expression          => Make_Array_Declaration
              (Defining_Identifier =>
                   Make_Defining_Identifier
                 (Map_C_Variable_Name (E, States_Array => True)),
               Array_Size => Make_Literal
                 (CV.New_Int_Value (Initial_State_Index, 0, 10)))),
         WStatements);

      if P = Thread_Sporadic and then
        Nb_On_Dispatch_Transitions > 1
      then
         N := CTU.Make_Call_Profile
           (Defining_Identifier => Make_Defining_Identifier
              (Map_C_BA_Related_Function_Name
                   (E, Update_Next_Complete_State => True)),
            Parameters          =>  Make_List_Id
              (Make_Defining_Identifier
                   (VN (V_Next_Complete_State))));
         Append_Node_To_List (N, WStatements);
      end if;

      N := Make_Function_Implementation
        (Specification => Make_Specification_Of_BA_Related_Function
           (S, States_Initialization => True),
         Declarations  => No_List,
         Statements    => WStatements);

      Append_Node_To_List (N, CTN.Declarations (Current_File));

   end Make_States_Initialization_Function;

   ----------------------
   -- Search_State_Kind --
   -----------------------

   function Search_State_Kind
     (BA        : Node_Id;
      State_Idt : Node_Id) return Ocarina.Types.Byte
   is
      State, List_Node : Node_Id;
      Found            : Boolean := False;
      Result           : Ocarina.Types.Byte;
   begin

      State := BATN.First_Node (BATN.States (BA));

      while Present (State) loop

         List_Node := BATN.First_Node (BATN.Identifiers (State));

         while Present (List_Node) loop

            if (Standard.Utils.To_Upper
                (BATN.Display_Name (List_Node)) =
                  Standard.Utils.To_Upper
                    (BATN.Display_Name (State_Idt)))
            then
               Result := State_Kind (State);
               Found := True;
            end if;

            exit when Found;
            List_Node := BATN.Next_Node (List_Node);
         end loop;

         exit when Found;
         State := BATN.Next_Node (State);
      end loop;

      if not Found then
         Display_Error
           ("The state '"
              & Get_Name_String (BATN.Display_Name (State_Idt))
            & "' is not declared",
            Fatal => True);
      end if;

      return Result;

   end Search_State_Kind;

   --------------------------------
   -- Find_Index_In_States_Array --
   --------------------------------

   function Find_Index_In_States_Array
     (BA        : Node_Id;
      State_Idt : Node_Id) return Unsigned_Long_Long
   is
      State, List_Node : Node_Id;
      Found            : Boolean := False;
      Result           : Unsigned_Long_Long := 0;
   begin

      State := BATN.First_Node (BATN.States (BA));

      while Present (State) loop

         List_Node := BATN.First_Node (BATN.Identifiers (State));

         while Present (List_Node) loop

            if (Standard.Utils.To_Upper
                (BATN.Display_Name (List_Node)) =
                  Standard.Utils.To_Upper
                    (BATN.Display_Name (State_Idt)))
            then
               Found := True;
            end if;

            exit when Found;
            Result := Result + 1;
            List_Node := BATN.Next_Node (List_Node);
         end loop;

         exit when Found;
         State := BATN.Next_Node (State);
      end loop;

      if not Found then
         Display_Error
           ("The Destination state is not declared",
            Fatal => True);
      end if;

      return Result;

   end Find_Index_In_States_Array;

   --------------------------
   -- Update_Current_State --
   --------------------------

   procedure Update_Current_State
     (E                : Node_Id;
      BA               : Node_Id;
      Transition_Node  : Node_Id;
      Stats            : List_Id)
   is
      N : Node_Id;
   begin
      N := Message_Comment (" Update the current state ");
      CTU.Append_Node_To_List (N, Stats);

      N := Make_Assignment_Statement
        (Variable_Identifier => Make_Defining_Identifier
           (Map_C_Variable_Name (E, Current_State => True)),
         Expression          => Make_Array_Declaration
           (Defining_Identifier =>
                Make_Defining_Identifier
              (Map_C_Variable_Name (E, States_Array => True)),
            Array_Size => Make_Literal
              (CV.New_Int_Value
                   (Find_Index_In_States_Array
                        (BA, BATN.Destination (Transition_Node)),
                    0, 10))));
      CTU.Append_Node_To_List (N, Stats);
   end Update_Current_State;

   ---------------------------
   -- Map_C_Transition_Node --
   ---------------------------

   function Map_C_Transition_Node
     (Node             : Node_Id;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id;
      Else_St          : List_Id) return List_Id
   is
      N, N1            : Node_Id;
      Else_stats       : constant List_Id := New_List (CTN.K_Statement_List);
      elsif_statements : constant List_Id := New_List (CTN.K_Statement_List);
      E                : constant Node_Id := AIN.Parent_Subcomponent (S);
      BA               : constant Node_Id := Get_Behavior_Specification (S);
      Condition        : Node_Id;
   begin
      N := BATN.Next_Node (Node);

      if Present (BATN.Behavior_Condition (Node))
        and then Present (BATN.Condition (BATN.Behavior_Condition (Node)))
      then
         Condition := Evaluate_BA_Value_Expression
           (Node             => BATN.Value_Expression
              (BATN.Condition
                   (BATN.Behavior_Condition (Node))),
            Subprogram_Root  => S,
            Declarations     => Declarations,
            Statements       => Statements);
      else
         Display_Error
           ("As there are an more than one transition that starts from"
            & " the same state called '"
            & Get_Name_String
              (BATN.Display_Name (BATN.First_Node
               (BATN.Sources (Node))))
            & "', all these transitions must have conditions",
            Fatal => True);
      end if;

      if Present (N) then
         if Present (BATN.Behavior_Action_Block (Node)) and then
           Present (BATN.Behav_Acts (BATN.Behavior_Action_Block (Node)))
         then
            N1 := Message_Comment (" Mapping of actions ");
            CTU.Append_Node_To_List (N1, elsif_statements);

            Map_C_Behavior_Action_Block
              (Node         => BATN.Behavior_Action_Block (Node),
               S            => S,
               Declarations => Declarations,
               Statements  => elsif_statements);
         end if;

         Update_Current_State (E, BA, Node, elsif_statements);

         CTU.Append_Node_To_List
           (CTU.Make_If_Statement
              (Condition       => Condition,
               Statements      => elsif_statements,
               Else_Statements =>
                 Map_C_Transition_Node
                   (Node         => N,
                    S            => S,
                    Declarations => Declarations,
                    Statements   => Statements,
                    Else_St      => Else_St)),
            Else_stats);
      else

         if Present (BATN.Behavior_Action_Block (Node)) and then
           Present (BATN.Behav_Acts (BATN.Behavior_Action_Block (Node)))
         then
            N1 := Message_Comment (" Mapping of actions ");
            CTU.Append_Node_To_List (N1, elsif_statements);

            Map_C_Behavior_Action_Block
              (Node         => BATN.Behavior_Action_Block (Node),
               S            => S,
               Declarations => Declarations,
               Statements  => elsif_statements);
         end if;

         Update_Current_State (E, BA, Node, elsif_statements);

         if Present (Else_St) then
            CTU.Append_Node_To_List
              (CTU.Make_If_Statement
                 (Condition       => Condition,
                  Statements      => elsif_statements,
                  Else_Statements => Else_St),
               Else_stats);
         else
            CTU.Append_Node_To_List
              (CTU.Make_If_Statement
                 (Condition       => Condition,
                  Statements      => elsif_statements),
               Else_stats);
         end if;

      end if;

      return Else_stats;

   end Map_C_Transition_Node;

   ---------------------------------------
   -- Map_C_On_Dispatch_Transition_Node --
   ---------------------------------------

   function Map_C_On_Dispatch_Transition_Node
     (Node             : Node_Id;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id;
      Index_Transition : in out Unsigned_Long_Long) return List_Id
   is
      N, N1            : Node_Id;
      Else_stats       : constant List_Id := New_List (CTN.K_Statement_List);
      elsif_statements : constant List_Id := New_List (CTN.K_Statement_List);
      E                : constant Node_Id := AIN.Parent_Subcomponent (S);
      BA               : constant Node_Id := Get_Behavior_Specification (S);
      Condition        : Node_Id;
   begin
      N := BATN.Next_Node (Node);
      Index_Transition := Index_Transition + 1;

      Condition := CTU.Make_Expression
        (Left_Expr  => Make_Defining_Identifier
           (VN (V_Index_Transition_To_Execute)),
         Operator   => CTU.Op_Equal_Equal,
         Right_Expr => Make_Literal
           (CV.New_Int_Value (Index_Transition, 1, 10)));

      if Present (N) then
         if Present (BATN.Behavior_Action_Block (Node)) and then
           Present (BATN.Behav_Acts (BATN.Behavior_Action_Block (Node)))
         then
            N1 := Message_Comment (" Mapping of actions ");
            CTU.Append_Node_To_List (N1, elsif_statements);

            Map_C_Behavior_Action_Block
              (Node         => BATN.Behavior_Action_Block (Node),
               S            => S,
               Declarations => Declarations,
               Statements  => elsif_statements);
         end if;

         Update_Current_State (E, BA, Node, elsif_statements);

         CTU.Append_Node_To_List
           (CTU.Make_If_Statement
              (Condition       => Condition,
               Statements      => elsif_statements,
               Else_Statements =>
                 Map_C_On_Dispatch_Transition_Node
                   (Node         => N,
                    S            => S,
                    Declarations => Declarations,
                    Statements   => Statements,
                    Index_Transition => Index_Transition)),
            Else_stats);
      else
         if Present (BATN.Behavior_Action_Block (Node)) and then
           Present (BATN.Behav_Acts (BATN.Behavior_Action_Block (Node)))
         then
            N1 := Message_Comment (" Mapping of actions ");
            CTU.Append_Node_To_List (N1, elsif_statements);

            Map_C_Behavior_Action_Block
              (Node         => BATN.Behavior_Action_Block (Node),
               S            => S,
               Declarations => Declarations,
               Statements  => elsif_statements);
         end if;

         Update_Current_State (E, BA, Node, elsif_statements);

         CTU.Append_Node_To_List
           (CTU.Make_If_Statement
              (Condition       => Condition,
               Statements      => elsif_statements),
            Else_stats);

      end if;

      return Else_stats;

   end Map_C_On_Dispatch_Transition_Node;

   ---------------------------------------------
   -- Map_C_A_List_Of_On_Dispatch_Transitions --
   ---------------------------------------------

   procedure Map_C_A_List_Of_On_Dispatch_Transitions
     (S                            : Node_Id;
      BA                           : Node_Id;
      Sub_Transition_List          : List_Id;
      WDeclarations                : List_Id;
      WStatements                  : List_Id)
   is
      N                : Node_Id;
      Transition_Node  : Node_Id;
      E                : constant Node_Id := AIN.Parent_Subcomponent (S);
      Condition     : Node_Id;
      If_Statements : constant List_Id := New_List (CTN.K_Statement_List);
      Else_Stats    : List_Id;
      Index_Transition          : Unsigned_Long_Long := 0;
   begin

      if (BANu.Length (Sub_Transition_List) = 1) then

         Transition_Node := BATN.First_Node (Sub_Transition_List);

         --  i)
         --
         --  {
         --    /* map actions if there exist */
         --    ...
         --    /* update current state */
         --    __po_hi_consumer_current_state =
         --       __po_hi_consumer_states_array[3];
         --  }
         --
         --  break;

         if Present (BATN.Behavior_Action_Block (Transition_Node))
           and then Present
             (BATN.Behav_Acts
                (BATN.Behavior_Action_Block (Transition_Node)))
         then

            N := Message_Comment (" Mapping of actions ");
            CTU.Append_Node_To_List (N, WStatements);

            Map_C_Behavior_Action_Block
              (BATN.Behavior_Action_Block (Transition_Node),
               S, WDeclarations, WStatements);
         end if;

         --  ii) Update the current state
         Update_Current_State (E, BA, Transition_Node, WStatements);

      elsif (BANu.Length (Sub_Transition_List) >= 2) then
         Transition_Node := BATN.First_Node (Sub_Transition_List);
         Index_Transition := Index_Transition + 1;

         Condition := CTU.Make_Expression
           (Left_Expr  => Make_Defining_Identifier
              (VN (V_Index_Transition_To_Execute)),
            Operator   => CTU.Op_Equal_Equal,
            Right_Expr => Make_Literal
               (CV.New_Int_Value (Index_Transition, 1, 10)));

         if Present (BATN.Behavior_Action_Block (Transition_Node))
           and then Present
             (BATN.Behav_Acts
                (BATN.Behavior_Action_Block
                   (Transition_Node)))
         then

            N := Message_Comment (" Mapping of actions ");
            CTU.Append_Node_To_List (N, If_Statements);

            Map_C_Behavior_Action_Block
              (BATN.Behavior_Action_Block (Transition_Node),
               S, WDeclarations, If_Statements);
         end if;

         Update_Current_State (E, BA, Transition_Node, If_Statements);

         Transition_Node := BATN.Next_Node (Transition_Node);

         Else_Stats := Map_C_On_Dispatch_Transition_Node
           (Transition_Node, S, WDeclarations,
            WStatements, Index_Transition);

         CTU.Append_Node_To_List
           (CTU.Make_If_Statement
              (Condition       => Condition,
               Statements      => If_Statements,
               Else_Statements => Else_Stats),
            WStatements);

      end if;

   end Map_C_A_List_Of_On_Dispatch_Transitions;

   ---------------------------------
   -- Map_C_A_List_Of_Transitions --
   ---------------------------------

   procedure Map_C_A_List_Of_Transitions
     (S                         : Node_Id;
      BA                        : Node_Id;
      Otherwise_Transition_Node : Node_Id;
      Sub_Transition_List       : List_Id;
      WDeclarations             : List_Id;
      WStatements               : List_Id)
   is
      N                : Node_Id;
      Transition_Node  : Node_Id;
      E                : constant Node_Id := AIN.Parent_Subcomponent (S);

      Condition     : Node_Id;
      If_Statements : constant List_Id := New_List (CTN.K_Statement_List);
      Else_Stats    : List_Id;

   begin

      if Present (Otherwise_Transition_Node) then
         Else_Stats := New_List (CTN.K_Statement_List);

         if Present (BATN.Behavior_Action_Block (Otherwise_Transition_Node))
           and then Present (BATN.Behav_Acts
                             (BATN.Behavior_Action_Block
                                (Otherwise_Transition_Node)))
         then
            N := Message_Comment (" Mapping of actions ");
            CTU.Append_Node_To_List (N, Else_Stats);

            Map_C_Behavior_Action_Block
              (BATN.Behavior_Action_Block (Otherwise_Transition_Node),
               S, WDeclarations, Else_Stats);
         end if;

         Update_Current_State (E, BA, Otherwise_Transition_Node, Else_Stats);
      else
         Else_Stats := No_List;
      end if;

      if (BANu.Length (Sub_Transition_List) = 1) then

         Transition_Node := BATN.First_Node (Sub_Transition_List);

         if Present (BATN.Behavior_Condition (Transition_Node))
           and then Present (BATN.Condition
                             (BATN.Behavior_Condition (Transition_Node)))
         then

            --  i) Make if .. else => in the case of otherwise transition
            --  or if without else => in the case of one transition with
            --  condition

            Condition := Evaluate_BA_Value_Expression
              (Node             => BATN.Value_Expression
                 (BATN.Condition (BATN.Behavior_Condition (Transition_Node))),
               Subprogram_Root  => S,
               Declarations     => WDeclarations,
               Statements       => WStatements);

            if Present (BATN.Behavior_Action_Block (Transition_Node))
              and then Present (BATN.Behav_Acts
                                (BATN.Behavior_Action_Block (Transition_Node)))
            then

               N := Message_Comment (" Mapping of actions ");
               CTU.Append_Node_To_List (N, If_Statements);

               Map_C_Behavior_Action_Block
                 (BATN.Behavior_Action_Block (Transition_Node),
                  S, WDeclarations, If_Statements);
            end if;

            --  ii) Update the current state
            Update_Current_State (E, BA, Transition_Node, If_Statements);

            CTU.Append_Node_To_List
              (CTU.Make_If_Statement
                 (Condition       => Condition,
                  Statements      => If_Statements,
                  Else_Statements => Else_Stats),
               WStatements);
         else
            if not Present (Otherwise_Transition_Node) then

               --  i) Map actions of the transition without if statement
               if Present (BATN.Behavior_Action_Block (Transition_Node))
                 and then Present (BATN.Behav_Acts
                                   (BATN.Behavior_Action_Block
                                      (Transition_Node)))
               then

                  N := Message_Comment (" Mapping of actions ");
                  CTU.Append_Node_To_List (N, WStatements);

                  Map_C_Behavior_Action_Block
                    (BATN.Behavior_Action_Block (Transition_Node),
                     S, WDeclarations, WStatements);
               end if;

               --  ii) Update the current state
               Update_Current_State (E, BA, Transition_Node, WStatements);

            else
               Display_Error
                 ("As there is an otherwise transition then"
                  & " the other transition that starts from"
                  & " the same state called '"
                  & Get_Name_String
                    (BATN.Display_Name (BATN.First_Node
                     (BATN.Sources (Transition_Node))))
                  & "' must have a condition",
                  Fatal => True);
            end if;
         end if;

      elsif (BANu.Length (Sub_Transition_List) > 1) then
         Transition_Node := BATN.First_Node (Sub_Transition_List);

         if Present (BATN.Behavior_Condition (Transition_Node))
           and then Present (BATN.Condition
                             (BATN.Behavior_Condition (Transition_Node)))
         then
            Condition := Evaluate_BA_Value_Expression
              (Node             => BATN.Value_Expression
                 (BATN.Condition (BATN.Behavior_Condition (Transition_Node))),
               Subprogram_Root  => S,
               Declarations     => WDeclarations,
               Statements       => WStatements);

            if Present (BATN.Behavior_Action_Block (Transition_Node))
              and then Present (BATN.Behav_Acts
                                (BATN.Behavior_Action_Block
                                   (Transition_Node)))
            then
               N := Message_Comment (" Mapping of actions ");
               CTU.Append_Node_To_List (N, If_Statements);

               Map_C_Behavior_Action_Block
                 (BATN.Behavior_Action_Block (Transition_Node),
                  S, WDeclarations, If_Statements);
            end if;

            Update_Current_State (E, BA, Transition_Node, If_Statements);

            Transition_Node := BATN.Next_Node (Transition_Node);

            Else_Stats := Map_C_Transition_Node
              (Transition_Node, S, WDeclarations,
               WStatements, Else_Stats);

            CTU.Append_Node_To_List
              (CTU.Make_If_Statement
                 (Condition       => Condition,
                  Statements      => if_Statements,
                  Else_Statements => Else_Stats),
               WStatements);
         else
            Display_Error
              ("As there are more than one transition that starts from"
               & " the same state called '"
               & Get_Name_String
                 (BATN.Display_Name (BATN.First_Node
                  (BATN.Sources (Transition_Node))))
               & "', all these transitions must have conditions",
               Fatal => True);
         end if;
      end if;

   end Map_C_A_List_Of_Transitions;

   ---------------------------------------------------------
   -- Examine_Current_State_Until_Reaching_Complete_State --
   ---------------------------------------------------------

   procedure Examine_Current_State_Until_Reaching_Complete_State
     (S                         : Node_Id;
      BA                        : Node_Id;
      WDeclarations             : List_Id;
      WStatements               : List_Id)
   is
      While_Statements     : constant List_Id := New_List
        (CTN.K_Statement_List);
      State, List_Node     : Node_Id;
      Switch_Alternatives  : constant List_Id := New_List
        (CTN.K_Alternatives_List);
      Switch_Statements    : List_Id;
      Switch_Labels        : List_Id;

      N                            : Node_Id;
      Behav_Transition             : Node_Id;
      Transition_Node              : Node_Id;
      Otherwise_Transition_Node    : Node_Id := No_Node;
      E                            : constant Node_Id :=
        AIN.Parent_Subcomponent (S);
      Sub_Transition_List          : List_Id;
      Source                       : Node_Id;
      Condition     : Node_Id;
   begin
      --  Make while (__po_hi_producer_current_state.kind ==
      --  __po_hi_execution)

      Condition := CTU.Make_Expression
        (Left_Expr  => Make_Member_Designator
           (Defining_Identifier => Make_Defining_Identifier
             (MN (M_kind)),
            Aggregate_Name      => Make_Defining_Identifier
              (Map_C_Variable_Name (E, Current_State => True))),
         Operator   => CTU.Op_Equal_Equal,
         Right_Expr => RE (RE_Execution));

      --  Make a « switch case » statement on all the
      --  « execution » states

      State := BATN.First_Node (BATN.States (BA));

      while Present (State) loop
         if Behavior_State_Kind'Val (BATN.State_Kind (State))
           = BSK_No_Kind
         then
            List_Node := BATN.First_Node (BATN.Identifiers (State));

            while Present (List_Node) loop

               Switch_Statements := New_List (CTN.K_Statement_List);
               Switch_Labels := New_List (CTN.K_Label_List);
               Sub_Transition_List := BANu.New_List
                 (BATN.K_List_Id, No_Location);

               Otherwise_Transition_Node := No_Node;

               CTU.Append_Node_To_List
                 (Make_Defining_Identifier
                    (BATN.Display_Name (List_Node)),
                  Switch_Labels);

               --  /** go over all transitions that have « s1 »
               --  as source state. In the case of one transition
               --  without condition, we don't make an « if » statement,
               --  instead we map the actions and then we update the
               --  current state with the destination state of
               --  the transition. **/

               Behav_Transition := BATN.First_Node
                 (BATN.Transitions (BA));
               while Present (Behav_Transition) loop
                  Transition_Node := BATN.Transition (Behav_Transition);

                  if BATN.Kind (Transition_Node) =
                    BATN.K_Execution_Behavior_Transition
                  then
                     if BANu.Length (BATN.Sources (Transition_Node)) = 1
                     then
                        Source := BATN.First_Node
                          (BATN.Sources (Transition_Node));
                        if  (Standard.Utils.To_Upper
                             (BATN.Display_Name (List_Node)) =
                               Standard.Utils.To_Upper
                                 (BATN.Display_Name (Source)))
                        then
                           if Present (BATN.Behavior_Condition
                                       (Transition_Node)) and then
                             Present (BATN.Condition
                                      (BATN.Behavior_Condition
                                         (Transition_Node))) and then

                             BATN.Is_Otherwise
                               (BATN.Condition
                                  (BATN.Behavior_Condition
                                     (Transition_Node)))
                           then
                              Otherwise_Transition_Node :=
                                Transition_Node;
                           else
                              BATN.Set_Next_Node (Transition_Node, No_Node);
                              BANu.Append_Node_To_List
                                (Transition_Node,
                                 Sub_Transition_List);
                           end if;
                        end if;
                     end if;
                  end if;
                  Behav_Transition := BATN.Next_Node (Behav_Transition);
               end loop;

               Map_C_A_List_Of_Transitions
                 (S, BA, Otherwise_Transition_Node, Sub_Transition_List,
                  WDeclarations, Switch_Statements);

               N :=
                 Make_Switch_Alternative (Switch_Labels,
                                          Switch_Statements);

               CTU.Append_Node_To_List (N, Switch_Alternatives);

               List_Node := BATN.Next_Node (List_Node);
            end loop;
         end if;

         State := BATN.Next_Node (State);
      end loop;

      Append_Node_To_List
        (Make_Switch_Alternative (No_List, No_List),
         Switch_Alternatives);

      N :=
        Make_Switch_Statement
          (Expression   => Make_Member_Designator
             (Defining_Identifier => Make_Defining_Identifier
                (MN (M_Name)),
              Aggregate_Name      => Make_Defining_Identifier
                (Map_C_Variable_Name (E, Current_State => True))),
           Alternatives => Switch_Alternatives);

      CTU.Append_Node_To_List (N, While_Statements);

      CTU.Append_Node_To_List
        (CTU.Make_While_Statement
           (Condition  => Condition,
            Statements => While_Statements),
         WStatements);

   end Examine_Current_State_Until_Reaching_Complete_State;

   -------------------------------------
   -- Make_BA_Initialization_Function --
   -------------------------------------

   procedure Make_BA_Initialization_Function
     (S            : Node_Id)
   is
      BA                           : Node_Id;
      N                            : Node_Id;
      Behav_Transition             : Node_Id;
      Transition_Node              : Node_Id;
      Otherwise_Transition_Node    : Node_Id := No_Node;
      Sub_Transition_List          : List_Id;
      Source                       : Node_Id;
      WStatements    : constant List_Id := New_List (CTN.K_Statement_List);
      WDeclarations  : constant List_Id := New_List (CTN.K_Declaration_List);
      At_Least_One_Dest_State_Is_Execution : Boolean := False;

   begin

--  8)
--        void producer_ba_initialization
--      (__po_hi_task_id self)
--  {
--
--  base_types__integer tmp;
--  test__ba__backend__alpha_type tmp2;
--
--  /*
--  /** 1) For all transitions that have the initial state as
--  a source state. In the case of a single transition without condtition,
--  we don't make an « if » statement, instead we map the actions and then
--  we update the __po_hi_producer_current_state with the destination
--  state of the transition.
--  **/
--
--  /*  Read the data from the port Data_In*/
--  __po_hi_gqueue_get_value (self, LOCAL_PORT (producer, data_in),
--                            &(__data_in_request));
--  __data_in_value =
--  __data_in_request.PORT_VARIABLE (producer, data_in);
--
--  if (__data_in_value <= 10)
--  {  /* actions */
--     .....
--  --  -- /* update the current state */
--    __po_hi_producer_current_state = __po_hi_producer_states_array[1];
--  }
--  else if (__data_in_value > 10)
--  {  /* actions */
--
--
--     /* update the current state */
--    __po_hi_producer_current_state = __po_hi_producer_states_array[2];
--  }/** we add « else » statement if there is a transition with
--     « otherwise » condition **/
--
      BA := Get_Behavior_Specification (S);

      Map_C_Behavior_Variables (S, WDeclarations);

--  1) For all transitions that have the initial state as a source state.
--  In the case of a single transition without condition, we don't make an
--  « if » statement, instead we map the actions and then we update the
--  current_state with the destination state of the transition.

      N := Message_Comment
        ("1) For all transitions that have the initial"
         & " state as a source state."
         & " In the case of a single transition without condition,"
         & " we don't make an 'if' statement,"
         & " instead we map the actions and then we update the"
         & " current_state with the destination state of the transition.");

      CTU.Append_Node_To_List (N, WStatements);

      Behav_Transition := BATN.First_Node (BATN.Transitions (BA));
      Sub_Transition_List := BANu.New_List (BATN.K_List_Id, No_Location);

      while Present (Behav_Transition) loop
         Transition_Node := BATN.Transition (Behav_Transition);

         if BATN.Kind (Transition_Node) =
              BATN.K_Execution_Behavior_Transition
         then
            if BANu.Length (BATN.Sources (Transition_Node)) = 1 then
               Source := BATN.First_Node (BATN.Sources (Transition_Node));

               if Behavior_State_Kind'Val (Search_State_Kind (BA, Source))
                 = BSK_Initial
               then
                  if not At_Least_One_Dest_State_Is_Execution then

                     At_Least_One_Dest_State_Is_Execution :=
                       Behavior_State_Kind'Val
                         (Search_State_Kind
                            (BA, BATN.Destination (Transition_Node)))
                         = BSK_No_Kind;

                  end if;

                  if Present (BATN.Behavior_Condition (Transition_Node))
                    and then BATN.Is_Otherwise
                      (BATN.Condition
                         (BATN.Behavior_Condition (Transition_Node)))
                  then
                     Otherwise_Transition_Node := Transition_Node;
                  else
                     BANu.Append_Node_To_List
                       (Transition_Node, Sub_Transition_List);
                  end if;
               end if;
            else
               Display_Error
                 ("Many sources states for a transition are not supported",
               Fatal => True);
            end if;
         else
            Display_Error
              ("Mode Transition is not supported",
               Fatal => True);
         end if;

         Behav_Transition := BATN.Next_Node (Behav_Transition);
      end loop;

      Map_C_A_List_Of_Transitions
        (S, BA, Otherwise_Transition_Node, Sub_Transition_List,
         WDeclarations, WStatements);

--  /** 2) Now we examine __po_hi_producer_current_state if it is not a
--    « complete » state, i.e. it is an « execution » state, then we
--  should proceed transitions until reaching a « complete » state. **/
--
--  while (__po_hi_producer_current_state.kind == __po_hi_execution)
--  {
--  --  --  /** we make a « switch case » statement on all the
--  « execution » states **/
--  switch (__po_hi_producer_current_state.name)
--  {
--  --  --  case s1:
--  /** go over all transitions that have « s1 » as source state.
--  In the case of one transition without condition, we don't make an
--  « if » statement, instead we map the actions and then we update the
--  __po_hi_producer_current_state with the destination state of the
--   transition. **/
--
--  if (tmp < 10)
--  { /* actions */
--    ......
--  --  --    /* update the current state */
--   __po_hi_producer_current_state = __po_hi_producer_states_array[2];
--  }
--  else if (tmp >= 12 && tmp < 16)
--  { /* actions */
--    .....
--  --  --    /* update the current state */
--  __po_hi_producer_current_state = __po_hi_producer_states_array[1];
--  }
--  /** There is a transition with « otherwise » condition, then we add
--     « else » statement **/
--  else
--  { /* actions */
--    ......
--
--  /* update the current state */
--   __po_hi_producer_current_state = __po_hi_producer_states_array[3];
--  }
--  break;
--
--  case s2:
--
--  /** go over all transitions that have « s2 » as source state.
--  In the case of one transition without condition, we don't make an
--  « if » statement, instead we map the actions and the we update the
--  __po_hi_producer_current_state with the destination state of the
--   transition. Idem que « s1 » **/
--
--  /* actions */
--   ....
--
--  /* update the current state */
--  __po_hi_producer_current_state = __po_hi_producer_states_array[3];
--
--  break;
--  --  --  default: // do nothing;
--  break;
--  }
--     }
--
--  }
--
      if At_Least_One_Dest_State_Is_Execution then

         N := Message_Comment
           ("2) Now we examine current state"
            & " if it is not a 'complete' state, i.e. it is an 'execution'"
            & "  state, then we should proceed transitions until reaching"
            & " a 'complete' state.");

         CTU.Append_Node_To_List (N, WStatements);

         Examine_Current_State_Until_Reaching_Complete_State
           (S, BA, WDeclarations, WStatements);

      end if;

      N := Make_Function_Implementation
        (Specification => Make_Specification_Of_BA_Related_Function
           (S, BA_Initialization => True),
         Declarations  => WDeclarations,
         Statements    => WStatements);

      Append_Node_To_List (N, CTN.Declarations (Current_File));
   end Make_BA_Initialization_Function;

   -----------------------------------------------
   -- Make_Specification_Of_BA_Related_Function --
   -----------------------------------------------

   function Make_Specification_Of_BA_Related_Function
     (S                          : Node_Id;
      BA_Body                    : Boolean := False;
      BA_Initialization          : Boolean := False;
      States_Initialization      : Boolean := False;
      Update_Next_Complete_State : Boolean := False) return Node_Id
   is
      N, N1  : Node_Id;
      E      : constant Node_Id := AIN.Parent_Subcomponent (S);
      P      : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (S);
      Parameter_List  : List_Id;
      Nb_On_Dispatch_Transitions : constant Unsigned_Long_Long :=
        Compute_Nb_On_Dispatch_Transitions (S);
   begin

      if BA_Body then
         Parameter_List := New_List (CTN.K_List_Id);
         N :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (PN (P_Self)),
              Parameter_Type => RE (RE_Task_Id));
         Append_Node_To_List (N, Parameter_List);
      elsif BA_Initialization then
         Parameter_List := New_List (CTN.K_List_Id);
         N :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (PN (P_Self)),
              Parameter_Type => RE (RE_Task_Id));
         Append_Node_To_List (N, Parameter_List);
      end if;

      if P = Thread_Periodic
        or else (P = Thread_Sporadic and then
                 Nb_On_Dispatch_Transitions = 1)
      then
         if States_Initialization then
            Parameter_List := No_List;
         end if;
      elsif P = Thread_Sporadic and then
        Nb_On_Dispatch_Transitions > 1
      then
         if States_Initialization or else Update_Next_Complete_State then
            Parameter_List := Make_List_Id
              (Make_Parameter_Specification
                 (Defining_Identifier =>
                      Make_Defining_Identifier
                    (VN (V_Next_Complete_State)),
                  Parameter_Type      =>
                    Make_Pointer_Type
                      (RE (RE_Ba_Automata_State_T))));
         elsif BA_Body then
            N :=
              Make_Parameter_Specification
                (Make_Defining_Identifier (VN (V_Next_Complete_State)),
                 Parameter_Type => CTU.Make_Pointer_Type
                   (RE (RE_Ba_Automata_State_T)));
            Append_Node_To_List (N, Parameter_List);

            N :=
              Make_Parameter_Specification
                (Make_Defining_Identifier (VN (V_Index_Transition_To_Execute)),
                 Parameter_Type => RE (RE_Int32_T));
            Append_Node_To_List (N, Parameter_List);
         elsif BA_Initialization then
            N :=
              Make_Parameter_Specification
                (Make_Defining_Identifier (VN (V_Next_Complete_State)),
                 Parameter_Type => CTU.Make_Pointer_Type
                   (RE (RE_Ba_Automata_State_T)));
            Append_Node_To_List (N, Parameter_List);
         end if;
      end if;

      --  add data subcomponents of the thread to the call_parameters
      --  of the procedure <<thread_instance_name>>_ba_body
      if BA_Body or else BA_Initialization then
         if not AINU.Is_Empty (AIN.Subcomponents (S)) then
            N1 := AIN.First_Node (AIN.Subcomponents (S));

            while Present (N1) loop
               if AINU.Is_Data (AIN.Corresponding_Instance (N1)) then

                  N :=
                    Make_Parameter_Specification
                      (Map_C_Defining_Identifier (N1),
                       Parameter_Type =>
                         CTU.Make_Pointer_Type
                           (Map_C_Data_Type_Designator
                              (AIN.Corresponding_Instance (N1))));

                  Append_Node_To_List (N, Parameter_List);

               end if;
               N1 := AIN.Next_Node (N1);
            end loop;
         end if;
      end if;

      if BA_Body then
         N := Make_Function_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (Map_C_BA_Related_Function_Name (E, BA_Body => True)),
            Parameters          => Parameter_List,
            Return_Type         => New_Node (CTN.K_Void));
      elsif BA_Initialization then
         N := Make_Function_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (Map_C_BA_Related_Function_Name (E, BA_Initialization => True)),
            Parameters          => Parameter_List,
            Return_Type         => New_Node (CTN.K_Void));
      elsif States_Initialization then
         N := Make_Function_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (Map_C_BA_Related_Function_Name
                   (E, States_Initialization => True)),
            Parameters          => Parameter_List,
            Return_Type         => New_Node (CTN.K_Void));
      elsif Update_Next_Complete_State then
         N := Make_Function_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (Map_C_BA_Related_Function_Name
                   (E, Update_Next_Complete_State => True)),
            Parameters          => Parameter_List,
            Return_Type         => New_Node (CTN.K_Void));
      end if;
      return N;
   end Make_Specification_Of_BA_Related_Function;

   ----------------------------------------------
   -- Map_C_Implementation_of_BA_Body_Function --
   ----------------------------------------------

   procedure Map_C_Implementation_of_BA_Body_Function
     (S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      N  : Node_Id;
   begin

      N := Make_Function_Implementation
        (Specification => Make_Specification_Of_BA_Related_Function
           (S, BA_Body => True),
         Declarations  => Declarations,
         Statements    => Statements);

      Append_Node_To_List (N, CTN.Declarations (Current_File));

   end Map_C_Implementation_of_BA_Body_Function;

   -----------------------------------------------
   -- Make_BA_Body_Function_For_Sporadic_Thread --
   -----------------------------------------------

   procedure Make_BA_Body_Function_For_Sporadic_Thread
     (S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      BA                         : Node_Id;
      State, List_Node             : Node_Id;
      Switch_Alternatives          : constant List_Id := New_List
        (CTN.K_Alternatives_List);
      Switch_Statements            : List_Id;
      Switch_Labels                : List_Id;

      N                            : Node_Id;
      Behav_Transition             : Node_Id;
      Transition_Node              : Node_Id;
      E                            : constant Node_Id :=
        AIN.Parent_Subcomponent (S);
      Sub_Transition_List          : List_Id;
      Source                       : Node_Id;
   begin

      --  /* 1) we make switch case statement on all complete states from
      --  which on dispatch transitions are started */

      BA := Get_Behavior_Specification (S);

      Map_C_Behavior_Variables (S, Declarations);

      Behav_Transition := BATN.First_Node (BATN.Transitions (BA));

      N := Message_Comment
        ("1) we make switch case statement on all complete states "
         & " from which 'on dispatch' transitions are started.");
      CTU.Append_Node_To_List (N, Statements);

      --  Make a « switch case » statement on all the
      --  « complete » states

      State := BATN.First_Node (BATN.States (BA));

      while Present (State) loop
         if Behavior_State_Kind'Val (BATN.State_Kind (State))
           = BSK_Initial_Complete
           or else Behavior_State_Kind'Val (BATN.State_Kind (State))
             = BSK_Initial_Complete_Final
           or else Behavior_State_Kind'Val (BATN.State_Kind (State))
             = BSK_Complete
           or else Behavior_State_Kind'Val (BATN.State_Kind (State))
             = BSK_Complete_Final
         then

            List_Node := BATN.First_Node (BATN.Identifiers (State));

            while Present (List_Node) loop
               Switch_Statements := New_List (CTN.K_Statement_List);
               Switch_Labels := New_List (CTN.K_Label_List);

               Sub_Transition_List := BANu.New_List
                 (BATN.K_List_Id, No_Location);

               CTU.Append_Node_To_List
                 (Make_Defining_Identifier
                    (BATN.Display_Name (List_Node)),
                  Switch_Labels);

               --  /** go over all « on dispatch » transitions that have
               --  « BATN.Display_Name (List_Node) » as source state.
               --  Then we make an "if" on the index of "on dispatch"
               --  transition to execute (index_transition_to_execute)
               --  received from activity.c
               --  if (index_transition_to_execute == 1)
               --  for each on dispatch transition, Actions are mapped
               --  in the if block statements.
               --  then we update the current state with the destination
               --  state of the transition. **/
               --

               Behav_Transition := BATN.First_Node
                 (BATN.Transitions (BA));
               while Present (Behav_Transition) loop

                  Transition_Node := BATN.Transition (Behav_Transition);

                  if BATN.Kind (Transition_Node) =
                    BATN.K_Execution_Behavior_Transition
                  then
                     if BANu.Length (BATN.Sources (Transition_Node)) = 1
                     then
                        Source := BATN.First_Node
                          (BATN.Sources (Transition_Node));
                        if  (Standard.Utils.To_Upper
                             (BATN.Display_Name (List_Node)) =
                               Standard.Utils.To_Upper
                                 (BATN.Display_Name (Source)))
                        then
                           if Present (BATN.Behavior_Condition
                                       (Transition_Node)) and then
                             Present (BATN.Condition
                                      (BATN.Behavior_Condition
                                         (Transition_Node))) and then

                             BATN.Kind
                               (BATN.Condition
                                  (Behavior_Condition (Transition_Node)))
                               = BATN.K_Dispatch_Condition_Thread
                           then
                              BATN.Set_Next_Node (Transition_Node, No_Node);
                              BANu.Append_Node_To_List
                                (Transition_Node,
                                 Sub_Transition_List);
                           end if;
                        end if;
                     end if;
                  end if;
                  Behav_Transition := BATN.Next_Node (Behav_Transition);
               end loop;

               Map_C_A_List_Of_On_Dispatch_Transitions
                 (S, BA, Sub_Transition_List,
                  Declarations, Switch_Statements);

               N :=
                 Make_Switch_Alternative (Switch_Labels,
                                          Switch_Statements);

               CTU.Append_Node_To_List (N, Switch_Alternatives);

               List_Node := BATN.Next_Node (List_Node);
            end loop;
         end if;

         State := BATN.Next_Node (State);
      end loop;

      Append_Node_To_List
        (Make_Switch_Alternative (No_List, No_List),
         Switch_Alternatives);

      N :=
        Make_Switch_Statement
          (Expression   => Make_Member_Designator
             (Defining_Identifier => Make_Defining_Identifier
                (MN (M_Name)),
              Aggregate_Name      => Make_Defining_Identifier
                (Map_C_Variable_Name (E, Current_State => True))),
           Alternatives => Switch_Alternatives);

      CTU.Append_Node_To_List (N, Statements);

      --  2) Now we examine __po_hi_producer_current_state if
      --  it is not a « complete » state, i.e. it is an « execution »
      --  state, then we should proceed transitions until reaching
      --  a « complete » state.
      --

      N := Message_Comment
        ("2) Now we examine current_state if "
           & "it is not a 'complete' state, i.e. it is an 'execution' "
         & " state, then we should proceed transitions until reaching"
         & " a 'complete' state.");
      CTU.Append_Node_To_List (N, Statements);

      Examine_Current_State_Until_Reaching_Complete_State
        (S, BA, Declarations, Statements);

      Map_C_Implementation_of_BA_Body_Function (S, Declarations, Statements);

   end Make_BA_Body_Function_For_Sporadic_Thread;

   -----------------------------------------------
   -- Make_BA_Body_Function_For_Periodic_Thread --
   -----------------------------------------------

   procedure Make_BA_Body_Function_For_Periodic_Thread
     (S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      BA                         : Node_Id;
      Behav_Transition           : Node_Id;
      N                          : Node_Id;
      Dispatch_Transition        : Node_Id := No_Node;
      Transition_Node            : Node_Id;
      E            : constant Node_Id := AIN.Parent_Subcomponent (S);
   begin
      --  void <<thread_name>>_ba_body
      --      (__po_hi_task_id self)
      --  {
      --    base_types__integer tmp;
      --    test__ba__backend__alpha_type tmp2;
      --
      --    /** 1) mapping actions of the transition having the
      --    « on dispatch » condition **/
      --
      --    ...
      --
      --    /** 2) update the current state **/
      --    __po_hi_producer_current_state = __po_hi_producer_states_array[1];
      --
      --    /** 3) Now we examine __po_hi_producer_current_state if
      --    it is not a « complete » state, i.e. it is an « execution » state,
      --    then we should proceed transitions until reaching
      --    a « complete » state. **/
      --

      BA := Get_Behavior_Specification (S);

      Map_C_Behavior_Variables (S, Declarations);

      Behav_Transition := BATN.First_Node (BATN.Transitions (BA));

      while Present (Behav_Transition) loop
         Transition_Node := BATN.Transition (Behav_Transition);

         if Present (Behavior_Condition (Transition_Node))
           and then Present (BATN.Condition
                             (Behavior_Condition (Transition_Node)))
           and then
             BATN.Kind (BATN.Condition (Behavior_Condition (Transition_Node)))
           = BATN.K_Dispatch_Condition_Thread
         then
            Dispatch_Transition := Transition_Node;
         end if;

         exit when Present (Dispatch_Transition);

         Behav_Transition := BATN.Next_Node (Behav_Transition);
      end loop;

      if Present (Dispatch_Transition) then
         if Present (BATN.Behavior_Action_Block (Dispatch_Transition))
           and then Present (BATN.Behav_Acts
                             (BATN.Behavior_Action_Block
                                (Dispatch_Transition)))
         then
            --  1) mapping actions of the transition having
            --  the « on dispatch » condition

            N := Message_Comment
              ("1) Mapping actions of the transition having "
               & " the 'on dispatch' condition.");

            CTU.Append_Node_To_List (N, Statements);

            Map_C_Behavior_Action_Block
              (BATN.Behavior_Action_Block (Dispatch_Transition),
               S, Declarations, Statements);
         end if;

         --  2) update the current state
         Update_Current_State (E, BA, Dispatch_Transition, Statements);

         --  3) Now we examine __po_hi_producer_current_state if
         --  it is not a « complete » state, i.e. it is an « execution »
         --  state,then we should proceed transitions until reaching
         --  a « complete » state.
         --
         if Behavior_State_Kind'Val
           (Search_State_Kind (BA, BATN.Destination (Dispatch_Transition)))
           = BSK_No_Kind
         then
            N := Message_Comment
              ("3) As the current state is execution state "
               & " then we should proceed transitions until reaching"
               & " a 'complete' state.");

            CTU.Append_Node_To_List (N, Statements);
            Examine_Current_State_Until_Reaching_Complete_State
              (S, BA, Declarations, Statements);
         end if;
      end if;

      Map_C_Implementation_of_BA_Body_Function (S, Declarations, Statements);

   end Make_BA_Body_Function_For_Periodic_Thread;

   ----------------------------------------
   -- Map_C_Many_Transitions_Of_A_Thread --
   ----------------------------------------

   procedure Map_C_Many_Transitions_Of_A_Thread
     (S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      P  : constant Supported_Thread_Dispatch_Protocol :=
        Get_Thread_Dispatch_Protocol (S);
      Nb_On_Dispatch_Transitions : constant Unsigned_Long_Long :=
        Compute_Nb_On_Dispatch_Transitions (S);
   begin

      Map_BA_States_To_C_Types (S);

      if P = Thread_Sporadic and then
        Nb_On_Dispatch_Transitions > 1
      then
         Make_Update_Next_Complete_State_Function (S);
      end if;

      Make_States_Initialization_Function (S);

      if Is_To_Make_Init_Sequence (S) then
         Make_BA_Initialization_Function (S);
      end if;

      if P = Thread_Periodic then

         Make_BA_Body_Function_For_Periodic_Thread
           (S, Declarations, Statements);

      elsif P = Thread_Sporadic then

         Make_BA_Body_Function_For_Sporadic_Thread
           (S, Declarations, Statements);
      end if;

   end Map_C_Many_Transitions_Of_A_Thread;

   ---------------------------------
   -- Map_C_Behavior_Action_Block --
   ---------------------------------

   procedure Map_C_Behavior_Action_Block
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Behavior_Action_Block);
   begin

      if Present (BATN.Behav_Acts (Node)) then
         Map_C_Behav_Acts (Node, S, Declarations, Statements);
      end if;

      --  Behavior_Time (Node) is not yet supported
      --
      --  if Present (Behavior_Time (Node)) then
      --
      --  end if;

   end Map_C_Behavior_Action_Block;

   ----------------------
   -- Map_C_Behav_Acts --
   ----------------------

   procedure Map_C_Behav_Acts
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      WStatements  : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Behavior_Action_Block
                     or else BATN.Kind (Node) = K_Conditional_Statement
                     or else BATN.Kind (Node) = K_While_Cond_Structure
                     or else BATN.Kind (Node) = K_For_Cond_Structure
                     or else BATN.Kind (Node) = K_ForAll_Cond_Structure);

      pragma Assert (Present (BATN.Behav_Acts (Node)));

      Behav_actions   : Node_Id;
      Behav_action    : Node_Id;
   begin

      Behav_actions := BATN.Behav_Acts (Node);

      --  In the case of a sequence of behavior actions:
      --  It is mapped into a sequence of C-statement, i.e.
      --  each action is mapped to the corresponding C-statement.
      --  Statement sequence is writen in the same order as the
      --  action sequence.
      --
      if not BANu.Is_Empty (BATN.Behavior_Action_Sequence (Behav_actions))
      then

         Behav_action := BATN.First_Node
           (BATN.Behavior_Action_Sequence (Behav_actions));

         while Present (Behav_action) loop
            Map_C_Behavior_Action (Behav_action, S, Declarations, WStatements);
            Behav_action := BATN.Next_Node (Behav_action);
         end loop;
      end if;

      --  In the case of a set of behavior actions:
      --  we treat it as a sequence of behavior actions.
      --
      if not BANu.Is_Empty (BATN.Behavior_Action_Set (Behav_actions)) then
         Behav_action := BATN.First_Node
           (BATN.Behavior_Action_Set (Behav_actions));

         while Present (Behav_action) loop
            Map_C_Behavior_Action (Behav_action, S, Declarations, WStatements);
            Behav_action := BATN.Next_Node (Behav_action);
         end loop;
      end if;

      --  In the case of a single action: it is mapped
      --  into a C-statement.
      --
      if Present (BATN.Behavior_Action (Behav_actions)) and then
        BANu.Is_Empty (BATN.Behavior_Action_Sequence (Behav_actions))
        and then BANu.Is_Empty (BATN.Behavior_Action_Set (Behav_actions))
      then

         Map_C_Behavior_Action (BATN.Behavior_Action (Behav_actions), S,
                                Declarations, WStatements);
      end if;

   end Map_C_Behav_Acts;

   ---------------------------
   -- Map_C_Behavior_Action --
   ---------------------------

   procedure Map_C_Behavior_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Behavior_Action);
      pragma Assert (BATN.Kind (BATN.Action (Node)) = BATN.K_If_Cond_Struct
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_For_Cond_Structure
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_While_Cond_Structure
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_ForAll_Cond_Structure
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_DoUntil_Cond_Structure
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_Assignment_Action
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_Communication_Action
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_Timed_Act);

      Action_Node : constant Node_Id := BATN.Action (Node);
      N           : Node_Id;
   begin
      case BATN.Kind (Action_Node) is

         when K_If_Cond_Struct         =>
            Map_C_If_Cond_Struct (Action_Node, S, Declarations, Statements);

         when K_For_Cond_Structure     =>
            Map_C_For_or_ForAll_Cond_Struct
              (Action_Node, S, Declarations, Statements);

         when K_While_Cond_Structure   =>
            Map_C_While_Cond_Struct (Action_Node, S, Declarations, Statements);

         when K_ForAll_Cond_Structure  =>
            Map_C_For_or_ForAll_Cond_Struct
              (Action_Node, S, Declarations, Statements);

            --  when K_DoUntil_Cond_Structure =>
            --    Map_C_DoUntil_Cond_Struct (Action_Node);

         when BATN.K_Assignment_Action      =>
            Map_C_Assignment_Action (Action_Node, S, Declarations, Statements);

         when K_Communication_Action   =>
            Map_C_Communication_Action (Action_Node, S,
                                        Declarations, Statements);

            --  when K_Timed_Act           =>
            --    Map_C_Timed_Action (Action_Node);

         when others                   =>
            N := Message_Comment ("Behavior Actions not yet mapped");
            CTU.Append_Node_To_List (N, Statements);
      end case;

   end Map_C_Behavior_Action;

   ----------------------
   -- Map_C_Elsif_stat --
   ----------------------

   function Map_C_Elsif_stat
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id;
      Else_st      : Node_Id) return List_Id
   is
      N                : Node_Id;
      Else_stats       : constant List_Id := New_List (CTN.K_Statement_List);
      elsif_statements : constant List_Id := New_List (CTN.K_Statement_List);
   begin
      N := BATN.Next_Node (Node);

      if Present (N) then
         Map_C_Behav_Acts
           (Node         => Node,
            S            => S,
            Declarations => Declarations,
            WStatements  => elsif_statements);

         CTU.Append_Node_To_List
           (CTU.Make_If_Statement
              (Condition       => Evaluate_BA_Value_Expression
                   (Node             => Logical_Expr (Node),
                    Subprogram_Root  => S,
                    Declarations     => Declarations,
                    Statements       => Statements),
               Statements      => elsif_statements,
               Else_Statements =>
                 Map_C_Elsif_stat
                   (Node         => N,
                    S            => S,
                    Declarations => Declarations,
                    Statements   => Statements,
                    Else_st      => Else_st)),
            Else_stats);
      else
         Map_C_Behav_Acts
           (Node         => Node,
            S            => S,
            Declarations => Declarations,
            WStatements  => elsif_statements);
         declare
            else_sts : constant List_Id := New_List (CTN.K_Statement_List);
         begin
            if Present (Else_st) then

               Map_C_Behav_Acts
                 (Node         => Else_st,
                  S            => S,
                  Declarations => Declarations,
                  WStatements  => else_sts);

               CTU.Append_Node_To_List
                 (CTU.Make_If_Statement
                    (Condition       => Evaluate_BA_Value_Expression
                         (Node             => Logical_Expr (Node),
                          Subprogram_Root  => S,
                          Declarations     => Declarations,
                          Statements       => Statements),
                     Statements      => elsif_statements,
                     Else_Statements => else_sts),
                  Else_stats);
            else
               CTU.Append_Node_To_List
                 (CTU.Make_If_Statement
                    (Condition       => Evaluate_BA_Value_Expression
                         (Node             => Logical_Expr (Node),
                          Subprogram_Root  => S,
                          Declarations     => Declarations,
                          Statements       => Statements),
                     Statements      => elsif_statements),
                  Else_stats);
            end if;
         end;
      end if;

      return Else_stats;

   end Map_C_Elsif_stat;

   --------------------------
   -- Map_C_If_Cond_Struct --
   --------------------------

   procedure Map_C_If_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is

      pragma Assert (BATN.Kind (Node) = K_If_Cond_Struct);

      pragma Assert (Present (Logical_Expr (If_Statement (Node))));
      pragma Assert (Present (Behav_Acts (If_Statement (Node))));

      Condition        : Node_Id;
      else_Statements  : List_Id;
      if_Statements    : constant List_Id := New_List (CTN.K_Statement_List);
      List_Node1       : Node_Id;
   begin

      Condition := Evaluate_BA_Value_Expression
        (Node             => Logical_Expr (If_Statement (Node)),
         Subprogram_Root  => S,
         Declarations     => Declarations,
         Statements       => Statements);

      Map_C_Behav_Acts
        (Node         => If_Statement (Node),
         S            => S,
         Declarations => Declarations,
         WStatements  => if_Statements);

      if not BANu.Is_Empty (Elsif_Statement (Node)) then

         List_Node1 := BATN.First_Node (Elsif_Statement (Node));

         else_statements := Map_C_Elsif_stat (List_Node1, S, Declarations,
                           Statements, Else_Statement (Node));
      else
         if Present (Else_Statement (Node)) then
            else_statements := New_List (CTN.K_Statement_List);
            Map_C_Behav_Acts
              (Node         => Else_Statement (Node),
               S            => S,
               Declarations => Declarations,
               WStatements  => else_statements);
         else
            else_statements := No_List;
         end if;
      end if;

      CTU.Append_Node_To_List
        (CTU.Make_If_Statement
           (Condition       => Condition,
            Statements      => if_Statements,
            Else_Statements => else_statements),
         Statements);

   end Map_C_If_Cond_Struct;

   -------------------------------------
   -- Map_C_For_or_ForAll_Cond_Struct --
   -------------------------------------

   procedure Map_C_For_or_ForAll_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is

      pragma Assert (BATN.Kind (Node) = K_For_Cond_Structure
                     or else BATN.Kind (Node) = K_ForAll_Cond_Structure);

      Pre_Cond       : Node_Id;
      Post_Cond      : Node_Id;
      Used_Type      : Node_Id;
      init_value     : Node_Id;
      Condition      : Node_Id;
      For_Statements : constant List_Id := New_List (CTN.K_Statement_List);
   begin

      if BATN.Kind (In_Element_Values (Node)) = BATN.K_Integer_Range then

         Used_Type := Map_Used_Type (BATN.Corresponding_Declaration
                                     (BATN.Classifier_Ref (Node)));

         init_value := Evaluate_BA_Integer_Value
           (BATN.Lower_Int_Val (In_Element_Values (Node)),
            S,
            Declarations,
            Statements);

         Pre_Cond := Make_Variable_Declaration
           (Defining_Identifier => Make_Defining_Identifier
              (BATN.Display_Name (Element_Idt (Node))),
            Used_Type           => Used_Type,
            Value               => init_value);

         Condition := CTU.Make_Expression
           (Left_Expr  => Make_Defining_Identifier
              (BATN.Display_Name (Element_Idt (Node))),
            Operator   => CTU.Op_Less_Equal,
            Right_Expr => Evaluate_BA_Integer_Value
              (BATN.Upper_Int_Val (In_Element_Values (Node)),
               S,
               Declarations,
               Statements));

         Post_Cond := CTU.Make_Expression
              (Left_Expr  => Make_Defining_Identifier
                   (BATN.Display_Name (Element_Idt (Node))),
               Operator   => CTU.Op_Plus_Plus,
               Right_Expr => No_Node);

         Map_C_Behav_Acts
           (Node         => Node,
            S            => S,
            Declarations => Declarations,
            WStatements  => For_Statements);

         CTU.Append_Node_To_List
           (CTU.Make_For_Statement
              (Pre_Cond   => Pre_Cond,
               Condition  => Condition,
               Post_Cond  => Post_Cond,
               Statements => For_Statements),
            Statements);

      else
         Display_Error ("In For/ForAll construct, Kinds"
                        & " other than K_Integer_Range for In_Element_Values"
                        & " are not yet supported", Fatal => True);
      end if;

   end Map_C_For_or_ForAll_Cond_Struct;

   -----------------------------
   -- Map_C_While_Cond_Struct --
   -----------------------------

   procedure Map_C_While_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is

      pragma Assert (BATN.Kind (Node) = K_While_Cond_Structure);

      pragma Assert (Present (Logical_Expr (Node)));
      pragma Assert (Present (Behav_Acts (Node)));

      Condition        : Node_Id;
      while_statements : constant List_Id := New_List (CTN.K_Statement_List);
   begin

      Condition := Evaluate_BA_Value_Expression
        (Node             => Logical_Expr (Node),
         Subprogram_Root  => S,
         Declarations     => Declarations,
         Statements       => Statements);

      Map_C_Behav_Acts
        (Node         => Node,
         S            => S,
         Declarations => Declarations,
         WStatements  => while_statements);

      CTU.Append_Node_To_List
        (CTU.Make_While_Statement
           (Condition  => Condition,
            Statements => while_statements),
         Statements);

   end Map_C_While_Cond_Struct;

   --------------------------------
   -- Map_C_Communication_Action --
   --------------------------------

   procedure Map_C_Communication_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Communication_Action);

      Called_Spg               : Node_Id;
      Called_Spg_Instance      : Node_Id;
      Called_Spg_Spec          : Node_Id;
      Var_identifier           : Node_Id;
      Call_Parameters          : List_Id;
      N, k                     : Node_Id;
      Param_Node               : Node_Id;
      decl                     : Node_Id;
      Called_Spg_Spec_Exist    : Boolean := False;
   begin

      if Kind (BATN.Identifier (Node)) = BATN.K_Name then
         if BANu.Length (BATN.Idt (BATN.Identifier (Node))) = 1 then
            Var_identifier := CTU.Make_Defining_Identifier
              (Name => BATN.Display_Name
                 (BATN.First_Node
                      (BATN.Idt (BATN.Identifier (Node)))),
               Pointer => False);
         else
            Var_identifier := Map_C_BA_Name
              (Node         => BATN.Identifier (Node),
               S            => S,
               Declarations => Declarations,
               Statements   => Statements);
         end if;

      elsif Kind (BATN.Identifier (Node)) = BATN.K_Data_Component_Reference
      then
         Var_identifier := Map_C_Data_Component_Reference
           (Node         => BATN.Identifier (Node),
            S            => S,
            Declarations => Declarations,
            Statements   => Statements);
      end if;

      case Communication_Kind'Val (Comm_Kind (Node)) is

         when CK_Exclamation      =>

            if Is_Subprogram_Call (Node) then

               --  This means we have a call to a subprogram
               --

               --  If not yet added to the declarations of the
               --  current source file we must add the definition
               --  of the called spg

               --  First, we search if the called Spg is already declared
               --  in the current file, i.e. the called spg is not the
               --  first time is called
               --
               decl := CTN.First_Node (CTN.Declarations (Current_File));
               while Present (decl) loop

                  if Kind (decl) = CTN.K_Function_Specification
                    and then
                      Get_Name_String
                        (Standard.Utils.To_Lower
                           (CTN.Name (CTN.Defining_Identifier
                            (decl))))
                    = Get_Name_String
                    (Standard.Utils.To_Lower
                       (CTN.Name (Var_identifier)))

                  then
                     Called_Spg_Spec_Exist := True;
                  end if;
                  exit when Called_Spg_Spec_Exist;
                  decl := CTN.Next_Node (decl);
               end loop;

               if not Called_Spg_Spec_Exist then

                  Called_Spg :=
                    BATN.Corresponding_Entity
                      (BATN.First_Node
                         (BATN.Idt (BATN.Identifier (Node))));

                  Called_Spg_Instance := AAN.Default_Instance (Called_Spg);

                  declare
                     Proxy_Instance : Node_Id;
                  begin
                     if AINU.Is_Thread (S) then
                        Proxy_Instance := S;
                     elsif AINU.Is_Subprogram (S) then
                        Proxy_Instance := Get_Container_Thread (S);
                     end if;

                     if AIN.Subcomponents (Proxy_Instance) = No_List then
                        AIN.Set_Subcomponents
                          (Proxy_Instance,
                           AINU.New_List (AIN.K_List_Id,
                                          AIN.Loc (Proxy_Instance)));
                     end if;

                     declare
                        The_Sub : constant Node_Id :=
                          AINU.New_Node (AIN.K_Subcomponent_Instance,
                                         AIN.Loc (Proxy_Instance));
                     begin
                        AIN.Set_Parent_Component
                          (The_Sub, Called_Spg_Instance);
                        AIN.Set_Corresponding_Instance
                          (The_Sub, Called_Spg_Instance);

                        AIN.Set_Parent_Subcomponent
                          (Called_Spg_Instance,
                           (Proxy_Instance));
                        AINU.Append_Node_To_list
                          (The_Sub,
                           AIN.Subcomponents (Proxy_Instance));
                     end;
                  end;

                  Called_Spg_Spec := Map_C_Subprogram_Spec
                    (Called_Spg_Instance);

                  Set_Defining_Identifier
                    (Called_Spg_Spec,
                     Var_identifier);

                  Append_Node_To_List
                    (Called_Spg_Spec,
                     CTN.Declarations (Current_File));
               end if;

               --  Then, call the function provided by the user in our
               --  subprogram.

               if BANu.Is_Empty (Subprogram_Parameter_List (Node)) then

                  N := Make_Call_Profile (Var_identifier);

               else

                  Call_Parameters := New_List (CTN.K_Parameter_List);
                  N := BATN.First_Node (Subprogram_Parameter_List (Node));

                  while Present (N) loop
                     Param_Node := Parameter (N);

                     case BATN.Kind (Param_Node) is

                     when K_Value_Expression =>

                        K := Evaluate_BA_Value_Expression
                          (Node             => Param_Node,
                           Is_Out_Parameter => BATN.Is_Out (N),
                           Subprogram_Root  => S,
                           Declarations     => Declarations,
                           Statements       => Statements,
                           Is_Put_Value_On_Port => True);

                        Append_Node_To_List
                          (K,
                           Call_Parameters);

                     when K_Name =>
                        Append_Node_To_List
                          (Map_C_BA_Name
                             (Node             => Param_Node,
                              Is_Out_Parameter => BATN.Is_Out (N),
                              S                => S,
                              Declarations     => Declarations,
                              Statements       => Statements),
                           Call_Parameters);

                     when K_Data_Component_Reference =>
                        Append_Node_To_List
                          (Map_C_Data_Component_Reference
                             (Node             => Param_Node,
                              Is_Out_Parameter => BATN.Is_Out (N),
                              S                => S,
                              Declarations     => Declarations,
                              Statements       => Statements),
                           Call_Parameters);

                     when others =>
                        Display_Error ("Other param label Kinds are not"
                                       & " supported", Fatal => True);
                     end case;

                     N := BATN.Next_Node (N);

                  end loop;

                  N := Make_Call_Profile
                    (Defining_Identifier => Var_identifier,
                     Parameters          => Call_Parameters);
               end if;

               CTU.Append_Node_To_List (N, Statements);

            else
               --  It is a port sending action

               if not BANu.Is_Empty (Subprogram_Parameter_List (Node))
                 and then BANu.Length (Subprogram_Parameter_List (Node)) = 1
               then
                  --  Declare request variable if it is not yet declared
                  Make_Request_Variable_Declaration
                    (Declarations,
                     BATN.Display_Name
                       (BATN.First_Node
                            (BATN.Idt (BATN.Identifier (Node)))));

                  Make_Output_Port_Name
                    (Node         => BATN.Identifier (Node),
                     S            => S,
                     Statements   => Statements);

                  Make_Put_Value_On_port (Node, S, Declarations, Statements);
               end if;

               --  Declare request variable if it is not yet declared
               Make_Request_Variable_Declaration
                 (Declarations,
                  BATN.Display_Name
                    (BATN.First_Node
                         (BATN.Idt (BATN.Identifier (Node)))));

               Make_Send_Output_Port (Node, S, Statements);

            end if;

         when CK_Interrogative    =>
            if No (Target (Node)) then
               Make_Next_Value_of_Port
                 (BATN.First_Node (BATN.Idt (BATN.Identifier (Node))),
                  S, Statements);
            else
               CTU.Append_Node_To_List
                 (Make_Assignment_Statement
                    (Variable_Identifier => Map_C_Target
                         (Node         => BATN.Target (Node),
                          S            => S,
                          Declarations => Declarations,
                          Statements   => Statements),
                     Expression          => Make_Get_Value_of_Port
                       (BATN.First_Node (BATN.Idt (BATN.Identifier (Node))),
                        S, Declarations, Statements)),
                  Statements);

               Make_Next_Value_of_Port
                 (BATN.First_Node (BATN.Idt (BATN.Identifier (Node))),
                  S, Statements);
            end if;

            --  when CK_Greater_Greater  =>
            --  when CK_Exclamation_Greater =>
            --  when CK_Exclamation_Lesser  =>

         when others              =>
            Display_Error ("Other Communication Action Kinds"
                           & " are not yet supported",
                           Fatal => True);
      end case;

   end Map_C_Communication_Action;

   ---------------------------
   -- Make_Output_Port_Name --
   ---------------------------

   procedure Make_Output_Port_Name
     (Node         : Node_Id;
      S            : Node_Id;
      Statements   : List_Id)
   is
      N                 : Node_Id;
      Statement_Exist   : Boolean := False;
      Stat              : Node_Id;
      Request_Name      : Name_Id;
      Idt               : Name_Id;
   begin
      if BATN.Kind (Node) = BATN.K_Identifier then
         Idt := BATN.Display_Name (Node);
      else
         Idt := BATN.Display_Name (BATN.First_Node
                                   (BATN.Idt (Node)));
      end if;

      Request_Name := Make_Request_Variable_Name_From_Port_Name (Idt);

      Stat := CTN.First_Node (Statements);
      while Present (Stat) loop

         if CTN.Kind (Stat) = CTN.K_Assignment_Statement
           and then
             CTN.Kind (CTN.Defining_Identifier (Stat))
               = CTN.K_Member_Designator
             and then
               CTN.Kind (CTN.Aggregate_Name
                         (CTN.Defining_Identifier (Stat)))
           = CTN.K_Defining_Identifier
         then
            if Get_Name_String
              (Standard.Utils.To_Lower
                 (CTN.Name
                  (CTN.Aggregate_Name (CTN.Defining_Identifier (Stat)))))
                = Get_Name_String
              (Standard.Utils.To_Lower (Request_Name))
              and then
                Get_Name_String
                  (Standard.Utils.To_Lower
                     (CTN.Name
                        (CTN.Defining_Identifier
                             (CTN.Defining_Identifier (Stat)))))
                  = Get_Name_String
              (Standard.Utils.To_Lower (MN (M_Port)))

            then
               Statement_Exist := True;
            end if;
         end if;

         exit when Statement_Exist;
         Stat := CTN.Next_Node (Stat);
      end loop;

      if not Statement_Exist then
         --  Generate the following code :
         --  request.port =
         --           REQUEST_PORT (thread_instance_name, port_name);

         N := Message_Comment (" The name of an output port is built"
                               & " from the thread_instance name"
                               & " and the port name using"
                               & " the REQUEST_PORT macro. ");
         Append_Node_To_List (N, Statements);

         CTU.Append_Node_To_List
           (Make_Assignment_Statement
              (Variable_Identifier => Make_Member_Designator
                   (Defining_Identifier =>
                        Make_Defining_Identifier (MN (M_Port)),
                    Aggregate_Name      =>
                      Make_Defining_Identifier
                        (Request_Name)),
               Expression          => Make_Call_Profile
                 (RE (RE_REQUEST_PORT),
                  Make_List_Id
                    (Make_Defining_Identifier
                         (Map_Thread_Port_Variable_Name (S)),
                     Make_Defining_Identifier (Idt)))),
            Statements);
      end if;

   end Make_Output_Port_Name;

   ----------------------------
   -- Make_Put_Value_On_port --
   ----------------------------

   procedure Make_Put_Value_On_port
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      N : Node_Id;
   begin
      N := Message_Comment (" The name of the corresponding"
                            & " port variable is built from"
                            & " the port name,"
                            & " following similar pattern. ");
      Append_Node_To_List (N, Statements);

      N := Make_Call_Profile
        (RE (RE_PORT_VARIABLE),
         Make_List_Id
           (Make_Defining_Identifier
                (Map_Thread_Port_Variable_Name (S)),
            Make_Defining_Identifier
              (BATN.Display_Name (BATN.First_Node
               (BATN.Idt (BATN.Identifier (Node)))))));

      CTU.Append_Node_To_List
        (Make_Assignment_Statement
           (Variable_Identifier => Make_Member_Designator
                (Defining_Identifier => N,
                 Aggregate_Name      =>
                   Make_Defining_Identifier
                     (Make_Request_Variable_Name_From_Port_Name
                        (BATN.Display_Name (BATN.First_Node
                         (BATN.Idt (BATN.Identifier (Node))))))),
            Expression          => Evaluate_BA_Value_Expression
              (Node             => Parameter
                   (BATN.First_Node
                        (Subprogram_Parameter_List (Node))),
               Subprogram_Root  => S,
               Declarations     => Declarations,
               Statements       => Statements,
               Is_Put_Value_On_Port => True)),
         Statements);

   end Make_Put_Value_On_port;

   ---------------------------
   -- Make_Send_Output_Port --
   ---------------------------

   procedure Make_Send_Output_Port
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id)
   is
      N               : Node_Id;
      N1              : Name_Id;
      Call_Parameters : List_Id;
   begin

      N := Message_Comment (" Send the request through the thread "
                            & " *local* port,"
                            & " built from the instance name"
                            & " and the port name using"
                            & " the LOCAL_PORT macro. ");
      Append_Node_To_List (N, Statements);

      --  __po_hi_gqueue_store_out
      --    (self,
      --     LOCAL_PORT (<<thread_name>>, <<port_name>>),
      --     &__<<port_name>>_request);

      Call_Parameters := New_List (CTN.K_Parameter_List);

      Set_Str_To_Name_Buffer ("self");
      N1 := Name_Find;
      N := Make_Defining_Identifier (N1);
      Append_Node_To_List (N, Call_Parameters);

      Append_Node_To_List
        (Make_Call_Profile
           (RE (RE_Local_Port),
            Make_List_Id
              (Make_Defining_Identifier
                   (Map_Thread_Port_Variable_Name (S)),
               Make_Defining_Identifier
                 (BATN.Display_Name (BATN.First_Node
                  (BATN.Idt (BATN.Identifier (Node))))))),
         Call_Parameters);

      N :=
        Make_Variable_Address
          (Make_Defining_Identifier
             (Make_Request_Variable_Name_From_Port_Name
                (BATN.Display_Name (BATN.First_Node
                 (BATN.Idt (BATN.Identifier (Node)))))));
      Append_Node_To_List (N, Call_Parameters);

      N :=
        CTU.Make_Call_Profile
          (RE (RE_Gqueue_Store_Out),
           Call_Parameters);
      Append_Node_To_List (N, Statements);

      --  __po_hi_send_output
      --    (self,REQUEST_PORT(<<thread_name>>, <<port_name>>));

      Call_Parameters := New_List (CTN.K_Parameter_List);

      Append_Node_To_List (Make_Defining_Identifier (N1), Call_Parameters);

      Append_Node_To_List
        (Make_Call_Profile
           (RE (RE_REQUEST_PORT),
            Make_List_Id
              (Make_Defining_Identifier
                   (Map_Thread_Port_Variable_Name (S)),
               Make_Defining_Identifier
                 (BATN.Display_Name (BATN.First_Node
                  (BATN.Idt (BATN.Identifier (Node))))))),
         Call_Parameters);

      N := Make_Call_Profile
        (RE (RE_Send_Output),
         Call_Parameters);

      Append_Node_To_List (N, Statements);

   end Make_Send_Output_Port;

   ------------------------------------
   -- Map_C_Data_Component_Reference --
   ------------------------------------

   function Map_C_Data_Component_Reference
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Data_Component_Reference);

      Var_identifier : Node_Id := No_Node;
      N              : Node_Id;
   begin
      if not BANu.Is_Empty (BATN.Identifiers (Node)) then
         N := BATN.First_Node (BATN.Identifiers (Node));
         Var_identifier := Map_C_BA_Name
           (Node             => N,
            Is_Out_Parameter => Is_Out_Parameter,
            S                => S,
            Declarations     => Declarations,
            Statements       => Statements);
         N := BATN.Next_Node (N);
         while Present (N) loop
            Var_identifier := CTU.Make_Member_Designator
              (Defining_Identifier => Map_C_BA_Name
                 (Node             => N,
                  Is_Out_Parameter => Is_Out_Parameter,
                  S                => S,
                  Declarations     => Declarations,
                  Statements       => Statements),
               Aggregate_Name      => Var_identifier);
            N := BATN.Next_Node (N);
         end loop;
      end if;

      return Var_identifier;
   end Map_C_Data_Component_Reference;

   -------------------
   -- Map_C_BA_Name --
   -------------------

   function Map_C_BA_Name
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      use AAN;
      pragma Assert (BATN.Kind (Node) = BATN.K_Name);

      Var_identifier, Next_Ident  : Node_Id := No_Node;
      Corresponding_Entity        : Node_Id;
      N, N1                       : Node_Id;
      Is_Pointer, Next_Is_Pointer : Boolean := False;
   begin
      if not BANu.Is_Empty (BATN.Idt (Node)) then
         N := BATN.First_Node (BATN.Idt (Node));

         Corresponding_Entity := BATN.Corresponding_Entity (N);

         if present (Corresponding_Entity) then

            if AAN.Kind (Corresponding_Entity) = AAN.K_Parameter
              and then AAN.Is_Out (Corresponding_Entity)
            then

               Var_identifier := Evaluate_BA_Identifier
                 (Node             => N,
                  Is_Out_Parameter => True,
                  Subprogram_Root  => S,
                  Declarations     => Declarations,
                  Statements       => Statements);

               Is_Pointer := True;

            elsif AAN.Kind (Corresponding_Entity) = AAN.K_Port_Spec
              and then AAN.Is_Out (Corresponding_Entity)
            then
               --  Declare request variable if it is not yet declared
               Make_Request_Variable_Declaration
                 (Declarations,
                  BATN.Display_Name (N));

               Make_Output_Port_Name
                 (Node         => Node,
                  S            => S,
                  Statements   => Statements);

               --  N1 := Message_Comment (" The name of the corresponding"
               --                        & " port variable is built from"
               --                        & " the port name,"
               --                        & " following similar pattern. ");
               --  Append_Node_To_List (N1, Statements);

               N1 := Make_Call_Profile
                 (RE (RE_PORT_VARIABLE),
                  Make_List_Id
                    (Make_Defining_Identifier
                         (Map_Thread_Port_Variable_Name (S)),
                     Make_Defining_Identifier
                       (BATN.Display_Name (N))));

               Var_identifier := CTU.Make_Member_Designator
                 (Defining_Identifier => N1,
                  Aggregate_Name      =>
                    Make_Defining_Identifier
                      (Make_Request_Variable_Name_From_Port_Name
                           (BATN.Display_Name (N))));

            elsif AAN.Kind (Corresponding_Entity) = AAN.K_Subcomponent
              and then
                AINU.Is_Data (AIN.Corresponding_Instance
                              (Get_Subcomponent_Data_Instance
                                 (Corresponding_Entity, S)))
              and then Get_Data_Representation
                (AIN.Corresponding_Instance
                   (Get_Subcomponent_Data_Instance
                      (Corresponding_Entity, S))) = Data_Struct

            then
               Var_identifier := Evaluate_BA_Identifier
                 (Node             => N,
                  Is_Out_Parameter => Is_Out_Parameter,
                  Subprogram_Root  => S,
                  Declarations     => Declarations,
                  Statements       => Statements);
               Is_Pointer := True;
            end if;

         else
            Var_identifier := Evaluate_BA_Identifier
              (Node             => N,
               Is_Out_Parameter => Is_Out_Parameter,
               Subprogram_Root  => S,
               Declarations     => Declarations,
               Statements       => Statements);
            Is_Pointer := False;
         end if;

         N := BATN.Next_Node (N);

         while Present (N) loop

            Corresponding_Entity := BATN.Corresponding_Entity (N);
            if Present (Corresponding_Entity) then
               if AAN.Kind (Corresponding_Entity) = AAN.K_Parameter
                 and then AAN.Is_Out (Corresponding_Entity)
               then
                  Next_Ident := Evaluate_BA_Identifier
                    (Node             => N,
                     Is_Out_Parameter => True,
                     Subprogram_Root  => S,
                     Declarations     => Declarations,
                     Statements       => Statements);

                  Next_Is_Pointer := True;
               end if;
            else
               Next_Ident := Evaluate_BA_Identifier
                 (Node             => N,
                  Is_Out_Parameter => Is_Out_Parameter,
                  Subprogram_Root  => S,
                  Declarations     => Declarations,
                  Statements       => Statements);
               Next_Is_Pointer := False;
            end if;

            Var_identifier := CTU.Make_Member_Designator
              (Defining_Identifier => Next_Ident,
               Aggregate_Name      => Var_identifier,
               Is_Pointer          => Is_Pointer);

            Is_Pointer := Next_Is_Pointer;

            N := BATN.Next_Node (N);
         end loop;

         if not BANu.Is_Empty (BATN.Array_Index (Node)) then
            N := BATN.First_Node (BATN.Array_Index (Node));

            while Present (N) loop
               Var_identifier := Make_Array_Declaration
                 (Defining_Identifier => Var_identifier,
                  Array_Size => Evaluate_BA_Integer_Value
                    (Node         => N,
                     S            => S,
                     Declarations => Declarations,
                     Statements   => Statements));

               N := BATN.Next_Node (N);
            end loop;
         end if;
      end if;

      return Var_identifier;
   end Map_C_BA_Name;

   ------------------
   -- Map_C_Target --
   ------------------

   function Map_C_Target
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id) return Node_Id
   is
      use AAN;
      pragma Assert (BATN.Kind (Node) = BATN.K_Name or else
                     BATN.Kind (Node) = BATN.K_Data_Component_Reference);

      Var_identifier       : Node_Id;
      N                    : Node_Id;
      Corresponding_Entity : Node_Id;
   begin

      if BATN.Kind (Node) = BATN.K_Name then

         if not BANu.Is_Empty (BATN.Idt (Node)) then
            if BANu.Length (BATN.Idt (Node)) = 1 then
               --  We verify if target is
               --  an outgoing_subprogram_parameter_identifier
               --  in this case the corresponding idendifier must
               --  be a pointer
               --
               Corresponding_Entity := BATN.Corresponding_Entity
                 (BATN.First_Node (BATN.Idt (Node)));

               if present (Corresponding_Entity) then

                  if (AAN.Kind (Corresponding_Entity) = AAN.K_Parameter
                    and then AAN.Is_Out (Corresponding_Entity))
                    or else
                      (AAN.Kind (Corresponding_Entity) = AAN.K_Subcomponent
                       and then
                       Component_Category'Val (Category (Corresponding_Entity))
                       = CC_Data)
                  then
                     Var_identifier :=
                       CTU.Make_Defining_Identifier
                         (Name           => BATN.Display_Name
                            (BATN.First_Node (BATN.Idt (Node))),
                          Pointer        => True);
                  elsif AAN.Kind (Corresponding_Entity) = AAN.K_Port_Spec
                    and then AAN.Is_Out (Corresponding_Entity)
                  then
                     --  Declare request variable if it is not yet declared
                     Make_Request_Variable_Declaration
                       (Declarations,
                        BATN.Display_Name (BATN.First_Node
                          (BATN.Idt (Node))));

                     Make_Output_Port_Name
                       (Node         => Node,
                        S            => S,
                        Statements   => Statements);

                     N := Message_Comment (" The name of the corresponding"
                                           & " port variable is built from"
                                           & " the port name,"
                                           & " following similar pattern. ");
                     Append_Node_To_List (N, Statements);

                     N := Make_Call_Profile
                       (RE (RE_PORT_VARIABLE),
                        Make_List_Id
                          (Make_Defining_Identifier
                               (Map_Thread_Port_Variable_Name (S)),
                           Make_Defining_Identifier
                             (BATN.Display_Name (BATN.First_Node
                              (BATN.Idt (Node))))));

                     Var_identifier := CTU.Make_Member_Designator
                       (Defining_Identifier => N,
                        Aggregate_Name      =>
                          Make_Defining_Identifier
                            (Make_Request_Variable_Name_From_Port_Name
                                 (BATN.Display_Name (BATN.First_Node
                                  (BATN.Idt (Node))))));
                  end if;

               else
                  Var_identifier :=
                    CTU.Make_Defining_Identifier
                      (Name           => BATN.Display_Name
                         (BATN.First_Node (BATN.Idt (Node))),
                       Pointer        => False);
               end if;
            else
               Var_identifier := Map_C_BA_Name
                 (Node             => Node,
                  S                => S,
                  Declarations     => Declarations,
                  Statements       => Statements);
            end if;

         end if;

      elsif BATN.Kind (Node) = BATN.K_Data_Component_Reference
      then
         Var_identifier := Map_C_Data_Component_Reference
           (Node             => Node,
            S                => S,
            Declarations     => Declarations,
            Statements       => Statements);
      end if;
      return Var_identifier;
   end Map_C_Target;

   -----------------------------
   -- Map_C_Assignment_Action --
   -----------------------------

   procedure Map_C_Assignment_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Assignment_Action);
      Expr : Node_Id;
   begin

      Expr := Evaluate_BA_Value_Expression
        (Node             => BATN.Value_Expression (Node),
         Subprogram_Root  => S,
         Declarations     => Declarations,
         Statements       => Statements);

      CTU.Append_Node_To_List
        (CTU.Make_Assignment_Statement
           (Variable_Identifier => Map_C_Target
                (Node         => BATN.Target (Node),
                 S            => S,
                 Declarations => Declarations,
                 Statements   => Statements),
            Expression          => Expr),
         Statements);

      if BATN.Is_Any (Node) then
         Display_Error
           ("The mapping of (any) is not supported", Fatal => True);
      end if;

   end Map_C_Assignment_Action;

   ----------------------------------
   -- Evaluate_BA_Value_Expression --
   ----------------------------------

   function Evaluate_BA_Value_Expression
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Value_Expression);
      pragma Assert (not BANu.Is_Empty (BATN.Relations (Node)));

      N          : Node_Id;
      Left_Expr  : Node_Id;
      Right_Expr : Node_Id := No_Node;
      Op         : Operator_Type := Op_None;
      Expr       : Node_Id;
   begin

      N := BATN.First_Node (BATN.Relations (Node));

      Left_Expr := Evaluate_BA_Relation
        (N, Is_Out_Parameter, Subprogram_Root,
         Declarations, Statements, Is_Put_Value_On_Port);

      N := BATN.Next_Node (N);

      if No (N) then
         return Left_Expr;
      else

         while Present (N) loop

            case BATN.Kind (N) is
               when BATN.K_Relation =>
                  Right_Expr := Evaluate_BA_Relation
                    (Node             => N,
                     Subprogram_Root  => Subprogram_Root,
                     Declarations     => Declarations,
                     Statements       => Statements);

               when BATN.K_Operator =>
                  Op := Evaluate_BA_Operator (N);
               when others     => Display_Error
                    ("Not valid Value_Expression", Fatal => True);
            end case;

            if Right_Expr /= No_Node and then
              Op /= Op_None
            then
               Expr := Make_Expression (Left_Expr, Op, Right_Expr);
               Left_Expr := Expr;
               Op := Op_None;
               Right_Expr := No_Node;
            end if;

            N := BATN.Next_Node (N);
         end loop;

         return Expr;
      end if;

   end Evaluate_BA_Value_Expression;

   --------------------------
   -- Evaluate_BA_Relation --
   --------------------------

   function Evaluate_BA_Relation
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Relation);

      N          : Node_Id;
      Left_Expr  : Node_Id;
      Right_Expr : Node_Id := No_Node;
      Op         : Operator_Type := Op_None;
      Expr       : Node_Id;
   begin

      if not BANu.Is_Empty (BATN.Simple_Exprs (Node)) then

         N := BATN.First_Node (BATN.Simple_Exprs (Node));

         Left_Expr := Evaluate_BA_Simple_Expression
           (N, Is_Out_Parameter, Subprogram_Root,
            Declarations, Statements, Is_Put_Value_On_Port);

         N := BATN.Next_Node (N);

         if No (N) then
            return Left_Expr;
         else

            while Present (N) loop

               case BATN.Kind (N) is
                  when BATN.K_Simple_Expression =>
                     Right_Expr := Evaluate_BA_Simple_Expression
                       (Node             => N,
                        Subprogram_Root  => Subprogram_Root,
                        Declarations     => Declarations,
                        Statements       => Statements);
                  when BATN.K_Operator =>
                     Op := Evaluate_BA_Operator (N);
                  when others     => Display_Error
                       ("Not valid Relation", Fatal => True);
               end case;

               if Right_Expr /= No_Node and then
                 Op /= Op_None
               then
                  Expr := Make_Expression (Left_Expr, Op, Right_Expr);
                  Left_Expr := Expr;
                  Op := Op_None;
                  Right_Expr := No_Node;
               end if;

               N := BATN.Next_Node (N);
            end loop;

            return Expr;
         end if;

      end if;

      raise Program_Error;
      return No_Node;
   end Evaluate_BA_Relation;

   --------------------------
   -- Evaluate_BA_Operator --
   --------------------------

   function Evaluate_BA_Operator
     (Node             : Node_Id)
      return Operator_Type
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Operator);

   begin

      case Operator_Kind'Val (BATN.Operator_Category (Node)) is

         --  logical operator
         when OK_And              => return CTU.Op_And;
         when OK_Or               => return CTU.Op_Or;
         when OK_Xor              => Display_Error
              ("Not supported Operator", Fatal => True);
         when OK_Or_Else          => return CTU.Op_Or;
         when OK_And_Then         => return CTU.Op_And;

         --  relational_operator
         when OK_Equal            => return CTU.Op_Equal_Equal;
         when OK_Non_Equal        => return CTU.Op_Not_Equal;
         when OK_Less_Than        => return CTU.Op_Less;
         when OK_Less_Or_Equal    => return CTU.Op_Less_Equal;
         when OK_Greater_Than     => return CTU.Op_Greater;
         when OK_Greater_Or_Equal => return CTU.Op_Greater_Equal;

         --  unary_adding_opetor
         --  binary_adding_operator
         when OK_Plus             => return CTU.Op_Plus;
         when OK_Minus            => return CTU.Op_Minus;

         --  multiplying operator
         when OK_Multiply         => return CTU.Op_Asterisk;
         when OK_Divide           => return CTU.Op_Slash;
         when OK_Mod              => return CTU.Op_Modulo;
         when OK_Rem              => Display_Error
              ("Not supported Operator", Fatal => True);

         --  highest precedence operator
         when OK_Exponent         => Display_Error
              ("Not supported Operator", Fatal => True);
         when OK_Abs              => Display_Error
              ("Not supported Operator", Fatal => True);
         when OK_Not              => return CTU.Op_Not;

         when others              => Display_Error
              ("Not valid Operator", Fatal => True);
      end case;

      raise Program_Error;
      return CTU.Op_None;

   end Evaluate_BA_Operator;

   -----------------------------------
   -- Evaluate_BA_Simple_Expression --
   -----------------------------------

   function Evaluate_BA_Simple_Expression
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Simple_Expression);
      pragma Assert (not BANu.Is_Empty (BATN.Term_And_Operator (Node)));

      N          : Node_Id;
      Left_Expr  : Node_Id;
      Right_Expr : Node_Id := No_Node;
      Op         : Operator_Type := Op_None;
      Expr       : Node_Id;
   begin

      N := BATN.First_Node (BATN.Term_And_Operator (Node));

      Left_Expr := Evaluate_BA_Term
        (N, Is_Out_Parameter, Subprogram_Root,
         Declarations, Statements, Is_Put_Value_On_Port);

      N := BATN.Next_Node (N);

      if No (N) then
         return Left_Expr;
      else
         while Present (N) loop

            case BATN.Kind (N) is
               when BATN.K_Term =>
                  Right_Expr := Evaluate_BA_Term
                    (Node             => N,
                     Subprogram_Root  => Subprogram_Root,
                     Declarations     => Declarations,
                     Statements       => Statements);
               when BATN.K_Operator =>
                  Op := Evaluate_BA_Operator (N);
               when others     => Display_Error
                    ("Not valid BA_Term", Fatal => True);
            end case;

            if Right_Expr /= No_Node and then
              Op /= Op_None
            then
               Expr := Make_Expression (Left_Expr, Op, Right_Expr);
               Left_Expr := Expr;
               Op := Op_None;
               Right_Expr := No_Node;
            end if;

            N := BATN.Next_Node (N);
         end loop;

         return Expr;
      end if;

   end Evaluate_BA_Simple_Expression;

   ----------------------
   -- Evaluate_BA_Term --
   ----------------------

   function Evaluate_BA_Term
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Term);
      pragma Assert (not BANu.Is_Empty (BATN.Factors (Node)));

      N          : Node_Id;
      Left_Expr  : Node_Id;
      Right_Expr : Node_Id := No_Node;
      Op         : Operator_Type := Op_None;
      Expr       : Node_Id;
   begin

      N := BATN.First_Node (Factors (Node));

      Left_Expr := Evaluate_BA_Factor (N, Is_Out_Parameter,
                                       Subprogram_Root,
                                       Declarations, Statements,
                                       Is_Put_Value_On_Port);

      N := BATN.Next_Node (N);

      if No (N) then
         return Left_Expr;
      else

         while Present (N) loop

            case BATN.Kind (N) is
               when BATN.K_Factor =>
                  Right_Expr := Evaluate_BA_Factor
                    (Node             => N,
                     Subprogram_Root  => Subprogram_Root,
                     Declarations     => Declarations,
                     Statements       => Statements);
               when BATN.K_Operator =>
                  Op := Evaluate_BA_Operator (N);
               when others     => Display_Error
                    ("Not valid BA_Term", Fatal => True);
            end case;

            if Right_Expr /= No_Node and then
              Op /= Op_None
            then
               Expr := Make_Expression (Left_Expr, Op, Right_Expr);
               Left_Expr := Expr;
               Op := Op_None;
               Right_Expr := No_Node;
            end if;

            N := BATN.Next_Node (N);
         end loop;

         return Expr;
      end if;

   end Evaluate_BA_Term;

   ------------------------
   -- Evaluate_BA_Factor --
   ------------------------

   function Evaluate_BA_Factor
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Factor);
   begin
      --  if BATN.Is_Abs (Node) then
      --     --  We must add the library #include <stdlib.h>
      --     --  in order to be able to use their functions
      --     --  abs (x), pow (x,y)
      --     --
      --     Display_Error ("Abs not treated yet", Fatal => True);
      --  elsif BATN.Is_Not (Node) then
      --     Op := CTU.Op_Not;
      --  end if;

      if BATN.Is_Not (Node) then
         return Make_Expression
           (Left_Expr  => Evaluate_BA_Value
              (BATN.Lower_Value (Node), Is_Out_Parameter,
               Subprogram_Root, Declarations, Statements),
            Operator   => CTU.Op_Not);
      else
         return Evaluate_BA_Value
           (BATN.Lower_Value (Node), Is_Out_Parameter,
            Subprogram_Root, Declarations, Statements, Is_Put_Value_On_Port);
      end if;

      --  if Present (BATN.Upper_Value (Node)) then
      --     Display_Error ("Exponent not treated yet", Fatal => True);
      --  end if;

   end Evaluate_BA_Factor;

   -----------------------
   -- Evaluate_BA_Value --
   -----------------------

   function Evaluate_BA_Value
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Value_Variable
                     or else Kind (Node) = BATN.K_Value_Expression
                     or else Kind (Node) = BATN.K_Literal
                     or else Kind (Node) = BATN.K_Boolean_Literal
                     or else Kind (Node) = BATN.K_Property_Constant
                     or else Kind (Node) = BATN.K_Property_Reference
                     or else Kind (Node) = BATN.K_Identifier);
      result : Node_Id;
   begin

      case BATN.Kind (Node) is

         when BATN.K_Value_Variable     =>
            result := Evaluate_BA_Value_Variable
              (Node         => Node,
               S            => Subprogram_Root,
               Declarations => Declarations,
               Statements   => Statements);

         when BATN.K_Literal              =>
            result := Evaluate_BA_Literal (Node);

         when BATN.K_Boolean_Literal    =>
            result := Evaluate_BA_Boolean_Literal (Node);

         when BATN.K_Property_Constant  =>
            result := Evaluate_BA_Property_Constant
              (Node, Is_Out_Parameter, Subprogram_Root,
               Declarations, Statements, Is_Put_Value_On_Port);

            --  when BATN.K_Property_Reference =>
            --    Evaluate_BA_Property_Reference (Node);

         when BATN.K_Value_Expression   =>
            result := Evaluate_BA_Value_Expression
              (Node             => Node,
               Subprogram_Root  => Subprogram_Root,
               Declarations     => Declarations,
               Statements       => Statements);

         when BATN.K_Identifier           =>
            result := Evaluate_BA_Identifier
              (Node             => Node,
               Subprogram_Root  => Subprogram_Root,
               Declarations     => Declarations,
               Statements       => Statements);

         when others                      =>
            Display_Error ("Mapping of other kinds of BA Value"
                           & " are not yet supported.",
                           Fatal => True);
      end case;

      return result;

   end Evaluate_BA_Value;

   -------------------------------
   -- Evaluate_BA_Integer_Value --
   -------------------------------

   function Evaluate_BA_Integer_Value
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Integer_Value);
      pragma Assert (BATN.Kind (BATN.Entity (Node)) = K_Value_Variable
                       or else BATN.Kind (BATN.Entity (Node)) = K_Literal
                     or else BATN.Kind (BATN.Entity (Node))
                     = K_Property_Constant);

      Entity_Node : constant Node_Id := BATN.Entity (Node);
      result      : Node_Id;
   begin
      case BATN.Kind (Entity_Node) is
         when K_Value_Variable    =>
            result := Evaluate_BA_Value_Variable
              (Node             => Entity_Node,
               S                => S,
               Declarations     => Declarations,
               Statements       => Statements);

         when K_Literal           =>
            result := Evaluate_BA_Literal (Entity_Node);

         when K_Property_Constant =>
            result := Evaluate_BA_Property_Constant
              (Node             => Entity_Node,
               Subprogram_Root  => S,
               Declarations     => Declarations,
               Statements       => Statements);

         when others              =>
            Display_Error (" Incorrect Integer Value ", Fatal => True);
      end case;

      return result;

   end Evaluate_BA_Integer_Value;

   -------------------------
   -- Evaluate_BA_Literal --
   -------------------------

   function Evaluate_BA_Literal (Node : Node_Id) return Node_Id is
      pragma Assert (BATN.Kind (Node) = BATN.K_Literal);
   begin

      return CTU.Make_Literal (CV.To_C_Value (BATN.Value (Node)));

   end Evaluate_BA_Literal;

   ------------------------------------------------------
   -- Make_Call_Parameter_For_Get_Count_and_Next_Value --
   ------------------------------------------------------

   function Make_Call_Parameter_For_Get_Count_and_Next_Value
     (Node : Node_Id;
      S    : Node_Id) return List_Id
   is
      N               : Node_Id;
      N1              : Name_Id;
      Call_Parameters : List_Id;
   begin

      Call_Parameters := New_List (CTN.K_Parameter_List);

      Set_Str_To_Name_Buffer ("self");
      N1 := Name_Find;
      N := Make_Defining_Identifier (N1);
      Append_Node_To_List (N, Call_Parameters);

      Append_Node_To_List
        (Make_Call_Profile
           (RE (RE_Local_Port),
            Make_List_Id
              (Make_Defining_Identifier
                   (Map_Thread_Port_Variable_Name (S)),
               Make_Defining_Identifier
                 (BATN.Display_Name (Node)))),
         Call_Parameters);
      return Call_Parameters;

   end Make_Call_Parameter_For_Get_Count_and_Next_Value;

   ----------------------------
   -- Make_Get_Count_of_Port --
   ----------------------------

   function Make_Get_Count_of_Port
     (Node             : Node_Id;
      S                : Node_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      Call_Parameters : List_Id;
      result          : Node_Id := No_Node;
   begin

      --  Make the call to __po_hi_gqueue_get_count
      --  if it is an event port
      if AAN.Is_Event (BATN.Corresponding_Entity (Node)) then
         Call_Parameters := Make_Call_Parameter_For_Get_Count_and_Next_Value
           (Node, S);

         result := CTU.Make_Call_Profile
           (RE (RE_Gqueue_Get_Count),
            Call_Parameters);
      end if;
      return result;

   end Make_Get_Count_of_Port;

   -----------------------------
   -- Make_Next_Value_of_Port --
   -----------------------------

   procedure Make_Next_Value_of_Port
     (Node             : Node_Id;
      S                : Node_Id;
      Statements       : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      Call_Parameters : List_Id;
   begin

      --  Make the call to __po_hi_gqueue_next_value
      --  if it is an event port
      if AAN.Is_Event (BATN.Corresponding_Entity (Node)) then
         Call_Parameters := Make_Call_Parameter_For_Get_Count_and_Next_Value
           (Node, S);

         Append_Node_To_List
           (CTU.Make_Call_Profile
              (RE (RE_Gqueue_Next_Value),
               Call_Parameters),
            Statements);
      end if;

   end Make_Next_Value_of_Port;

   -----------------------------------------------
   -- Make_Request_Variable_Name_From_Port_Name --
   -----------------------------------------------

   function Make_Request_Variable_Name_From_Port_Name
     (Port_Name : Name_Id) return Name_Id
   is
      Result : Name_Id;
   begin

      Set_Str_To_Name_Buffer ("__");
      Get_Name_String_And_Append (Port_Name);
      Add_Str_To_Name_Buffer ("_request");

      Result := Name_Find;
      return Standard.Utils.To_Lower (Result);

   end Make_Request_Variable_Name_From_Port_Name;

   ---------------------------------------
   -- Make_Request_Variable_Declaration --
   ---------------------------------------

   procedure Make_Request_Variable_Declaration
     (Declarations : List_Id;
      Port_Name    : Name_Id)
   is
      decl                      : Node_Id;
      request_declaration_exist : Boolean := False;
      Request_Name              : Name_Id;
   begin

      Request_Name := Make_Request_Variable_Name_From_Port_Name (Port_Name);

      decl := CTN.First_Node (Declarations);
      while Present (decl) loop

         if Kind (decl) = CTN.K_Variable_Declaration then

            if Get_Name_String
              (Standard.Utils.To_Lower
                 (CTN.Name (CTN.Defining_Identifier (decl))))
                = Get_Name_String (Request_Name)
            then
               request_declaration_exist := True;
            end if;
         end if;

         exit when request_declaration_exist;
         decl := CTN.Next_Node (decl);
      end loop;

      if not request_declaration_exist then
         CTU.Append_Node_To_List
           (Make_Variable_Declaration
              (Defining_Identifier =>
                   Make_Defining_Identifier (Request_Name),
               Used_Type           => RE (RE_Request_T)),
            Declarations);
      end if;

   end Make_Request_Variable_Declaration;

   ----------------------------
   -- Make_Get_Value_of_Port --
   ----------------------------

   function Make_Get_Value_of_Port
     (Node             : Node_Id;
      Subprogram_Root  : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      N               : Node_Id;
      N1              : Name_Id;
      result          : Node_Id;
      Call_Parameters : List_Id;
   begin
      --  Read from the in data port
      N := Message_Comment ("Read the data from the port "
                            & Get_Name_String
                              (BATN.Display_Name (Node)));

      Append_Node_To_List (N, Statements);

      Make_Request_Variable_Declaration (Declarations,
                                         BATN.Display_Name (Node));

      --  Make the call to __po_hi_gqueue_get_value

      Call_Parameters := New_List (CTN.K_Parameter_List);

      Set_Str_To_Name_Buffer ("self");
      N1 := Name_Find;
      N := Make_Defining_Identifier (N1);
      Append_Node_To_List (N, Call_Parameters);

      Append_Node_To_List
        (Make_Call_Profile
           (RE (RE_Local_Port),
            Make_List_Id
              (Make_Defining_Identifier
                   (Map_Thread_Port_Variable_Name (Subprogram_Root)),
               Make_Defining_Identifier (BATN.Display_Name (Node)))),
         Call_Parameters);

      N :=
        Make_Variable_Address
          (Make_Defining_Identifier
             (Make_Request_Variable_Name_From_Port_Name
                (BATN.Display_Name (Node))));
      Append_Node_To_List (N, Call_Parameters);

      N :=
        CTU.Make_Call_Profile
          (RE (RE_Gqueue_Get_Value),
           Call_Parameters);
      Append_Node_To_List (N, Statements);

      Call_Parameters := New_List (CTN.K_Parameter_List);

      Append_Node_To_List
        (Make_Defining_Identifier
           (Map_Thread_Port_Variable_Name (Subprogram_Root)),
         Call_Parameters);

      Append_Node_To_List
        (Make_Defining_Identifier (BATN.Display_Name (Node)),
         Call_Parameters);

      result := CTU.Make_Call_Profile
        (Make_Member_Designator
           (Defining_Identifier => RE (RE_PORT_VARIABLE),
            Aggregate_Name      =>
              Make_Defining_Identifier
                (Make_Request_Variable_Name_From_Port_Name
                     (BATN.Display_Name (Node)))),
         Call_Parameters);

      return result;

   end Make_Get_Value_of_Port;

   ----------------------------
   -- Evaluate_BA_Identifier --
   ----------------------------

   function Evaluate_BA_Identifier
     (Node                 : Node_Id;
      Is_Out_Parameter     : Boolean := False;
      Subprogram_Root      : Node_Id := No_Node;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id
   is
      use AAN;
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);
      Pointer          : Boolean := False;
      Variable_Address : Boolean := False;
      N, N1            : Node_Id;
      result           : Node_Id;
   begin

      N := BATN.Corresponding_Entity (Node);
      if Is_Out_Parameter then
         if Find_BA_Variable (Node,
                              Get_Behavior_Specification (Subprogram_Root))
           /= No_Node
         then
            --  The given parameter is a BA variable
            --  then it must be mapped to a variable
            --  address to enable its modification
            --  by the called subprogram
            --
            Variable_Address := True;
         else
            --  The given parameter is an OUT/INOUT parameter
            --  of the subprogram implementing the BA, it is
            --  already a pointer
            --
            Pointer := False;
         end if;
      else
         --  In the case of IN parameter_label, if it contain an
         --  OUT parameter (ex. p1) of the subprogram implementing
         --  the BA, then in the generated code C, in order
         --  to have the value of p1, it must be mapped to *p1
         --
         if Subprogram_Root /= No_Node and then
           AINU.Is_Subprogram (Subprogram_Root)
         then
            declare
               use type AIN.Node_Kind;
               Fs : constant Ocarina.ME_AADL.AADL_Instances.Nutils.Node_Array
                 := Features_Of (Subprogram_Root);
            begin
               for F of Fs loop
                  if Standard.Utils.To_Upper
                    (AIN.Display_Name (AIN.Identifier (F)))
                    = Standard.Utils.To_Upper
                    (BATN.Display_Name (Node))
                    --  and then AIN.Is_Out (F)
                    and then
                      ((AIN.Kind (F) = AIN.K_Parameter_Instance
                        and then AIN.Is_Out (F))
                       or else
                         (AIN.Kind (F) = AIN.K_Subcomponent_Access_Instance
                          and then
                          Get_Required_Data_Access
                            (AIN.Corresponding_Instance (F))
                          /= Access_Read_Only))
                  then
                     Pointer := True;
                  end if;
               end loop;
            end;
         end if;

         if Present (N) then
            if AAN.kind (N) = AAN.K_Subcomponent then
               N1 := AIN.Corresponding_Instance
                 (Get_Subcomponent_Data_Instance
                    (N, Subprogram_Root));
               if AINU.Is_Data (N1) then
                  if Get_Data_Representation (N1) = Data_Struct then
                     if not Is_Put_Value_On_Port then
                        return CTU.Make_Defining_Identifier
                          (Name  => BATN.Display_Name (Node));
                     else
                        Pointer := True;
                     end if;
                  else
                     Pointer := True;
                  end if;
               end if;
            end if;
         end if;
      end if;

      result := CTU.Make_Defining_Identifier
        (Name             => BATN.Display_Name (Node),
         Pointer          => Pointer,
         Variable_Address => Variable_Address);

      if Present (N) then
         if AAN.kind (N) = AAN.K_Port_Spec
           and then AAN.Is_Data (N)
         then
            if AAN.Is_In (N) then
               result := Make_Get_Value_of_Port
                 (Node, Subprogram_Root, Declarations, Statements);
            else
               --  i.e the identifier is an out port

               Make_Request_Variable_Declaration
                 (Declarations,
                  BATN.Display_Name (Node));

               Make_Output_Port_Name
                 (Node         => Node,
                  S            => Subprogram_Root,
                  Statements   => Statements);

               N := Make_Call_Profile
                 (RE (RE_PORT_VARIABLE),
                  Make_List_Id
                    (Make_Defining_Identifier
                         (Map_Thread_Port_Variable_Name (Subprogram_Root)),
                     Make_Defining_Identifier
                       (BATN.Display_Name (Node))));

               result := CTU.Make_Member_Designator
                 (Defining_Identifier => N,
                  Aggregate_Name      =>
                    Make_Defining_Identifier
                      (Make_Request_Variable_Name_From_Port_Name
                           (BATN.Display_Name (Node))));

            end if;
         end if;
      end if;

      return result;

   end Evaluate_BA_Identifier;

   --------------------------------------------
   -- Make_Intermediate_Variable_Declaration --
   --------------------------------------------

   procedure Make_Intermediate_Variable_Declaration
     (N            : Name_Id;
      Used_Type    : Node_Id;
      Declarations : List_Id)
   is
      decl              : Node_Id;
      declaration_exist : Boolean := False;
   begin

      decl := CTN.First_Node (Declarations);
      while Present (decl) loop

         if Kind (decl) = CTN.K_Variable_Declaration then

            if Get_Name_String
              (Standard.Utils.To_Lower
                 (CTN.Name (CTN.Defining_Identifier (decl))))
                = Get_Name_String
              (Standard.Utils.To_Lower (N))

            then
               declaration_exist := True;
            end if;
         end if;

         exit when declaration_exist;
         decl := CTN.Next_Node (decl);
      end loop;

      if not declaration_exist then
         CTU.Append_Node_To_List
           (CTU.Make_Variable_Declaration
              (Defining_Identifier => Make_Defining_Identifier (N),
               Used_Type => Used_Type),
            Declarations);
      end if;

   end Make_Intermediate_Variable_Declaration;

   ------------------------------------
   -- Get_Subcomponent_Data_Instance --
   ------------------------------------

   function Get_Subcomponent_Data_Instance
     (Node             : Node_Id;
      Parent_Component : Node_Id) return Node_Id
   is
      Fs : constant Ocarina.ME_AADL.AADL_Instances.Nutils.Node_Array
        := Subcomponents_Of (Parent_Component);
      result : Node_Id;
   begin
      for F of Fs loop
         if Present (AIN.Identifier (F))
           and then Standard.Utils.To_Upper
             (AIN.Display_Name (AIN.Identifier (F)))
           = Standard.Utils.To_Upper
           (AAN.Display_Name (AAN.Identifier (Node)))
         then
            result := F;
         end if;
      end loop;
      return result;
   end Get_Subcomponent_Data_Instance;

   ----------------------------
   -- Get_Port_Spec_Instance --
   ----------------------------

   function Get_Port_Spec_Instance
     (Node             : Node_Id;
      Parent_Component : Node_Id) return Node_Id
   is
      use type AIN.Node_Kind;
      Fs : constant Ocarina.ME_AADL.AADL_Instances.Nutils.Node_Array
        := Features_Of (Parent_Component);
      result : Node_Id;
   begin
      for F of Fs loop
         if Standard.Utils.To_Upper
           (AIN.Display_Name (AIN.Identifier (F)))
           = Standard.Utils.To_Upper
           (BATN.Display_Name (Node))
           and then
             AIN.Kind (F) = AIN.K_Port_Spec_Instance
           --  and then AIN.Is_In (F)
           --  and then AIN.Is_Data (F)
         then
            result := F;
         end if;
      end loop;
      return result;
   end Get_Port_Spec_Instance;

   --------------------------------
   -- Evaluate_BA_Value_Variable --
   --------------------------------

   function Evaluate_BA_Value_Variable
     (Node             : Node_Id;
      S                : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Value_Variable);

      Ident          : constant Node_Id := BATN.Identifier (Node);
      result         : Node_Id;
      Var_identifier : Node_Id;
      N              : Name_Id;
      Used_Type      : Node_Id;
   begin

      if Is_Interrogative (Node) then
         --  the mapping when Is_Interrogative, i.e. :
         --  incoming_port_name?

         Set_Str_To_Name_Buffer ("__");
         Get_Name_String_And_Append
           (BATN.Display_Name
              (BATN.First_Node
                   (BATN.Idt (BATN.Identifier (Node)))));
         Add_Str_To_Name_Buffer ("_gqueue_get_value");
         N := Name_Find;
         Var_identifier := CTU.Make_Defining_Identifier (N);

         Used_Type := Map_C_Data_Type_Designator
           (AIN.Corresponding_Instance
              (Get_Port_Spec_Instance
                   (Node             => BATN.First_Node
                        (BATN.Idt (BATN.Identifier (Node))),
                    Parent_Component => S)));

         Make_Intermediate_Variable_Declaration
           (N            => N,
            Used_Type    => Used_Type,
            Declarations => Declarations);

         CTU.Append_Node_To_List
           (CTU.Make_Assignment_Statement
              (Variable_Identifier => Var_identifier,
               Expression          => Make_Get_Value_of_Port
                 (BATN.First_Node (BATN.Idt (BATN.Identifier (Node))),
                  S, Declarations, Statements)),
            Statements);

         result := Var_identifier;

         Make_Next_Value_of_Port
           (BATN.First_Node (BATN.Idt (BATN.Identifier (Node))),
            S, Statements);

      elsif Is_Count (Node) then
         --  Here we must add a variable and we assign it with
         --  the returned value of the function __po_hi_gqueue_get_count
         --  int __po_hi_gqueue_get_count
         --     ( __po_hi_task_id id, __po_hi_local_port_t port)

         Set_Str_To_Name_Buffer ("__");
         Get_Name_String_And_Append
           (BATN.Display_Name
              (BATN.First_Node
                   (BATN.Idt (BATN.Identifier (Node)))));
         Add_Str_To_Name_Buffer ("_gqueue_get_count");
         N := Name_Find;
         Var_identifier := CTU.Make_Defining_Identifier (N);

         Make_Intermediate_Variable_Declaration
           (N            => N,
            Used_Type    => RE (RE_Int16_T),
            Declarations => Declarations);

         CTU.Append_Node_To_List
           (CTU.Make_Assignment_Statement
              (Variable_Identifier => Var_identifier,
               Expression          => Make_Get_Count_of_Port
                 (BATN.First_Node
                      (BATN.Idt (BATN.Identifier (Node))), S)),
            Statements);

         result := Var_identifier;

      --  TODO: the mapping for the cases Fresh and Updated
      --  elsif Is_Fresh (Node) then
      --
      --  elsif Is_Updated (Node) then
      --
      elsif BATN.Kind (Ident) = BATN.K_Name then
         result := Map_C_BA_Name
           (Node         => Ident,
            S            => S,
            Declarations => Declarations,
            Statements   => Statements);
      elsif BATN.Kind (Ident) = BATN.K_Data_Component_Reference then
         result := Map_C_Data_Component_Reference
           (Node         => Ident,
            S            => S,
            Declarations => Declarations,
            Statements   => Statements);
      end if;

      return result;

   end Evaluate_BA_Value_Variable;

   -----------------------------------
   -- Evaluate_BA_Property_Constant --
   -----------------------------------

   function Evaluate_BA_Property_Constant
     (Node                 : Node_Id;
      Is_Out_parameter     : Boolean := False;
      Subprogram_Root      : Node_Id;
      Declarations         : List_Id;
      Statements           : List_Id;
      Is_Put_Value_On_Port : Boolean := False) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Property_Constant);

   begin
      --  We must treat later the case of Property_Set (Node)
      --
      --        if Present (BATN.Property_Set (Node)) then
      --
      --        end if;

      return Evaluate_BA_Identifier
        (BATN.Identifier (Node), Is_Out_Parameter,
         Subprogram_Root, Declarations, Statements, Is_Put_Value_On_Port);

   end Evaluate_BA_Property_Constant;

   ---------------------------
   -- Print_Boolean_Literal --
   ---------------------------

   function Evaluate_BA_Boolean_Literal (Node : Node_Id) return Node_Id  is
      pragma Assert (BATN.Kind (Node) = BATN.K_Boolean_Literal);
      N : Name_Id;
   begin
      if BATN.Is_True (Node) then
         Set_Str_To_Name_Buffer ("True");
         N := Name_Find;
         return Make_Defining_Identifier (N);
         --  Expr := PHR.RE (PHR.RE_True);
         --  Expr := Make_Literal (CV.New_Int_Value (1, 1, 10));
      else
         Set_Str_To_Name_Buffer ("False");
         N := Name_Find;
         return  Make_Defining_Identifier (N);
         --  Expr := PHR.RE (PHR.RE_False);
         --  Expr := Make_Literal (CV.New_Int_Value (0, 1, 10));
      end if;

   end Evaluate_BA_Boolean_Literal;

end Ocarina.Backends.C_Common.BA;
