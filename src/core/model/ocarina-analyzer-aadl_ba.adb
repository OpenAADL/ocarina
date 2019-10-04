------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . A N A L Y Z E R . A A D L _ B A              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2016-2019 ESA & ISAE.                    --
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
with Errors;
with Utils;
with Locations;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL_BA;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL_BA.Tokens;

package body Ocarina.Analyzer.AADL_BA is

   use Utils;
   use Ocarina.Namet;
   use Errors;
   use Locations;

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL_BA;
   use Ocarina.Analyzer.AADL.Finder;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ANU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package BATN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   package BAT renames Ocarina.ME_AADL_BA.Tokens;

   type Behavior_State_Kind_Array is array (Positive range <>)
     of Ocarina.ME_AADL_BA.Behavior_State_Kind;

   function Analyze_Behavior_Specification
     (Root             : Node_Id;
      BA_Root          : Node_Id;
      Parent_Component : Node_Id)
      return Boolean;

   function Analyze_Behavior_Variables
     (Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Behavior_States
     (Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Behavior_Transitions
     (Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Behavior_Transition
     (Transition        : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Behavior_Action_Block
     (Action_Block      : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Behav_Acts
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Behavior_Action
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean;

   function Analyze_If_Cond_Struct
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean;

   function Analyze_For_or_ForAll_Cond_Struct
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean;

   function Analyze_While_Cond_Struct
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean;

   function Analyze_DoUntil_Cond_Struct
     (Node              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Assignment_Action
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean;

   function Analyze_Data_Component_Reference
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_BA_Name
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_BA_Value_Variable
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_BA_Integer_Value
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Link_Target
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Communication_Action
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Spg_Call_Or_Port_Sending
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Is_A_Required_Spg_Access_Of_the_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Link_Spg
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Check_Spg_Params
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Link_Output_Or_Internal_Port
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Check_Send_Output_Param
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Link_Input_Port
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_BA_Value_Expression
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id := No_List)
      return Boolean;

   function Analyze_BA_Relation
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id)
      return Boolean;

   function Analyze_BA_Simple_Expression
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id)
      return Boolean;

   function Analyze_Term
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id)
      return Boolean;

   function Analyze_Factor
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id)
      return Boolean;

   function Analyze_Value
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id := No_List)
      return Boolean;

   function Find_Parameter_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id;

   function Find_Out_Parameter_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id;

   function Find_Requires_Data_Access_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id;

   function Find_Data_Subcomponent_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id;

   function Find_Data_Port_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id;

   function Find_and_Link_Event_Port_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id;

   function Find_Output_Port_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id;

   function Find_Input_Port_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id;

   function Get_Component_Type_From_Impl
     (Root            : Node_Id;
      Component_Impl  : Node_Id)
      return Node_Id;

   function Analyze_Timed_Action
     (Node              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Analyze_Dispatch_Condition
     (Condition         : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean;

   function Enter_Name_In_Table (Identifier : Node_Id)
                                 return boolean;

   procedure Remove_All_Identifiers_From_Table
     (BA_Root          : Node_Id);

   function Test_With_Identifiers_In_Parent_Component
     (Id         : Node_Id;
      Root       : Node_Id;
      Component  : Node_Id)
      return Boolean;

   procedure Select_All_States
     (BA_Root         : Node_Id;
      List_First_Node : in out Node_Id;
      List_Last_Node  : in out Node_Id);

   procedure Select_States
     (BA_Root         :    Node_Id;
      Kinds           : Behavior_State_Kind_Array;
      List_First_Node : in out Node_Id;
      List_Last_Node  : in out Node_Id);

   function Exist_In_Modes
     (Component : Node_Id;
      State     : Node_Id)
      return boolean;

   function Link_Variable
     (Root             : Node_Id;
      Node             : Node_Id)
      return Boolean;

   function Find_State
     (State_Id : Node_Id;
      BA_Root  : Node_Id)
      return Node_Id;

   function Link_Frozen_Port
     (Frozen_Port : Node_Id;
      Root        : Node_Id;
      Component   : Node_Id)
      return Node_Id;

   function Link_Dispatch_Trigger_Event
     (Dispatch_Trigger_Event : Node_Id;
      Root                   : Node_Id;
      Component              : Node_Id)
      return Node_Id;

   function Build_Full_Package_Name
     (Package_Name  : List_Id) return Name_Id;

   function Length (L : Node_List) return Natural;

   Language : constant String := "behavior_specification";

   ----------
   -- Init --
   ----------

   procedure Init is
   begin

      Ocarina.Analyzer.Register_Analyzer (Language, Analyze_Model'Access);
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
   end Reset;

   -------------------
   -- Analyze_Model --
   -------------------

   function Analyze_Model (Root : Node_Id) return Boolean is
      use ATN;

      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);

      Success : Boolean := True;
      L2 : List_Id;
      N1 : Node_Id;
      N2 : Node_Id;
      List_Node : Node_Id;
      BA_Root : Node_Id;
      Parent_Component : Node_Id;

   begin

      if not ANU.Is_Empty (ATN.Declarations (Root))
      then
         List_Node := ATN.First_Node
           (ATN.Declarations (Root));

         while Present (List_Node) loop
            if ATN.Kind (List_Node) = ATN.K_Package_Specification then

               N1 := ATN.First_Node (Declarations (List_Node));
               while Present (N1) loop
                  if ((ATN.Kind (N1) = ATN.K_Component_Type)
                      or else
                        (ATN.Kind (N1) = ATN.K_Component_Implementation))
                  then

                     L2 := ATN.Annexes (N1);
                     if not ANU.Is_Empty (L2) then

                        N2 := ATN.First_Node (L2);
                        while Present (N2) loop
                           if Get_Name_String
                             (Utils.To_Lower
                                (ATN.Name (ATN.Identifier (N2)))) =
                               BAT.Language and then
                               Present (ATN.Corresponding_Annex (N2))
                           then
                              BA_Root := ATN.Corresponding_Annex (N2);
                              Parent_Component := Container_Component (N2);

                              Success := Success and then
                                Analyze_Behavior_Specification
                                  (Root, BA_Root, Parent_Component);
                           end if;
                           N2 := ATN.Next_Node (N2);
                        end loop;
                     end if;
                  end if;
                  N1 := ATN.Next_Node (N1);
               end loop;
            end if;

            List_Node := ATN.Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Analyze_Model;

   ------------------------------------
   -- Analyze_Behavior_Specification --
   ------------------------------------

   function Analyze_Behavior_Specification
     (Root             : Node_Id;
      BA_Root          : Node_Id;
      Parent_Component : Node_Id)
      return Boolean
   is
      use ATN;

      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      Success   : Boolean := True;
      List_Node : Node_Id;
      Node : Node_Id;
      Behavior_Variable : Node_Id;
      Behavior_State : Node_Id;
      Behavior_Transition : Node_Id;
   begin
      if not Is_Empty (Variables (BA_Root)) then
         Behavior_Variable := BATN.First_Node (Variables (BA_Root));
         while Present (Behavior_Variable) loop
            List_Node := BATN.First_Node
              (BATN.Identifiers (Behavior_Variable));

            while Present (List_Node) loop
               Success := Success and then Enter_Name_In_Table (List_Node);
               List_Node := BATN.Next_Node (List_Node);
            end loop;
            Behavior_Variable := BATN.Next_Node (Behavior_Variable);
         end loop;

         Success := Success and then
         --  analyser behavior_variables
           Analyze_Behavior_Variables (Root, BA_Root, Parent_Component);
      end if;

      if not Is_Empty (States (BA_Root)) then
         Behavior_State := BATN.First_Node (States (BA_Root));
         while Present (Behavior_State) loop
            List_Node := BATN.First_Node
              (BATN.Identifiers (Behavior_State));

            while Present (List_Node) loop
               Success := Success and then Enter_Name_In_Table (List_Node);
               List_Node := BATN.Next_Node (List_Node);
            end loop;
            Behavior_State := BATN.Next_Node (Behavior_State);
         end loop;

         Success := Success and then
         --  analyser behavior_states
           Analyze_Behavior_States (Root, BA_Root, Parent_Component);
      end if;

      if not Is_Empty (Transitions (BA_Root)) then
         Behavior_Transition := BATN.First_Node (Transitions (BA_Root));
         while Present (Behavior_Transition) loop
            if Present (Behavior_Transition_Idt
                        (Transition (Behavior_Transition)))
            then
               Node := Behavior_Transition_Idt
                 (Transition (Behavior_Transition));
               Success := Success and then Enter_Name_In_Table (Node);
            end if;
            Behavior_Transition := BATN.Next_Node (Behavior_Transition);
         end loop;

         Success := Success and then
         --  analyser behavior_transitions
           Analyze_Behavior_Transitions (Root, BA_Root, Parent_Component);
      end if;

      if Success then
         Remove_All_Identifiers_From_Table (BA_Root);
      end if;
      return Success;
   end Analyze_Behavior_Specification;

   --------------------------------
   -- Analyze_Behavior_Variables --
   --------------------------------

   function Analyze_Behavior_Variables
     (Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;

      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);

      List_Node         : Node_Id;
      Behavior_Variable : Node_Id;
      Success           : Boolean := True;
   begin

      Behavior_Variable := BATN.First_Node (Variables (BA_Root));
      while Present (Behavior_Variable) loop
         if not Is_Empty (BATN.Identifiers (Behavior_Variable)) then
            List_Node := BATN.First_Node
              (BATN.Identifiers (Behavior_Variable));
            while Present (List_Node) loop
               Success := Success and then
                 Test_With_Identifiers_In_Parent_Component
                   (List_Node, Root, Parent_Component);
               List_Node := BATN.Next_Node (List_Node);
            end loop;
         end if;

         if Present (BATN.Classifier_Ref (Behavior_Variable)) then
            Success := Link_Variable (Root, Behavior_Variable);
            if not Success then
               Error_Loc (1) := BATN.Loc
                 (BATN.First_Node (BATN.Identifiers (Behavior_Variable)));
               DE ("behavior variable(s) must be explicitly typed "
                   & "with a valid data component classifier.");
            end if;
         end if;
         Behavior_Variable := BATN.Next_Node (Behavior_Variable);
      end loop;
      return Success;
   end Analyze_Behavior_Variables;

   -----------------------------
   -- Analyze_Behavior_States --
   -----------------------------

   function Analyze_Behavior_States
     (Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);

      Success   : Boolean := True;
      Behavior_State : Node_Id;
      List_Node : Node_Id;

      Initial_States_List  : Node_List;
      Complete_States_List : Node_List;
      Final_States_List    : Node_List;
      Execute_States_List  : Node_List;

   begin

      Behavior_State := BATN.First_Node (States (BA_Root));
      while Present (Behavior_State) loop
         if not Is_Empty (BATN.Identifiers (Behavior_State)) then
            List_Node := BATN.First_Node
              (BATN.Identifiers (Behavior_State));
            while Present (List_Node) loop
               if (Is_Complete (BA_Root, List_Node) and then
                     not Exist_In_Modes (Parent_Component, List_Node)) or else
                 not Is_Complete (BA_Root, List_Node)
               then
                  Success := Success and then
                    Test_With_Identifiers_In_Parent_Component
                      (List_Node, Root, Parent_Component);
               end if;
               List_Node := BATN.Next_Node (List_Node);
            end loop;
         end if;
         Behavior_State := BATN.Next_Node (Behavior_State);
      end loop;

      Select_States (BA_Root, (BSK_Initial,
                     BSK_Initial_Complete,
                     BSK_Initial_Complete_Final,
                     BSK_Initial_Final),
                     Initial_States_List.First, Initial_States_List.Last);

      Select_States (BA_Root, (BSK_Complete,
                     BSK_Initial_Complete,
                     BSK_Initial_Complete_Final,
                     BSK_Complete_Final),
                     Complete_States_List.First, Complete_States_List.Last);

      Select_States (BA_Root, (BSK_Final,
                     BSK_Initial_Final,
                     BSK_Initial_Complete_Final,
                     BSK_Complete_Final),
                     Final_States_List.First, Final_States_List.Last);

      Select_States (BA_Root, (1 => BSK_No_Kind),
                     Execute_States_List.First, Execute_States_List.Last);

      if Component_Category'Val (Category (Parent_Component)) = CC_Subprogram
      then
         --  For a subprogram, there can be only one initial and one
         --  final state, and no complete state.

         if Length (Initial_States_List) > 1 then
            Success := False;
            Error_Loc (1) := BATN.Loc (BATN.Next_Node
                                       (Initial_States_List.First));
            DE (Get_Name_String (ATN.Name
                (ATN.Identifier (Parent_Component)))
                & " ( Subprogram )"
                & " can't have more than one initial state.");
         else
            if Length (Initial_States_List) = 0 then
               Success := False;
               Error_Loc (1) := BATN.Loc (Node_Id (States (BA_Root)));
               DE (Get_Name_String (ATN.Name
                   (ATN.Identifier (Parent_Component)))
                   & " ( Subprogram )"
                   & " has no initial state.");
            end if;
         end if;

         if Length (Complete_States_List) > 0 then
            Success := False;
            Error_Loc (1) := BATN.Loc (Complete_States_List.First);
            DE (Get_Name_String (ATN.Name
                (ATN.Identifier (Parent_Component)))
                & " ( Subprogram )"
                & " can't have complete state.");
         end if;

         if Length (Final_States_List) > 1 then
            Success := False;
            Error_Loc (1) := BATN.Loc (BATN.Next_Node
                                       (Final_States_List.First));
            DE (Get_Name_String (ATN.Name
                (ATN.Identifier (Parent_Component)))
                & " ( Subprogram )"
                & " can't have more than one final state.");
         else
            if Length (Final_States_List) = 0 then
               Success := False;
               Error_Loc (1) := BATN.Loc (Node_Id (States (BA_Root)));
               DE (Get_Name_String (ATN.Name
                   (ATN.Identifier (Parent_Component)))
                   & " ( Subprogram )"
                   & " has no final state.");
            end if;
         end if;

      elsif Component_Category'Val (Category (Parent_Component)) = CC_Thread
      then
         --  For a thread, there can be only one initial state, and
         --  several complete and/ou final states.

         if Length (Initial_States_List) > 1 then
            Success := False;
            Error_Loc (1) := BATN.Loc (BATN.Next_Node
                                       (Initial_States_List.First));
            DE (Get_Name_String (ATN.Name
                (ATN.Identifier (Parent_Component)))
                & " ( Thread )"
                & " can't have more than one initial state.");
         else
            if Length (Initial_States_List) = 0 then
               Success := False;
               Error_Loc (1) := BATN.Loc (Node_Id (States (BA_Root)));
               DE (Get_Name_String (ATN.Name
                   (ATN.Identifier (Parent_Component)))
                   & " ( Thread )"
                   & " has no initial state.");
            end if;
         end if;

         if Length (Complete_States_List) = 0 then
            Success := False;
            Error_Loc (1) := BATN.Loc (Node_Id (States (BA_Root)));
            DE (Get_Name_String (ATN.Name
                (ATN.Identifier (Parent_Component)))
                & " ( Thread )"
                & " must define one or more complete states.");
         end if;

         if Length (Final_States_List) = 0 then
            Success := False;
            Error_Loc (1) := BATN.Loc (Node_Id (States (BA_Root)));
            DE (Get_Name_String (ATN.Name
                (ATN.Identifier (Parent_Component)))
                & " ( Thread )"
                & " must define one or more final states.");
         end if;
      end if;
      return Success;
   end Analyze_Behavior_States;

   ----------------------------------
   -- Analyze_Behavior_Transitions --
   ----------------------------------

   function Analyze_Behavior_Transitions
     (Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);

      Success   : Boolean := True;
      Behavior_Transition : Node_Id;

   begin
      Behavior_Transition := BATN.First_Node (Transitions (BA_Root));
      while Present (Behavior_Transition) loop
         if Present (Behavior_Transition_Idt
                     (Transition (Behavior_Transition)))
         then
            Success := Success and then
              Test_With_Identifiers_In_Parent_Component
                (Behavior_Transition_Idt
                   (Transition (Behavior_Transition)), Root, Parent_Component);
         end if;
         Success := Success and then Analyze_Behavior_Transition
           (Transition (Behavior_Transition),
            Root,
            BA_Root, Parent_Component);
         Behavior_Transition := BATN.Next_Node (Behavior_Transition);
      end loop;
      return Success;
   end Analyze_Behavior_Transitions;

   ---------------------------------
   -- Analyze_Behavior_Transition --
   ---------------------------------

   function Analyze_Behavior_Transition
     (Transition        : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);

      Source_Transition_List : constant List_Id := Sources (Transition);
      Destination_Transition : constant Node_Id
        := BATN.Destination (Transition);
      Success   : Boolean := True;
      List_Node : Node_Id;
      Pointed_State : Node_Id;
      Cond_Node : Node_Id;

   begin
      if not Is_Empty (Source_Transition_List) then
         List_Node := BATN.First_Node (Source_Transition_List);

         while Present (List_Node) loop
            Pointed_State := Find_State (List_Node, BA_Root);
            if No (Pointed_State) then
               Error_Loc (1) := BATN.Loc (List_Node);

               Error_Name (1) := BATN.Display_Name (List_Node);
               DE ("#is not found in States");

               Success := False;
            else
               BATN.Set_Corresponding_Entity (List_Node, Pointed_State);
            end if;
            List_Node := BATN.Next_Node (List_Node);
         end loop;
      end if;

      Pointed_State := Find_State (Destination_Transition, BA_Root);
      if No (Pointed_State) then
         Error_Loc (1) := BATN.Loc (Destination_Transition);

         Error_Name (1) := BATN.Display_Name (Destination_Transition);
         DE ("#is not found in States");

         Success := False;
      else
         BATN.Set_Corresponding_Entity (Destination_Transition, Pointed_State);
      end if;

      if Success then
         if Present (Behavior_Condition (Transition))
         then
            Cond_Node := Condition (Behavior_Condition (Transition));
            if Kind (Cond_Node) = K_Dispatch_Condition_Thread
              and then Component_Category'Val
                (Category (Parent_Component)) = CC_Subprogram
            then
               Error_Loc (1) := BATN.Loc (Node_Id (Source_Transition_List));
               DE ("Subprogram components must not contain" &
                     " a dispatch condition in any of its transitions");
               Success := False;
            elsif Kind (Cond_Node) = K_Execute_Condition then
               if Present (BATN.Value_Expression (Cond_Node)) then
                  Success := Success and then
                    Analyze_BA_Value_Expression
                      (Node              => Value_Expression (Cond_Node),
                       Root              => Root,
                       BA_Root           => BA_Root,
                       Parent_Component  => Parent_Component,
                       Is_Parameter_Expr => False,
                       Is_Out_Parameter  => False);
               end if;
            end if;
         end if;
      end if;

      if Success then
         List_Node := BATN.First_Node (Source_Transition_List);

         while Present (List_Node) loop
            if (Is_Final (BA_Root, List_Node))
            then
               if not Is_Initial (BA_Root, List_Node) and then
                 not Is_Complete (BA_Root, List_Node)
               then
                  Error_Loc (1) := BATN.Loc (List_Node);

                  Error_Name (1) := BATN.Display_Name (List_Node);
                  DE ("#Transitions out " &
                        "of final states are not allowed");
                  Success := False;
               end if;
            end if;

            if Present (Behavior_Condition (Transition))
            then
               Cond_Node := Condition (Behavior_Condition (Transition));
               if Kind (Cond_Node) = K_Dispatch_Condition_Thread
                 and then not Is_Complete (BA_Root, List_Node)
               then
                  Error_Loc (1) := BATN.Loc (List_Node);

                  Error_Name (1) := BATN.Display_Name (List_Node);
                  DE ("#Only transition " &
                        "out of complete states may have dispatch condition");
                  Success := False;
               end if;
            end if;
            if (Is_Complete (BA_Root, List_Node) and then
                  not Present (Behavior_Condition (Transition)))
              or else (Is_Complete (BA_Root, List_Node) and then
                       Kind (Condition (Behavior_Condition (Transition)))
                       /= K_Dispatch_Condition_Thread)
            then
               Error_Loc (1) := BATN.Loc (List_Node);

               Error_Name (1) := BATN.Display_Name (List_Node);
               DE ("#Transitions out " &
                     "of complete states must have dispatch condition");
               Success := False;
            end if;

            List_Node := BATN.Next_Node (List_Node);
         end loop;
      end if;

      if Present (Behavior_Condition (Transition)) and then
        Kind (Condition (Behavior_Condition (Transition)))
        = K_Dispatch_Condition_Thread
      then
         Success := Success and then Analyze_Dispatch_Condition
           (Condition (Behavior_Condition (Transition)), Root,
            BA_Root, Parent_Component);
      end if;

      --  Analyze the Behavior_Action_Block of the transition

      if Success  and then
        Present (BATN.Behavior_Action_Block (Transition)) and then
        BATN.Kind (Transition) = BATN.K_Execution_Behavior_Transition
      then
         Success := Analyze_Behavior_Action_Block
           (BATN.Behavior_Action_Block (Transition),
            Root,
            BA_Root,
            Parent_Component);
      end if;

      return Success;
   end Analyze_Behavior_Transition;

   -----------------------------------
   -- Analyze_Behavior_Action_Block --
   -----------------------------------

   function Analyze_Behavior_Action_Block
     (Action_Block      : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (Action_Block) = BATN.K_Behavior_Action_Block);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
   begin

      return Analyze_Behav_Acts (Action_Block, Root,
                                 BA_Root, Parent_Component);
   end Analyze_Behavior_Action_Block;

   ------------------------
   -- Analyze_Behav_Acts --
   ------------------------

   function Analyze_Behav_Acts
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (Node) = BATN.K_Behavior_Action_Block
                     or else BATN.Kind (Node) = K_Conditional_Statement
                     or else BATN.Kind (Node) = K_While_Cond_Structure
                     or else BATN.Kind (Node) = K_For_Cond_Structure
                     or else BATN.Kind (Node) = K_ForAll_Cond_Structure);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);

      Success           : Boolean := True;
      Behav_actions     : Node_Id;
      Behav_action      : Node_Id;
   begin

      Behav_actions := BATN.Behav_Acts (Node);

      --  In the case of a sequence of behavior actions

      if Present (Behav_Actions) and then
        not Is_Empty (BATN.Behavior_Action_Sequence (Behav_actions))
      then

         Behav_action := BATN.First_Node
           (BATN.Behavior_Action_Sequence (Behav_actions));

         while Present (Behav_action) loop
            Success := Success and then Analyze_Behavior_Action
              (Behav_action, Root, BA_Root, Parent_Component,
               Scope_BA_Entities (Behav_Acts (Node)));

            Behav_action := BATN.Next_Node (Behav_action);
         end loop;
      end if;

      --  In the case of a set of behavior actions:

      if Present (Behav_Actions) and then
        not Is_Empty (BATN.Behavior_Action_Set (Behav_actions))
      then
         Behav_action := BATN.First_Node
           (BATN.Behavior_Action_Set (Behav_actions));

         while Present (Behav_action) loop
            Success := Success and then Analyze_Behavior_Action
              (Behav_action, Root, BA_Root, Parent_Component,
               Scope_BA_Entities (Behav_Acts (Node)));

            Behav_action := BATN.Next_Node (Behav_action);
         end loop;
      end if;

      --  In the case of a single action

      if Present (Behav_Actions) and then
        Present (BATN.Behavior_Action (Behav_actions)) and then
        Is_Empty (BATN.Behavior_Action_Sequence (Behav_actions)) and then
        Is_Empty (BATN.Behavior_Action_Set (Behav_actions))
      then
         Success := Success and then Analyze_Behavior_Action
           (BATN.Behavior_Action (Behav_actions),
            Root, BA_Root, Parent_Component,
            Scope_BA_Entities (Behav_Acts (Node)));
      end if;

      return Success;
   end Analyze_Behav_Acts;

   -----------------------------
   -- Analyze_Behavior_Action --
   -----------------------------

   function Analyze_Behavior_Action
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
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

      Success         : Boolean := True;
      Action_Node : constant Node_Id := BATN.Action (Node);

   begin

      case BATN.Kind (Action_Node) is

      when K_If_Cond_Struct         =>
         Success := Analyze_If_Cond_Struct
           (Action_Node, Root, BA_Root, Parent_Component, Scope_BA_Entities);

      when K_For_Cond_Structure     =>
         Success := Analyze_For_or_ForAll_Cond_Struct
           (Action_Node, Root, BA_Root, Parent_Component,
            Scope_BA_Entities);

      when K_While_Cond_Structure   =>
         Success := Analyze_While_Cond_Struct
           (Action_Node, Root, BA_Root, Parent_Component, Scope_BA_Entities);

      when K_ForAll_Cond_Structure  =>
         Success := Analyze_For_or_ForAll_Cond_Struct
           (Action_Node, Root, BA_Root, Parent_Component,
            Scope_BA_Entities);

      when K_DoUntil_Cond_Structure =>
         Success := Analyze_DoUntil_Cond_Struct
           (Action_Node, BA_Root, Parent_Component); --  , Scope_BA_Entities);

      when BATN.K_Assignment_Action =>
         Success := Analyze_Assignment_Action
           (Action_Node, Root, BA_Root, Parent_Component, Scope_BA_Entities);

      when K_Communication_Action   =>
         Success := Analyze_Communication_Action
           (Action_Node, Root, BA_Root, Parent_Component);
         --  , Scope_BA_Entities);

      when K_Timed_Act              =>
         Success := Analyze_Timed_Action
           (Action_Node, BA_Root, Parent_Component);

      when others                   =>
         Error_Loc (1) := BATN.Loc (Action_Node);

         Error_Name (1) := BATN.Display_Name (Action_Node);
         DE ("#Action kind does not exist");
         Success := False;

      end case;

      return Success;

   end Analyze_Behavior_Action;

   ----------------------------
   -- Analyze_If_Cond_Struct --
   ----------------------------

   function Analyze_If_Cond_Struct
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (Node) = BATN.K_If_Cond_Struct);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      Success : boolean;
      N       : Node_Id;
   begin
      Success := Analyze_BA_Value_Expression
           (Node              => Logical_Expr (If_Statement (Node)),
            Root              => Root,
            BA_Root           => BA_Root,
            Parent_Component  => Parent_Component,
            Is_Parameter_Expr => False,
            Is_Out_Parameter  => False,
            Scope_BA_Entities => Scope_BA_Entities);

      BATN.Set_Scope_BA_Entities (BATN.Behav_Acts (If_Statement (Node)),
                                  Scope_BA_Entities);

      Success := Success and then Analyze_Behav_Acts
        (Node             => If_Statement (Node),
         Root             => Root,
         BA_Root          => BA_Root,
         Parent_Component => Parent_Component);

      if not Is_Empty (Elsif_Statement (Node)) then
         N := BATN.First_Node (Elsif_Statement (Node));
         while Present (N) loop
            Success := Success and then Analyze_BA_Value_Expression
              (Node              => Logical_Expr (N),
               Root              => Root,
               BA_Root           => BA_Root,
               Parent_Component  => Parent_Component,
               Is_Parameter_Expr => False,
               Is_Out_Parameter  => False,
               Scope_BA_Entities => Scope_BA_Entities);

            BATN.Set_Scope_BA_Entities (BATN.Behav_Acts (N),
                                        Scope_BA_Entities);
            Success := Success and then Analyze_Behav_Acts
              (Node             => N,
               Root             => Root,
               BA_Root          => BA_Root,
               Parent_Component => Parent_Component);
            N := BATN.Next_Node (N);
         end loop;
      end if;

      if Present (Else_Statement (Node)) then
         BATN.Set_Scope_BA_Entities (BATN.Behav_Acts (Else_Statement (Node)),
                                     Scope_BA_Entities);
         Success := Success and then Analyze_Behav_Acts
           (Node             => Else_Statement (Node),
            Root             => Root,
            BA_Root          => BA_Root,
            Parent_Component => Parent_Component);
      end if;

      return Success;

   end Analyze_If_Cond_Struct;

   ---------------------------------------
   -- Analyze_For_or_ForAll_Cond_Struct --
   ---------------------------------------

   function Analyze_For_or_ForAll_Cond_Struct
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_For_Cond_Structure
                     or else BATN.Kind (Node) = BATN.K_ForAll_Cond_Structure);
      Success : boolean;
      L       : List_Id;
   begin

      if Present (BATN.Classifier_Ref (Node)) then
         Success := Link_Variable (Root, Node);
         if not Success then

            Error_Loc (1) := BATN.Loc (Element_Idt (Node));

            DE ("counter of for structure in behavior specification"
                & " must be explicitly typed"
                & " with a valid data component classifier.");
         end if;
      end if;

      if Present (In_Element_Values (Node)) then

         if BATN.Kind (In_Element_Values (Node)) = BATN.K_Integer_Range then
            Success := Success and then Analyze_BA_Integer_Value
              (Node             => BATN.Lower_Int_Val
                 (In_Element_Values (Node)),
               Root             => Root,
               BA_Root          => BA_Root,
               Parent_Component => Parent_Component)
              and then Analyze_BA_Integer_Value
              (Node             => BATN.Upper_Int_Val
                 (In_Element_Values (Node)),
               Root             => Root,
               BA_Root          => BA_Root,
               Parent_Component => Parent_Component);
         end if;
      end if;

      if Is_Empty (Scope_BA_Entities) then
         L := New_List (BATN.K_BA_Entity_List, No_Location);
      else
         L := Scope_BA_Entities;
      end if;

      Append_Node_To_List (Node, L);

      BATN.Set_Scope_BA_Entities (BATN.Behav_Acts (Node), L);

      Success := Success and then Analyze_Behav_Acts
           (Node             => Node,
            Root             => Root,
            BA_Root          => BA_Root,
            Parent_Component => Parent_Component);

      Remove_Node_From_List (Node, L);
      BATN.Set_Scope_BA_Entities (BATN.Behav_Acts (Node), L);

      return Success;
   end Analyze_For_or_ForAll_Cond_Struct;

   -------------------------------
   -- Analyze_While_Cond_Struct --
   -------------------------------

   function Analyze_While_Cond_Struct
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_While_Cond_Structure);
      Success : Boolean;
   begin
      Success := Analyze_BA_Value_Expression
           (Node              => Logical_Expr (Node),
            Root              => Root,
            BA_Root           => BA_Root,
            Parent_Component  => Parent_Component,
            Is_Parameter_Expr => False,
            Is_Out_Parameter  => False,
            Scope_BA_Entities => Scope_BA_Entities);

      BATN.Set_Scope_BA_Entities (BATN.Behav_Acts (Node),
                                  Scope_BA_Entities);
      Success := Success and then Analyze_Behav_Acts
        (Node             => Node,
         Root             => Root,
         BA_Root          => BA_Root,
         Parent_Component => Parent_Component);
      return Success;
   end Analyze_While_Cond_Struct;

   ---------------------------------
   -- Analyze_DoUntil_Cond_Struct --
   ---------------------------------

   function Analyze_DoUntil_Cond_Struct
     (Node              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_DoUntil_Cond_Structure);

   begin
      return True;
   end Analyze_DoUntil_Cond_Struct;

   -------------------------------
   -- Analyze_Assignment_Action --
   -------------------------------

   function Analyze_Assignment_Action
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Scope_BA_Entities : List_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Assignment_Action);
      Success     : Boolean := True;
   begin
      Success := Link_Target (BATN.Target (Node), Root, BA_Root,
                              Parent_Component);

      if Present (BATN.Value_Expression (Node)) then

         Success := Success and then Analyze_BA_Value_Expression
           (Node              => BATN.Value_Expression (Node),
            Root              => Root,
            BA_Root           => BA_Root,
            Parent_Component  => Parent_Component,
            Is_Parameter_Expr => False,
            Is_Out_Parameter  => False,
            Scope_BA_Entities => Scope_BA_Entities);

      end if;

      return Success;
   end Analyze_Assignment_Action;

   --------------------------------------
   -- Analyze_Data_Component_Reference --
   --------------------------------------

   function Analyze_Data_Component_Reference
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (Node) = BATN.K_Data_Component_Reference);
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      Success : Boolean := True;
      N : Node_Id;
   begin
      if not Is_Empty (BATN.Identifiers (Node)) then
         N := BATN.First_Node (BATN.Identifiers (Node));

         while Present (N) loop
            Success := Success and then
              Analyze_BA_Name  (N, Root, BA_Root, Parent_Component);
            exit when not Success;
            N := BATN.Next_Node (N);
         end loop;
      end if;

      return Success;
   end Analyze_Data_Component_Reference;

   ---------------------
   -- Analyze_BA_Name --
   ---------------------

   function Analyze_BA_Name
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (Node) = BATN.K_Name);
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      Success : Boolean := True;
      N       : Node_Id;
   begin
      if not Is_Empty (BATN.Idt (Node)) then

         N := BATN.First_Node (BATN.Idt (Node));
         while Present (N) loop
            Success := Success and then Analyze_Value
              (Node              => N,
               Root              => Root,
               BA_Root           => BA_Root,
               Parent_Component  => Parent_Component);
            N := BATN.Next_Node (N);
         end loop;

         if not Is_Empty (BATN.Array_Index (Node)) then
            N := BATN.First_Node (BATN.Array_Index (Node));

            while Present (N) loop
               Success := Success and then
                 Analyze_BA_Integer_Value (N, Root, BA_Root, Parent_Component);
               exit when not Success;
               N := BATN.Next_Node (N);
            end loop;
         end if;
      end if;
      return Success;
   end Analyze_BA_Name;

   -------------------------------
   -- Analyze_BA_Value_Variable --
   -------------------------------

   function Analyze_BA_Value_Variable
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Value_Variable);

      Success : Boolean := True;
   begin

      if BATN.Kind (BATN.Identifier (Node)) = BATN.K_Name then
         Success := Analyze_BA_Name
           (Node             => BATN.Identifier (Node),
            Root             => Root,
            BA_Root          => BA_Root,
            Parent_Component => Parent_Component);

      elsif BATN.Kind (BATN.Identifier (Node))
        = BATN.K_Data_Component_Reference
      then
         Success := Analyze_Data_Component_Reference
           (Node             => BATN.Identifier (Node),
            Root             => Root,
            BA_Root          => BA_Root,
            Parent_Component => Parent_Component);
      end if;

      if Is_Interrogative (Node) then
         Success := Success and then Link_Input_Port
           (Node             => Node,
            Root             => Root,
            BA_Root          => BA_Root,
            Parent_Component => Parent_Component);

         if not Success then
            Error_Loc (1) := BATN.Loc (BATN.First_Node
                                       (BATN.Idt (BATN.Identifier (Node))));
            DE ("Only Incoming port Name are allowed.");
         end if;

      elsif Is_Count (Node) or else Is_Fresh (Node)
        or else Is_Updated (Node)
      then
         Success := Success and then
           not No (Find_and_Link_Event_Port_Of_Parent_Component
                   (Node             => Node,
                    Root             => Root,
                    Parent_Component => Parent_Component));

         if not Success then
            Error_Loc (1) := BATN.Loc (BATN.First_Node
                                       (BATN.Idt (BATN.Identifier (Node))));
            DE ("Count, Fresh and Updated operators are applied only"
                & " on Data_Event or Event ports.");
         end if;
      end if;

      return Success;
   end Analyze_BA_Value_Variable;

   ------------------------------
   -- Analyze_BA_Integer_Value --
   ------------------------------

   function Analyze_BA_Integer_Value
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Integer_Value);
      pragma Assert (BATN.Kind (BATN.Entity (Node)) = K_Value_Variable
                       or else BATN.Kind (BATN.Entity (Node)) = K_Literal
                     or else BATN.Kind (BATN.Entity (Node))
                     = K_Property_Constant);

      Entity_Node : constant Node_Id := BATN.Entity (Node);
      Success : Boolean := True;
   begin
      if BATN.Kind (Entity_Node) = K_Value_Variable then
            Success := Analyze_BA_Value_Variable
              (Node             => Entity_Node,
               Root             => Root,
               BA_Root          => BA_Root,
               Parent_Component => Parent_Component);
      elsif BATN.Kind (Entity_Node) = K_Property_Constant then
            Success := Analyze_Value
              (Node             => Entity_Node,
               Root             => Root,
               BA_Root          => BA_Root,
               Parent_Component => Parent_Component);
      end if;
      return Success;
   end Analyze_BA_Integer_Value;

   -----------------
   -- Link_Target --
   -----------------

   function Link_Target
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      Success     : Boolean := True;
      Target_Idt  : Node_Id;
      N           : Node_Id := No_Node;
   begin

      if BATN.Kind (Node) = BATN.K_Name
        and then not Is_Empty (BATN.Idt (Node))
      then
         Target_Idt := BATN.First_Node
              (BATN.Idt (Node));
         if Length (BATN.Idt (Node)) = 1 then
            --  Target must be a BA_Variable or a parameter/requires
            --  data access of the subprogram implementing the BA
            --  or a subcomponent of the Parent_Component.

            if No (Find_BA_Variable (Target_Idt, BA_Root))
              and then No (Find_Out_Parameter_Of_Parent_Component
                           (Target_Idt, Root, Parent_Component))
              and then No (Find_Requires_Data_Access_Of_Parent_Component
                           (Target_Idt, Root, Parent_Component))
              and then No (Find_Output_Port_Of_Parent_Component
                           (Target_Idt, Root, Parent_Component))
              and then No (Find_Data_Subcomponent_Of_Parent_Component
                           (Target_Idt, Root, Parent_Component))
            then
               Success := False;
               Error_Loc (1)  := BATN.Loc (Target_Idt);
               Error_Name (1) := BATN.Display_Name (Target_Idt);
               DE ("(" & Get_Name_String (Remove_Prefix_From_Name
                   ("%ba%", BATN.Display_Name
                      (Target_Idt))) & ")"
                   & " does not point to"
                   & " any variable of the current BA or a parameter"
                   & " or a required data access feature"
                   & " of the subprogram having"
                   & " the current BA;"
                   & " or a data subcomponent or an output port"
                   & " of the parent component.");
            end if;

            if Present (Find_Out_Parameter_Of_Parent_Component
                        (Target_Idt, Root, Parent_Component))
            then
               N := Find_Out_Parameter_Of_Parent_Component
                 (Target_Idt, Root, Parent_Component);

            elsif Present (Find_Output_Port_Of_Parent_Component
                           (Target_Idt, Root, Parent_Component))
            then
               N := Find_Output_Port_Of_Parent_Component
                 (Target_Idt, Root, Parent_Component);
            elsif Present (Find_Data_Subcomponent_Of_Parent_Component
                           (Target_Idt, Root, Parent_Component))
            then
               N := Find_Data_Subcomponent_Of_Parent_Component
                 (Target_Idt, Root, Parent_Component);
            end if;

            if Present (N) then
               BATN.Set_Corresponding_Entity
                 (BATN.First_Node
                    (BATN.Idt (Node)), N);
            end if;

         else
            --  i.e.  Length (BATN.Idt (Node)) > 1
            Success := Analyze_BA_Name
              (Node, Root, BA_Root, Parent_Component);
         end if;
      elsif BATN.Kind (Node) = BATN.K_Data_Component_Reference
      then
         Success := Analyze_Data_Component_Reference
              (Node, Root, BA_Root, Parent_Component);
      end if;

      return Success;
   end Link_Target;

   ----------------------------------
   -- Analyze_Communication_Action --
   ----------------------------------

   function Analyze_Communication_Action
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Communication_Action);

      Success : Boolean := True;

   begin

      case Communication_Kind'Val (Comm_Kind (Node)) is

      when CK_Exclamation      =>
         --  This means we have a call to a subprogram
         --  or a port sending
         Success := Analyze_Spg_Call_Or_Port_Sending
           (Node, Root, BA_Root, Parent_Component);

      when CK_Interrogative    =>
         Success := Link_Input_Port
           (Node, Root, BA_Root, Parent_Component);
         if Present (BATN.Target (Node)) then
            Success := Success and then Link_Target
              (BATN.Target (Node), Root, BA_Root,
               Parent_Component);
         end if;

         --  when CK_Greater_Greater  => Success := ;
         --  when CK_Exclamation_Greater => Success := ;
         --  when CK_Exclamation_Lesser  => Success := ;
      when others              =>
         return Success;
      end case;

      return Success;

   end Analyze_Communication_Action;

   --------------------------------------
   -- Analyze_Spg_Call_Or_Port_Sending --
   --------------------------------------
   function Analyze_Spg_Call_Or_Port_Sending
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Communication_Action);

      Success : Boolean := True;

   begin

      if Kind (BATN.Identifier (Node)) = BATN.K_Name then

         Success := Is_A_Required_Spg_Access_Of_the_Parent_Component
           (Node, Root, BA_Root, Parent_Component)
           or else Link_Spg (Node, Root, BA_Root, Parent_Component)
           or else Link_Output_Or_Internal_Port
             (Node, Root, BA_Root, Parent_Component);

         if not Success then

            --  This means that the identifier of the communication action
            --  does not refer to a valid subprogram or or a port name

            Error_Loc (1) := BATN.Loc (BATN.First_Node
                                       (BATN.Idt (BATN.Identifier (Node))));
            Error_Name (1) := BATN.Display_Name
              (BATN.First_Node
                 (BATN.Idt (BATN.Identifier (Node))));
            DE ("(" & Get_Name_String (Remove_Prefix_From_Name
                ("%ba%", BATN.Display_Name
                   (BATN.First_Node
                      (BATN.Idt (BATN.Identifier (Node)))))) & ")"
                & " does not point to"
                & " a valid subprogram name or a port name.");
         else
            if Link_Spg (Node, Root, BA_Root, Parent_Component) then
               Success := Check_Spg_Params
                 (Node, Root, BA_Root, Parent_Component);
            elsif Link_Output_Or_Internal_Port
              (Node, Root, BA_Root, Parent_Component)
            then
               Success := Check_Send_Output_Param
                 (Node, Root, BA_Root, Parent_Component);
            end if;
         end if;

      end if;
      return Success;

   end Analyze_Spg_Call_Or_Port_Sending;

   ---------------
   -- Link_Port --
   ---------------

--     function Link_Port
--       (Node              : Node_Id;
--        Root              : Node_Id;
--        BA_Root           : Node_Id;
--        Parent_Component  : Node_Id)
--        return Boolean
--     is
--        use ATN;
--        pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
--        pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
--        pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
--                       or else Kind (Parent_Component) =
--                         ATN.K_Component_Implementation);
--        pragma Assert (Kind (BATN.Identifier (Node)) = BATN.K_Name);
--
--        Success                  : Boolean := False;
--        F                        : Node_Id;
--        Type_Of_Parent_Component : Node_Id := No_Node;
--
--     begin
--
--        if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
--           Type_Of_Parent_Component := Parent_Component;
--        else
--           Type_Of_Parent_Component := Get_Component_Type_From_Impl
--             (Root, Parent_Component);
--        end if;
--
--        if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then
--
--           F := ATN.First_Node (ATN.Features (Type_Of_Parent_Component));
--
--           while Present (F) loop
--
--              if ATN.Kind (F) = K_Port_Spec
--                and then Get_Name_String
--                  (Utils.To_Lower
--                     (Remove_Prefix_From_Name
--                        ("%ba%",
--                         BATN.Display_Name
--                           (BATN.First_Node
--                                (BATN.Idt (BATN.Identifier (Node)))))))
--                  = Get_Name_String
--                (Utils.To_Lower (ATN.Name (ATN.Identifier (F))))
--              then
--                 Success := True;
--                 BATN.Set_Corresponding_Entity
--                   (BATN.First_Node
--                      (BATN.Idt (BATN.Identifier (Node))), F);
--              end if;
--
--              exit when Success /= False;
--              F := ATN.Next_Node (F);
--           end loop;
--        end if;
--
--        return Success;
--     end Link_Port;

   ----------------------------------
   -- Link_Output_Or_Internal_Port --
   ----------------------------------

   function Link_Output_Or_Internal_Port
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (Kind (BATN.Identifier (Node)) = BATN.K_Name);

      Success                  : Boolean := False;
      F                        : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;

   begin

      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

         F := ATN.First_Node (ATN.Features (Type_Of_Parent_Component));

         while Present (F) loop

            if ATN.Kind (F) = K_Port_Spec
              and then ATN.Is_Out (F)
              and then (Is_Data (F) or else Is_Event (F))
              and then
                Get_Name_String
                  (Utils.To_Lower
                     (Remove_Prefix_From_Name
                        ("%ba%",
                         BATN.Display_Name
                           (BATN.First_Node
                                (BATN.Idt (BATN.Identifier (Node)))))))
                = Get_Name_String
              (Utils.To_Lower (ATN.Name (ATN.Identifier (F))))
            then
               Success := True;
               BATN.Set_Corresponding_Entity
                 (BATN.First_Node
                    (BATN.Idt (BATN.Identifier (Node))), F);
            end if;

            exit when Success /= False;
            F := ATN.Next_Node (F);
         end loop;
      end if;

      return Success;
   end Link_Output_Or_Internal_Port;

   -----------------------------
   -- Check_Send_Output_Param --
   -----------------------------

   function Check_Send_Output_Param
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (Kind (BATN.Identifier (Node)) = BATN.K_Name);

      Success : Boolean := False;
      N       : Node_Id;
   begin

      if Length (Subprogram_Parameter_List (Node)) > 1 then
         Success := False;
         Error_Loc (1) := BATN.Loc (BATN.First_Node
                                    (BATN.Idt (BATN.Identifier (Node))));
         Error_Name (1) := BATN.Display_Name
           (BATN.First_Node
              (BATN.Idt (BATN.Identifier (Node))));
         DE ("This communication action is INVALID : as"
             & " (" & Get_Name_String (Remove_Prefix_From_Name
               ("%ba%", BATN.Display_Name
                  (BATN.First_Node
                     (BATN.Idt (BATN.Identifier (Node)))))) & ")"
             & " is an output port then the parameters number"
             & " of this communication action must be zero or one at most.");

      elsif Length (Subprogram_Parameter_List (Node)) <= 1 then
         Success := True;
         if Length (Subprogram_Parameter_List (Node)) = 1 then

            --  Now we must verify the parameter given to put_value in
            --  the output port is also a port
            --  example : p!(p1)
            --
            N := BATN.First_Node (Subprogram_Parameter_List (Node));

            Success := Analyze_BA_Value_Expression
                    (Node              => Parameter (N),
                     Root              => Root,
                     BA_Root           => BA_Root,
                     Parent_Component  => Parent_Component,
                     Is_Parameter_Expr => False,
                     Is_Out_Parameter  => False);

         end if;
      end if;

      return Success;
   end Check_Send_Output_Param;

   ------------------------------------------------------
   -- Is_A_Required_Spg_Access_Of_the_Parent_Component --
   ------------------------------------------------------

   function Is_A_Required_Spg_Access_Of_the_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is

      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);

      Success                  : Boolean := False;
      F                        : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;
   begin
      --  1) Check the called Subprogram Name : i.e.  It must refer to
      --  a Subprogram declared in the AADL model or a required subprogram
      --  subprogram access of the parent_component

      --  1.a) a required subprogram subprogram access
      --  of the parent_component

      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

         F := ATN.First_Node (ATN.Features (Type_Of_Parent_Component));

         while Present (F) loop

            if ATN.Kind (F) = K_Subcomponent_Access
              and then Component_Category'Val (Subcomponent_Category (F))
                  = CC_Subprogram
              and then not ATN.Is_Provided (F)
              and then
                Get_Name_String
                  (Utils.To_Lower
                     (BATN.Display_Name
                           (BATN.First_Node
                                (BATN.Idt (BATN.Identifier (Node))))))
                = Get_Name_String
              (Utils.To_Lower (ATN.Name (ATN.Identifier (F))))
            then
               Success := True;
               BATN.Set_Corresponding_Entity
                 (BATN.First_Node
                    (BATN.Idt (BATN.Identifier (Node))), F);
            end if;

            exit when Success /= False;
            F := ATN.Next_Node (F);
         end loop;
      end if;

      return Success;

   end Is_A_Required_Spg_Access_Of_the_Parent_Component;

   --------------
   -- Link_Spg --
   --------------

   function Link_Spg
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is

      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);

      Success              : Boolean := False;
      N1                   : Node_Id;
      List_Node            : Node_Id;
   begin
      --  1) Check the called Subprogram Name : i.e.  It must refer to
      --  a Subprogram declared in the AADL model or a required subprogram
      --  subprogram access of the parent_component

      --  1.b) a Subprogram declared in the AADL model

      if not ANU.Is_Empty (ATN.Declarations (Root))
      then
         List_Node := ATN.First_Node
           (ATN.Declarations (Root));

         while Present (List_Node) loop
            if ATN.Kind (List_Node) = ATN.K_Package_Specification then

               N1 := ATN.First_Node (Declarations (List_Node));
               while Present (N1) loop
                  if ((ATN.Kind (N1) = ATN.K_Component_Type)
                      or else
                        (ATN.Kind (N1) = ATN.K_Component_Implementation))
                  then

                     if Component_Category'Val (Category (N1)) = CC_Subprogram
                       and then
                         Get_Name_String
                           (Utils.To_Lower (ATN.Name (ATN.Identifier (N1))))
                       = Get_Name_String
                       (Utils.To_Lower
                          (BATN.Display_Name
                               (BATN.First_Node
                                    (BATN.Idt (BATN.Identifier (Node))))))

                     then
                        BATN.Set_Corresponding_Entity
                          (BATN.First_Node
                             (BATN.Idt (BATN.Identifier (Node))), N1);
                        Success := True;
                     end if;
                  end if;
                  exit when Success;
                  N1 := ATN.Next_Node (N1);
               end loop;
            end if;

            List_Node := ATN.Next_Node (List_Node);
         end loop;
      end if;

      BATN.Set_Is_Subprogram_Call (Node, Success);

      return Success;
   end Link_Spg;

   ----------------------
   -- Check_Spg_Params --
   ----------------------

   function Check_Spg_Params
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is

      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);

      Success                  : Boolean := True;
      N1, N2                   : Node_Id;
      Spg_params               : List_Id;
   begin

      N1 := BATN.Corresponding_Entity
        (BATN.First_Node
           (BATN.Idt (BATN.Identifier (Node))));

      --  The identifier of the communication action
      --  is a valid subprogram_name

      --  Now we must verify the parameter list
      --  2) we check the consistency of the number
      --  of parameters
      --

      Spg_params := ATN.Features (N1);

      if ANU.Length (Spg_params) /=
        Length (Subprogram_Parameter_List (Node))
      then

         Success := False;
         Error_Loc (1) := BATN.Loc (BATN.First_Node
                                    (BATN.Idt (BATN.Identifier (Node))));
         Error_Name (1) := BATN.Display_Name
           (BATN.First_Node
              (BATN.Idt (BATN.Identifier (Node))));
         DE ("The number of parameters in the Subprogram "
             & "(" & Get_Name_String (Remove_Prefix_From_Name
               ("%ba%", BATN.Display_Name
                  (BATN.First_Node
                     (BATN.Idt (BATN.Identifier (Node)))))) & ")"
             & " called in the BA is not consistent with"
             & " the parameter number in its declaration"
             & " in the AADL model at "
             & Locations.Image (ATN.Loc (N1)) & ".");
      end if;

      --  The called Subprogram name and the number
      --  of parameters are valid
      --  3) Check the given parameters :
      --
      if Success and then
        not Is_Empty (Subprogram_Parameter_List (Node))
      then

         N1 := BATN.First_Node (Subprogram_Parameter_List (Node));
         N2 := ATN.First_Node (Spg_params);

         while Success and then Present (N2)
           and then Present (N1) loop

            if ATN.Kind (N2) = ATN.K_Parameter then

               if ATN.Is_Out (N2) then

                  --  i.e. N2 is OUT or INOUT parameter
                  --  in this case the value_expression of N1
                  --  should be either a variable of the current BA
                  --  or an OUT/INOUT parameter of the subprogram
                  --  Parent_Component.
                  --  If the analyze of the expression value of N1
                  --  gives that it is a BA variable or a OUT/INOUT
                  --  parameter of the subprogram Parent_Component
                  --  then we check the type of the parameter
                  --  See function Analyze_Value
                  --
                  Success := Analyze_BA_Value_Expression
                    (Node              => Parameter (N1),
                     Root              => Root,
                     BA_Root           => BA_Root,
                     Parent_Component  => Parent_Component,
                     Is_Parameter_Expr => True,
                     Is_Out_Parameter  => True,
                     Parameter_Type    => ATN.Full_Identifier
                       (ATN.Entity_Ref (N2)));

                  BATN.Set_Is_Out (N1, True);

                  if ATN.Is_In (N2) then
                     BATN.Set_Is_In (N1, True);
                  else
                     BATN.Set_Is_In (N1, False);
                  end if;

               else

                  --  In the case of a NON OUT parameter
                  --  The parameter should be any valid expression
                  --
                  Success := Analyze_BA_Value_Expression
                    (Node              => Parameter (N1),
                     Root              => Root,
                     BA_Root           => BA_Root,
                     Parent_Component  => Parent_Component,
                     Is_Parameter_Expr => True,
                     Is_Out_Parameter  => False);

                  BATN.Set_Is_Out (N1, False);
                  BATN.Set_Is_In (N1, True);

               end if;
            end if;

            N1 := BATN.Next_Node (N1);
            N2 := ATN.Next_Node (N2);
         end loop;

      end if;

      return Success;
   end Check_Spg_Params;

   ---------------------
   -- Link_Input_Port --
   ---------------------

   function Link_Input_Port
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (Kind (BATN.Identifier (Node)) = BATN.K_Name);

      Success                  : Boolean := False;
      F                        : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;

   begin

      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

         F := ATN.First_Node (ATN.Features (Type_Of_Parent_Component));

         while Present (F) loop

            if ATN.Kind (F) = K_Port_Spec
              and then ATN.Is_In (F)
              and then (Is_Data (F) or else Is_Event (F))
              and then
                Get_Name_String
                  (Utils.To_Lower
                     (Remove_Prefix_From_Name
                        ("%ba%",
                         BATN.Display_Name
                           (BATN.First_Node
                                (BATN.Idt (BATN.Identifier (Node)))))))
                = Get_Name_String
              (Utils.To_Lower (ATN.Name (ATN.Identifier (F))))
            then
               Success := True;
               BATN.Set_Corresponding_Entity
                 (BATN.First_Node
                    (BATN.Idt (BATN.Identifier (Node))), F);
            end if;

            exit when Success /= False;
            F := ATN.Next_Node (F);
         end loop;
      end if;

      return Success;
   end Link_Input_Port;

   ---------------------------------
   -- Analyze_BA_Value_Expression --
   ---------------------------------

   function Analyze_BA_Value_Expression
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id := No_List)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (Node) = BATN.K_Value_Expression);

      Success : Boolean := True;
      N       : Node_Id;
   begin
      N := BATN.First_Node (BATN.Relations (Node));

      if Is_Parameter_Expr and then Is_Out_Parameter then
         if Length (BATN.Relations (Node)) = 1 then
            Success := Analyze_BA_Relation
              (N, Root, BA_Root,
               Parent_Component, Is_Parameter_Expr,
               Is_Out_Parameter, Parameter_Type, Scope_BA_Entities);
         else
            Success := False;
            Error_Loc (1) := BATN.Loc (N);
            DE ("OUT/INOUT parameters cannot have more than one Relation.");
         end if;
      else
         while Success and then Present (N) loop
            if BATN.Kind (N) = BATN.K_Relation then
               Success := Analyze_BA_Relation
                 (Node              => N,
                  Root              => Root,
                  BA_Root           => BA_Root,
                  Parent_Component  => Parent_Component,
                  Is_Parameter_Expr => Is_Parameter_Expr,
                  Is_Out_Parameter  => Is_Out_Parameter,
                  Scope_BA_Entities   => Scope_BA_Entities);
            end if;
            --  if BATN.Kind (N) = BATN.K_Operator then
            --  -- K_Operator doit etre relational_operator
            --  -- i.e. = | != | < | <= | > |>=
            --  end if

            N := BATN.Next_Node (N);
         end loop;
      end if;
      return Success;
   end Analyze_BA_Value_Expression;

   -------------------------
   -- Analyze_BA_Relation --
   -------------------------

   function Analyze_BA_Relation
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (Node) = BATN.K_Relation);

      Success : Boolean := True;
      N       : Node_Id;
   begin
      N := BATN.First_Node (BATN.Simple_Exprs (Node));

      if Is_Parameter_Expr and then Is_Out_Parameter then
         if Length (BATN.Simple_Exprs (Node)) = 1 then
            Success := Analyze_BA_Simple_Expression
              (N, Root, BA_Root,
               Parent_Component, Is_Parameter_Expr,
               Is_Out_Parameter, Parameter_Type, Scope_BA_Entities);
         else
            Success := False;
            Error_Loc (1) := BATN.Loc (N);
            DE ("OUT/INOUT parameters cannot have more than one"
                & " Simple Expression.");
         end if;
      else
         while Success and then Present (N) loop
            if BATN.Kind (N) = BATN.K_Simple_Expression then
               Success := Analyze_BA_Simple_Expression
                 (Node              => N,
                  Root              => Root,
                  BA_Root           => BA_Root,
                  Parent_Component  => Parent_Component,
                  Is_Parameter_Expr => Is_Parameter_Expr,
                  Is_Out_Parameter  => Is_Out_Parameter,
                  Scope_BA_Entities => Scope_BA_Entities);
            end if;

            N := BATN.Next_Node (N);
         end loop;
      end if;

      return Success;
   end Analyze_BA_Relation;

   ----------------------------------
   -- Analyze_BA_Simple_Expression --
   ----------------------------------

   function Analyze_BA_Simple_Expression
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (Node) = BATN.K_Simple_Expression);

      Success : Boolean := True;
      N       : Node_Id;
   begin

      N := BATN.First_Node (BATN.Term_And_Operator (Node));

      if Is_Parameter_Expr and then Is_Out_Parameter then
         if Length (BATN.Term_And_Operator (Node)) = 1 then
            Success := Analyze_Term
              (N, Root, BA_Root,
               Parent_Component, Is_Parameter_Expr,
               Is_Out_Parameter, Parameter_Type, Scope_BA_Entities);
         else
            Success := False;
            Error_Loc (1) := BATN.Loc (N);
            DE ("OUT/INOUT parameters cannot have more than one"
                & " Term.");
         end if;
      else
         while Success and then Present (N) loop
            if BATN.Kind (N) = BATN.K_Term then
               Success := Analyze_Term
                 (Node              => N,
                  Root              => Root,
                  BA_Root           => BA_Root,
                  Parent_Component  => Parent_Component,
                  Is_Parameter_Expr => Is_Parameter_Expr,
                  Is_Out_Parameter  => Is_Out_Parameter,
                  Scope_BA_Entities => Scope_BA_Entities);
            end if;

            N := BATN.Next_Node (N);
         end loop;
      end if;

      return Success;
   end Analyze_BA_Simple_Expression;

   ------------------
   -- Analyze_Term --
   ------------------

   function Analyze_Term
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (Node) = BATN.K_Term);

      Success : Boolean := True;
      N       : Node_Id;
   begin

      N := BATN.First_Node (BATN.Factors (Node));

      if Is_Parameter_Expr and then Is_Out_Parameter then
         if Length (BATN.Factors (Node)) = 1 then
            Success := Analyze_Factor
              (N, Root, BA_Root,
               Parent_Component, Is_Parameter_Expr,
               Is_Out_Parameter, Parameter_Type, Scope_BA_Entities);
         else
            Success := False;
            Error_Loc (1) := BATN.Loc (N);
            DE ("OUT/INOUT parameters cannot have more than one"
                & " Factor.");
         end if;
      else
         while Success and then Present (N) loop
            if BATN.Kind (N) = BATN.K_Factor then
               Success := Analyze_Factor
                 (Node              => N,
                  Root              => Root,
                  BA_Root           => BA_Root,
                  Parent_Component  => Parent_Component,
                  Is_Parameter_Expr => Is_Parameter_Expr,
                  Is_Out_Parameter  => Is_Out_Parameter,
                  Scope_BA_Entities => Scope_BA_Entities);
            end if;

            N := BATN.Next_Node (N);
         end loop;
      end if;

      return Success;
   end Analyze_Term;

   --------------------
   -- Analyze_Factor --
   --------------------

   function Analyze_Factor
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id)
      return Boolean

   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (Node) = BATN.K_Factor);

      Success : Boolean := True;
      Lower_val, Upper_val     : Node_Id;
   begin

      Lower_val := BATN.Lower_Value (Node);
      Upper_val := BATN.Upper_Value (Node);

      if Is_Parameter_Expr and then Is_Out_Parameter
        and then Present (Upper_val)
      then
         return False;
      end if;

      if Present (Lower_val) then
         Success := Analyze_Value
           (Lower_val, Root, BA_Root, Parent_Component,
            Is_Parameter_Expr, Is_Out_Parameter, Parameter_Type,
            Scope_BA_Entities);
      end if;

      if Success and then Present (Upper_val) then
         Success := Analyze_Value
           (Node              => Upper_val,
            Root              => Root,
            BA_Root           => BA_Root,
            Parent_Component  => Parent_Component,
            Is_Parameter_Expr => Is_Parameter_Expr,
            Is_Out_Parameter  => Is_Out_Parameter,
            Scope_BA_Entities => Scope_BA_Entities);
      end if;

      return Success;
   end Analyze_Factor;

   -------------------
   -- Analyze_Value --
   -------------------

   function Analyze_Value
     (Node              : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id;
      Is_Parameter_Expr : Boolean := False;
      Is_Out_Parameter  : Boolean := False;
      Parameter_Type    : Node_Id := No_Node;
      Scope_BA_Entities : List_Id := No_List)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (Node) = BATN.K_Value_Variable
                     or else BATN.Kind (Node) = BATN.K_Value_Expression
                     or else BATN.Kind (Node) = BATN.K_Literal
                     or else BATN.Kind (Node) = BATN.K_Boolean_Literal
                     or else BATN.Kind (Node) = BATN.K_Property_Constant
                     or else BATN.Kind (Node) = BATN.K_Property_Reference
                     or else BATN.Kind (Node) = BATN.K_Identifier);

      Success              : Boolean := True;
      BA_Var               : Node_Id := No_Node;
      Out_param            : Node_Id := No_Node;
      Requires_data_access : Node_Id := No_Node;
      For_counter          : Node_Id := No_Node;
      N                    : Node_Id := No_Node;
      Scope_BA_Entity      : Node_Id;
      Ident                : Node_Id;
   begin

      if Is_Parameter_Expr and then Is_Out_Parameter then

         if BATN.Kind (Node) /= BATN.K_Property_Constant then
            Success := False;
            Error_Loc (1) := BATN.Loc (Node);
            DE (" This parameter can't be OUT or INOUT ");
         else
            BA_Var := Find_BA_Variable (BATN.Identifier (Node), BA_Root);
            Out_param := Find_Out_Parameter_Of_Parent_Component
              (BATN.Identifier (Node), Root, Parent_Component);
            Requires_data_access :=
              Find_Requires_Data_Access_Of_Parent_Component
                (BATN.Identifier (Node), Root, Parent_Component);

            Success := Present (BA_Var) or else Present (Out_param)
              or else Present (Requires_data_access)
              or else Present
                (Find_Data_Subcomponent_Of_Parent_Component
                   (BATN.Identifier (Node), Root, Parent_Component))
              or else
                (Present
                   (Find_Output_Port_Of_Parent_Component
                      (Node             => BATN.Identifier (Node),
                       Root             => Root,
                       Parent_Component => Parent_Component))
                    and then
                      Present
                        (Find_Data_Port_Of_Parent_Component
                             (Node             => BATN.Identifier (Node),
                              Root             => Root,
                              Parent_Component => Parent_Component)));

            if not Is_Empty (Scope_BA_Entities) then
               Scope_BA_Entity := BATN.First_Node (Scope_BA_Entities);
               while Present (Scope_BA_Entity) loop

                  if BATN.Kind (Scope_BA_Entity) = BATN.K_For_Cond_Structure
                    or else BATN.Kind (Scope_BA_Entity)
                    = BATN.K_ForAll_Cond_Structure
                  then
                     Success := Success or else
                       Get_Name_String
                         (Utils.To_Lower
                            (BATN.Display_Name
                               (Element_Idt (Scope_BA_Entity))))
                         = Get_Name_String
                       (Utils.To_Lower (BATN.Display_Name
                        (BATN.Identifier (Node))));

                     if Get_Name_String
                       (Utils.To_Lower
                          (BATN.Display_Name (Element_Idt (Scope_BA_Entity))))
                         = Get_Name_String
                       (Utils.To_Lower (BATN.Display_Name
                        (BATN.Identifier (Node))))
                     then
                        For_counter := Element_Idt (Scope_BA_Entity);
                     end if;
                  end if;
                  exit when Success;
                  Scope_BA_Entity := BATN.Next_Node (Scope_BA_Entity);
               end loop;
            end if;

            if not Success then
               Error_Loc (1) := BATN.Loc (BATN.Identifier (Node));
               Error_Name (1) := BATN.Display_Name
                 (BATN.Identifier (Node));
               DE (" The parameter ("
                   & Get_Name_String (Remove_Prefix_From_Name
                     ("%ba%", BATN.Display_Name
                        (BATN.Identifier (Node)))) & ")"
                   & " does not point to "
                   & "anything or point to something unreachable.");

            else
               --  Check if the type of the BA_var or the out_param
               --  (given as parameter) is consistent with corresponding
               --  parameter type in the called subprogram
               --
               if Present (BA_Var) then
                  Success := Get_Name_String
                    (Utils.To_Lower
                       (Remove_Prefix_From_Name
                            ("%ba%",
                             BATN.Name (BATN.Component_Type
                               (BATN.Classifier_Ref (BA_Var))))))
                    = Get_Name_String
                    (Utils.To_Lower (ATN.Name (Parameter_Type)));
               end if;

               if Present (For_counter) then
                  Success := Get_Name_String
                    (Utils.To_Lower
                       (Remove_Prefix_From_Name
                            ("%ba%",
                             BATN.Display_Name (BATN.Component_Type
                               (BATN.Classifier_Ref (Scope_BA_Entity))))))
                    = Get_Name_String
                    (Utils.To_Lower (ATN.Display_Name (Parameter_Type)));
               end if;

               if Present (Out_param) then
                  Success := Get_Name_String
                    (Utils.To_Lower
                       (ATN.Name (ATN.Full_Identifier
                        (ATN.Entity_Ref (Out_param)))))
                    = Get_Name_String
                    (Utils.To_Lower (ATN.Name (Parameter_Type)));
               end if;

               if Present (Requires_data_access) then
                  Success := Get_Name_String
                    (Utils.To_Lower
                       (ATN.Name (ATN.Full_Identifier
                        (ATN.Entity_Ref (Requires_data_access)))))
                    = Get_Name_String
                    (Utils.To_Lower (ATN.Name (Parameter_Type)));
               end if;

               if not Success then
                  Error_Loc (1) := BATN.Loc (BATN.Identifier (Node));
                  Error_Name (1) := BATN.Display_Name
                    (BATN.Identifier (Node));
                  DE (" The type of the parameter label ("
                      & Get_Name_String (Remove_Prefix_From_Name
                        ("%ba%", BATN.Display_Name
                           (BATN.Identifier (Node)))) & ")"
                      & " is not consistent with the corresponding parameter"
                      & " type in the called subprogram declaration.");

               end if;
            end if;
         end if;

      else

         if BATN.Kind (Node) = BATN.K_Property_Constant or else
           BATN.Kind (Node) = BATN.K_Identifier
         then
            if BATN.Kind (Node) = BATN.K_Property_Constant then
               Ident := BATN.Identifier (Node);
            else
               Ident := Node;
            end if;

            --  Check if the current Node refers to a valid
            --  variable in the current BA or a valid parameter
            --  or a valid requires data access of the subprogram
            --  implementing the current BA;
            --  or a feature or Data subcomponent of the
            --  Parent component.

            Success := Present
              (Find_BA_Variable (Ident, BA_Root))
              or else Present
                (Find_Data_Port_Of_Parent_Component
                   (Ident, Root, Parent_Component))
              or else Present
                (Find_Parameter_Of_Parent_Component
                   (Ident, Root, Parent_Component))
              or else Present
                (Find_Requires_Data_Access_Of_Parent_Component
                   (Ident, Root, Parent_Component))
              or else Present
                (Find_Data_Subcomponent_Of_Parent_Component
                   (Ident, Root, Parent_Component));

            if not Is_Empty (Scope_BA_Entities) then
               Scope_BA_Entity := BATN.First_Node (Scope_BA_Entities);
               while Present (Scope_BA_Entity) loop

                  if BATN.Kind (Scope_BA_Entity) = BATN.K_For_Cond_Structure
                    or else BATN.Kind (Scope_BA_Entity)
                    = BATN.K_ForAll_Cond_Structure
                  then
                     Success := Success or else
                       Get_Name_String
                         (Utils.To_Lower
                            (BATN.Display_Name
                               (Element_Idt (Scope_BA_Entity))))
                         = Get_Name_String
                       (Utils.To_Lower (BATN.Display_Name
                        (Ident)));
                  end if;
                  exit when Success;
                  Scope_BA_Entity := BATN.Next_Node (Scope_BA_Entity);
               end loop;
            end if;

            if Present (Find_Out_Parameter_Of_Parent_Component
                        (Ident, Root, Parent_Component))
            then
               N := Find_Out_Parameter_Of_Parent_Component
                 (Ident, Root, Parent_Component);

            elsif Present (Find_Output_Port_Of_Parent_Component
                           (Ident, Root, Parent_Component))
            then
               N := Find_Output_Port_Of_Parent_Component
                 (Ident, Root, Parent_Component);
            elsif Present (Find_Data_Port_Of_Parent_Component
                           (Ident, Root, Parent_Component))
            then
               N := Find_Data_Port_Of_Parent_Component
                 (Ident, Root, Parent_Component);
            elsif  Present (Find_Input_Port_Of_Parent_Component
                            (Ident, Root, Parent_Component))
            then
               N := Find_Input_Port_Of_Parent_Component
                 (Ident, Root, Parent_Component);
            elsif Present (Find_Data_Subcomponent_Of_Parent_Component
                           (Ident, Root, Parent_Component))
            then
               N := Find_Data_Subcomponent_Of_Parent_Component
                 (Ident, Root, Parent_Component);
            end if;

            if Present (N) then
               BATN.Set_Corresponding_Entity (Ident, N);
            end if;

            if not Success then
               --  Indeed, a BA_Value can be an Enumerator
               --  and here we can't retreive its value, that's why
               --  we assign Success with True
               Success := True;
--                 Error_Loc (1) := BATN.Loc (Ident);
--                 Error_Name (1) := BATN.Display_Name
--                   (Ident);
--                 if Is_Parameter_Expr then
--                    DE (" (" & Get_Name_String (Remove_Prefix_From_Name
--                        ("%ba%", BATN.Display_Name
--                           (Ident))) & ")"
--                        & " in the IN parameter value expression"
--                        & " does not point to"
--                        & " anything or point to something unreachable.");
--                 else
--                    Success := True;
--                      DE (" (" & Get_Name_String (Remove_Prefix_From_Name
--                          ("%ba%", BATN.Display_Name
--                             (Ident))) & ")"
--                          & " does not point to"
--                          & " anything or point to something unreachable.");
--                 end if;
            end if;

         elsif BATN.Kind (Node) = BATN.K_Value_Variable then

            Success := Analyze_BA_Value_Variable
              (Node             => Node,
               Root             => Root,
               BA_Root          => BA_Root,
               Parent_Component => Parent_Component);

         elsif BATN.Kind (Node) = BATN.K_Value_Expression then
            Success := Analyze_BA_Value_Expression
              (Node              => Node,
               Root              => Root,
               BA_Root           => BA_Root,
               Parent_Component  => Parent_Component,
               Is_Parameter_Expr => Is_Parameter_Expr,
               Is_Out_Parameter  => Is_Out_Parameter,
               Parameter_Type    => Parameter_Type,
               Scope_BA_Entities => Scope_BA_Entities);
         end if;

      end if;

      return Success;

   end Analyze_Value;

   ----------------------
   -- Find_BA_Variable --
   ----------------------

   function Find_BA_Variable
     (Node             : Node_Id;
      BA_Root          : Node_Id)
      return Node_Id
   is

      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      List_Node         : Node_Id;
      Behavior_Variable : Node_Id;
      BA_Variable       : Node_Id := No_Node;
   begin

      if not Is_Empty (Variables (BA_Root)) then

         Behavior_Variable := BATN.First_Node (Variables (BA_Root));
         while Present (Behavior_Variable) loop
            if not Is_Empty (BATN.Identifiers (Behavior_Variable)) then
               List_Node := BATN.First_Node
                 (BATN.Identifiers (Behavior_Variable));
               while Present (List_Node) loop

                  if Get_Name_String
                    (Utils.To_Lower
                       (Remove_Prefix_From_Name
                            ("%ba%", BATN.Name (Node))))
                        =
                    Get_Name_String
                      (Utils.To_Lower
                         (Remove_Prefix_From_Name
                            ("%ba%", BATN.Name (List_Node))))
                  then
                     BA_Variable := Behavior_Variable;
                  end if;

                  exit when BA_Variable /= No_Node;
                  List_Node := BATN.Next_Node (List_Node);
               end loop;

            end if;

            Behavior_Variable := BATN.Next_Node (Behavior_Variable);
         end loop;

      end if;

      return BA_Variable;

   end Find_BA_Variable;

   ----------------------------------
   -- Get_Component_Type_From_Impl --
   ----------------------------------

   function Get_Component_Type_From_Impl
     (Root            : Node_Id;
      Component_Impl  : Node_Id)
      return Node_Id
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (ATN.Kind (Component_Impl) =
                       ATN.K_Component_Implementation);

      N1                       : Node_Id;
      Component_Type_From_Impl : Node_Id := No_Node;
      List_Node                : Node_Id;
   begin

      if not ANU.Is_Empty (ATN.Declarations (Root))
      then
         List_Node := ATN.First_Node
           (ATN.Declarations (Root));

         while Present (List_Node) loop
            if ATN.Kind (List_Node) = ATN.K_Package_Specification then

               N1 := ATN.First_Node (Declarations (List_Node));
               while Present (N1) loop
                  if (ATN.Kind (N1) = ATN.K_Component_Type)
                    and then
                      Get_Name_String
                        (Utils.To_Lower
                           (ATN.Name (ATN.Identifier (N1)))) =
                    Get_Name_String
                      (Utils.To_Lower
                         (ATN.Name (ATN.Component_Type_Identifier
                          (Component_Impl))))
                  then
                     Component_Type_From_Impl := N1;

                  end if;
                  exit when Component_Type_From_Impl /= No_Node;
                  N1 := ATN.Next_Node (N1);
               end loop;
            end if;

            List_Node := ATN.Next_Node (List_Node);
         end loop;
      end if;

      return Component_Type_From_Impl;

   end Get_Component_Type_From_Impl;

   ----------------------------------------
   -- Find_Parameter_Of_Parent_Component --
   ----------------------------------------

   function Find_Parameter_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id
   is

      use ATN;
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      Parameter                : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;
      result                   : Node_Id := No_Node;
   begin
      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      if Component_Category'Val (Category (Type_Of_Parent_Component))
        = CC_Subprogram
      then

         if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

            Parameter := ATN.First_Node
              (ATN.Features (Type_Of_Parent_Component));

            while Present (Parameter) loop
               if ATN.Kind (Parameter) = K_Parameter
                 and then Get_Name_String
                   (Utils.To_Lower
                      (Remove_Prefix_From_Name
                         ("%ba%", BATN.Name (Node))))
                   = Get_Name_String
                 (Utils.To_Lower (ATN.Name (ATN.Identifier (Parameter))))
               then
                  result := Parameter;
               end if;

               exit when result /= No_Node;

               Parameter := ATN.Next_Node (Parameter);
            end loop;

         end if;
      end if;

      return result;

   end Find_Parameter_Of_Parent_Component;

   --------------------------------------------
   -- Find_Out_Parameter_Of_Parent_Component --
   --------------------------------------------

   function Find_Out_Parameter_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id
   is

      use ATN;
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      Parameter                : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;
      result                   : Node_Id := No_Node;
   begin
      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      if Component_Category'Val (Category (Type_Of_Parent_Component))
        = CC_Subprogram
      then

         if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

            Parameter := ATN.First_Node
              (ATN.Features (Type_Of_Parent_Component));

            while Present (Parameter) loop

               if ((ATN.Kind (Parameter) = K_Parameter
                    and then ATN.Is_Out (Parameter))
                   or else
                     (ATN.Kind (Parameter) = ATN.K_Subcomponent_Access))
                 and then Get_Name_String
                   (Utils.To_Lower
                      (Remove_Prefix_From_Name
                         ("%ba%", BATN.Name (Node))))
                   = Get_Name_String
                 (Utils.To_Lower (ATN.Name (ATN.Identifier (Parameter))))
               then
                  result := Parameter;
               end if;

               exit when result /= No_Node;

               Parameter := ATN.Next_Node (Parameter);
            end loop;
         end if;
      end if;

      return result;
   end Find_Out_Parameter_Of_Parent_Component;

   ---------------------------------------------------
   -- Find_Requires_Data_Access_Of_Parent_Component --
   ---------------------------------------------------

   function Find_Requires_Data_Access_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id
   is

      use ATN;
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      F                        : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;
      result                   : Node_Id := No_Node;
      --  Access_property_association : Node_Id;
   begin
      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      --  Here, we make the analyze on the declarative model
      --  But it must be made on the instance model to be able
      --  to use Get_Required_Data_Access (Corresponding_Instance (F))
      --  and check if it is Access_Read_Only, Access_Write_Only
      --  Access_Read_Write or Access_None.

      if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

         F := ATN.First_Node (ATN.Features (Type_Of_Parent_Component));

         while Present (F) loop

            if (ATN.Kind (F) = ATN.K_Subcomponent_Access) then

               --  if not ANU.Is_Empty (ATN.Properties (F)) then
               --     Access_property_association := ATN.First_Node
               --       (ATN.Properties (F));
               --     Put_Line (ATN.Kind (Property_Association_Value
               --               (Access_property_association))'Img);
               --     Put_Line ("Is_Access = "
               --           & ATN.Is_Access (Access_property_association)'Img);
               --  end if;

               if Component_Category'Val (Subcomponent_Category (F)) = CC_Data
                 and then not ATN.Is_Provided (F)
                 and then Get_Name_String
                   (Utils.To_Lower
                      (Remove_Prefix_From_Name
                         ("%ba%", BATN.Name (Node))))
                   = Get_Name_String
                 (Utils.To_Lower (ATN.Name (ATN.Identifier (F))))
               then
                  result := F;
               end if;
            end if;

            exit when result /= No_Node;

            F := ATN.Next_Node (F);
         end loop;
      end if;

      return result;
   end Find_Requires_Data_Access_Of_Parent_Component;

   ------------------------------------------
   -- Find_Output_Port_Of_Parent_Component --
   ------------------------------------------

   function Find_Output_Port_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      F                        : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;
      result                   : Node_Id := No_Node;

   begin

      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

         F := ATN.First_Node (ATN.Features (Type_Of_Parent_Component));

         while Present (F) loop

            if ATN.Kind (F) = K_Port_Spec
              and then ATN.Is_Out (F) and then
              Get_Name_String
                (Utils.To_Lower
                   (Remove_Prefix_From_Name
                      ("%ba%", BATN.Name (Node))))
                = Get_Name_String
              (Utils.To_Lower (ATN.Name (ATN.Identifier (F))))
            then
               result := F;
            end if;

            exit when result /= No_Node;

            F := ATN.Next_Node (F);
         end loop;
      end if;

      return result;

   end Find_Output_Port_Of_Parent_Component;

   -----------------------------------------
   -- Find_Input_Port_Of_Parent_Component --
   -----------------------------------------

   function Find_Input_Port_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      F                        : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;
      result                   : Node_Id := No_Node;

   begin

      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

         F := ATN.First_Node (ATN.Features (Type_Of_Parent_Component));

         while Present (F) loop

            if ATN.Kind (F) = K_Port_Spec
              and then ATN.Is_In (F) and then
              Get_Name_String
                (Utils.To_Lower
                   (Remove_Prefix_From_Name
                      ("%ba%", BATN.Name (Node))))
                = Get_Name_String
              (Utils.To_Lower (ATN.Name (ATN.Identifier (F))))
            then
               result := F;
            end if;

            exit when result /= No_Node;

            F := ATN.Next_Node (F);
         end loop;
      end if;

      return result;

   end Find_Input_Port_Of_Parent_Component;

   ----------------------------------------
   -- Find_and_Link_Event_Port_Of_Parent_Component --
   ----------------------------------------

   function Find_and_Link_Event_Port_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (Kind (BATN.Identifier (Node)) = BATN.K_Name);

      F                        : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;
      result                   : Node_Id := No_Node;

   begin

      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

         F := ATN.First_Node (ATN.Features (Type_Of_Parent_Component));

         while Present (F) loop

            if ATN.Kind (F) = K_Port_Spec
              and then Is_Event (F) and then
              Get_Name_String
                (Utils.To_Lower
                   (Remove_Prefix_From_Name
                      ("%ba%",
                       BATN.Display_Name
                         (BATN.First_Node
                              (BATN.Idt (BATN.Identifier (Node)))))))
                = Get_Name_String
              (Utils.To_Lower (ATN.Name (ATN.Identifier (F))))
            then
               result := F;
               BATN.Set_Corresponding_Entity
                 (BATN.First_Node
                    (BATN.Idt (BATN.Identifier (Node))), F);
            end if;

            exit when result /= No_Node;
            F := ATN.Next_Node (F);
         end loop;
      end if;

      return result;

   end Find_and_Link_Event_Port_Of_Parent_Component;

   ----------------------------------------
   -- Find_Data_Port_Of_Parent_Component --
   ----------------------------------------

   function Find_Data_Port_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      F                        : Node_Id;
      Type_Of_Parent_Component : Node_Id := No_Node;
      result                   : Node_Id := No_Node;

   begin

      if ATN.Kind (Parent_Component) = ATN.K_Component_Type then
         Type_Of_Parent_Component := Parent_Component;
      else
         Type_Of_Parent_Component := Get_Component_Type_From_Impl
           (Root, Parent_Component);
      end if;

      if not ANU.Is_Empty (ATN.Features (Type_Of_Parent_Component)) then

         F := ATN.First_Node (ATN.Features (Type_Of_Parent_Component));

         while Present (F) loop

            if ATN.Kind (F) = K_Port_Spec
              and then Is_Data (F) and then
              Get_Name_String
                (Utils.To_Lower
                   (Remove_Prefix_From_Name
                      ("%ba%", BATN.Name (Node))))
                = Get_Name_String
              (Utils.To_Lower (ATN.Name (ATN.Identifier (F))))
            then
               result := F;
            end if;

            exit when result /= No_Node;

            F := ATN.Next_Node (F);
         end loop;
      end if;

      return result;

   end Find_Data_Port_Of_Parent_Component;

   ------------------------------------------------
   -- Find_Data_Subcomponent_Of_Parent_Component --
   ------------------------------------------------

   function Find_Data_Subcomponent_Of_Parent_Component
     (Node              : Node_Id;
      Root              : Node_Id;
      Parent_Component  : Node_Id)
      return Node_Id
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else ATN.Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      List_Of_Subcomponents : List_Id;
      N1                    : Node_Id;
      result                : Node_Id := No_Node;
   begin
      if ATN.Kind (Parent_Component) = ATN.K_Component_Implementation then

         List_Of_Subcomponents := ATN.Subcomponents (Parent_Component);

         if not ANU.Is_Empty (List_Of_Subcomponents) then

            N1 := ATN.First_Node (List_Of_Subcomponents);
            while Present (N1) loop
               if  (ATN.Kind (N1) = ATN.k_Subcomponent and then
                    Component_Category'Val (Category (N1)) = CC_Data)
                 and then
                   Get_Name_String
                     (Utils.To_Lower
                        (Remove_Prefix_From_Name
                           ("%ba%", BATN.Name (Node))))
                   = Get_Name_String
                 (Utils.To_Lower (ATN.Name (ATN.Identifier (N1))))
               then
                  result := N1;
               end if;
               exit when result /= No_Node;
               N1 := ATN.Next_Node (N1);
            end loop;
         end if;
      end if;

      return result;
   end Find_Data_Subcomponent_Of_Parent_Component;

   --------------------------
   -- Analyze_Timed_Action --
   --------------------------

   function Analyze_Timed_Action
     (Node              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);
      pragma Assert (BATN.Kind (Node) = BATN.K_Timed_Act);

   begin
      return True;
   end Analyze_Timed_Action;

   --------------------------------
   -- Analyze_Dispatch_Condition --
   --------------------------------

   function Analyze_Dispatch_Condition
     (Condition         : Node_Id;
      Root              : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);
      pragma Assert (BATN.Kind (BA_Root) = BATN.K_Behavior_Annex);
      pragma Assert (ATN.Kind (Parent_Component) = ATN.K_Component_Type
                     or else Kind (Parent_Component) =
                       ATN.K_Component_Implementation);

      Success   : Boolean := True;
      Frozen_Port : Node_Id;
      Pointed_Node : Node_Id;
      Dispatch_Conjunction_Node : Node_Id;
      Dispatch_Trigger_Event : Node_Id;

   begin
      --  The incoming port identifiers and subprogram access feature
      --  identifiers that represent dispatch trigger events must
      --  refer to the respective feature in the component type to which
      --  the behavior annex subclause is associated

      if not Is_Empty (Dispatch_Conjunction
                       (Dispatch_Trigger_Condition (Condition)))
      then
         Dispatch_Conjunction_Node :=
           BATN.First_Node
             (Dispatch_Conjunction
                (Dispatch_Trigger_Condition (Condition)));
         while Present (Dispatch_Conjunction_Node) loop
            Dispatch_Trigger_Event :=
              BATN.First_Node
                (Dispatch_Triggers
                   (Dispatch_Conjunction_Node));
            while Present (Dispatch_Trigger_Event) loop
               Pointed_Node := Link_Dispatch_Trigger_Event
                 (Dispatch_Trigger_Event, Root, Parent_Component);
               if Present (Pointed_Node) then
                  if ATN.Kind (Pointed_Node) = K_Port_Spec
                  then
                     if not Is_Event (Pointed_Node) or else
                       not ATN.Is_In (Pointed_Node)
                     then
                        Error_Loc (1) := BATN.Loc (Dispatch_Trigger_Event);

                        Error_Name (1) := BATN.Display_Name
                          (Dispatch_Trigger_Event);
                        DE ("#(dispatch trigger) must refer to an IN event" &
                              " port or IN event data port");

                        Success := False;
                     end if;
                  else
                     if Component_Category'Val
                       (Subcomponent_Category (Pointed_Node)) /= CC_Subprogram
                       or else not Is_Provided (Pointed_Node)
                     then
                        Error_Loc (1) := BATN.Loc (Dispatch_Trigger_Event);

                        Error_Name (1) := BATN.Display_Name
                          (Dispatch_Trigger_Event);
                        DE ("#(dispatch trigger) must refer to a provides" &
                              " subprogram access feature");

                        Success := False;
                     end if;
                  end if;

                  if Success then
                     BATN.Set_Corresponding_Entity
                       (Dispatch_Trigger_Event, Pointed_Node);
                  end if;
               else
                  Error_Loc (1) := BATN.Loc (Dispatch_Trigger_Event);

                  Error_Name (1) := BATN.Display_Name
                    (Dispatch_Trigger_Event);
                  DE ("#(dispatch trigger) does not point to any event" &
                        " port or subprogram access feature");

                  Success := False;
               end if;
               Dispatch_Trigger_Event := BATN.Next_Node
                 (Dispatch_Trigger_Event);
            end loop;
            Dispatch_Conjunction_Node := BATN.Next_Node
              (Dispatch_Conjunction_Node);
         end loop;

      end if;
      --  The incoming port identifier in the frozen port list must refer
      --  to incoming ports in the component type to which the
      --  behavior annex subclause is associated

      if not Is_Empty (Frozen_Ports (Condition))
      then
         Frozen_Port := BATN.First_Node (Frozen_Ports (Condition));
         while Present (Frozen_Port) loop
            Pointed_Node := Link_Frozen_Port
              (Frozen_Port, Root, Parent_Component);
            if Present (Pointed_Node) then
               if not ATN.Is_In (Pointed_Node)
               then
                  Error_Loc (1) := BATN.Loc (Frozen_Port);

                  Error_Name (1) := BATN.Display_Name (Frozen_Port);
                  DE ("#(frozen port) must refer to an IN port");

                  Success := False;
               end if;
            else
               Error_Loc (1) := BATN.Loc (Frozen_Port);

               Error_Name (1) := BATN.Display_Name (Frozen_Port);
               DE ("#(frozen port) does not point to any port");

               Success := False;
            end if;
            Frozen_Port := BATN.Next_Node (Frozen_Port);
         end loop;
      end if;
      return Success;
   end Analyze_Dispatch_Condition;

   ----------------
   -- Find_State --
   ----------------

   function Find_State
     (State_Id : Node_Id;
      BA_Root  : Node_Id)
      return Node_Id
   is
      List_Of_States : Node_List;
      N1 : Node_Id;
      Node : Node_Id := No_Node;
   begin
      Select_All_States (BA_Root,
                         List_Of_States.First,
                         List_Of_States.Last);
      N1 := List_Of_States.First;
      while Present (N1) loop
         if Get_Name_String (BATN.Name (State_Id)) =
           Get_Name_String (BATN.Name (BATN.Item (N1)))
         then
            Node := Item (N1);
         end if;
         N1 := Next_Node (N1);
      end loop;
      return Node;
   end Find_State;

   ----------------------
   -- Link_Frozen_Port --
   ----------------------

   function Link_Frozen_Port
     (Frozen_Port : Node_Id;
      Root        : Node_Id;
      Component   : Node_Id)
      return Node_Id
   is
      use ATN;
      List_Of_Features  : List_Id;
      N1                : Node_Id;
      Node              : Node_Id := No_Node;
      Type_Of_Component : Node_Id;
   begin

      if ATN.Kind (Component) = ATN.K_Component_Type then
         Type_Of_Component := Component;
      else
         Type_Of_Component := Get_Component_Type_From_Impl
           (Root, Component);
      end if;

      List_Of_Features := ATN.Features (Type_Of_Component);

      if not ANU.Is_Empty (List_Of_Features) then

         N1 := ATN.First_Node (List_Of_Features);
         while Present (N1) loop
            if (ATN.Kind (N1) = ATN.K_Port_Spec) then
               if Get_Name_String
                 (Remove_Prefix_From_Name
                    ("%ba%", BATN.Name (Frozen_Port))) =
                   Get_Name_String (ATN.Name (ATN.Identifier (N1)))
               then
                  Node := N1;
               end if;
            end if;
            exit when Node /= No_Node;
            N1 := ATN.Next_Node (N1);
         end loop;
      end if;

      return Node;

   end Link_Frozen_Port;

   ---------------------------------
   -- Link_Dispatch_Trigger_Event --
   ---------------------------------

   function Link_Dispatch_Trigger_Event
     (Dispatch_Trigger_Event : Node_Id;
      Root                   : Node_Id;
      Component              : Node_Id)
      return Node_Id
   is
      use ATN;
      List_Of_Features  : List_Id;
      N1                : Node_Id;
      Node              : Node_Id := No_Node;
      Type_Of_Component : Node_Id;
   begin

      if ATN.Kind (Component) = ATN.K_Component_Type then
         Type_Of_Component := Component;
      else
         Type_Of_Component := Get_Component_Type_From_Impl
           (Root, Component);
      end if;

      List_Of_Features := ATN.Features (Type_Of_Component);

      if not ANU.Is_Empty (List_Of_Features) then

         N1 := ATN.First_Node (List_Of_Features);
         while Present (N1) loop
            if (ATN.Kind (N1) = ATN.K_Port_Spec or else
                 ATN.Kind (N1) = ATN.K_Subcomponent_Access)
            then
               if Get_Name_String
                 (Remove_Prefix_From_Name
                    ("%ba%", BATN.Name (Dispatch_Trigger_Event))) =
                   Get_Name_String (ATN.Name (ATN.Identifier (N1)))
               then
                  Node := N1;
               end if;
            end if;
            exit when Node /= No_Node;
            N1 := ATN.Next_Node (N1);
         end loop;
      end if;

      return Node;

   end Link_Dispatch_Trigger_Event;

   -------------------------
   -- Enter_Name_In_Table --
   -------------------------

   function Enter_Name_In_Table (Identifier : Node_Id)
                                 return Boolean
   is

      pragma Assert (BATN.Kind (Identifier) = BATN.K_Identifier);
      Success   : Boolean := True;
      Info : Nat;
   begin
      Info := Get_Name_Table_Info (BATN.Name (Identifier));
      if Info /= 0 then
         Error_Loc (1) := Loc (Identifier);
         Error_Loc (2) := Loc (Node_Id (Info));

         Error_Name (1) := Display_Name (Identifier);
         DE ("#conflicts with declaration!");

         Success := False;
      else
         Set_Name_Table_Info (Name (Identifier), Nat (Identifier));
      end if;
      return Success;
   end Enter_Name_In_Table;

   procedure Remove_All_Identifiers_From_Table
     (BA_Root          : Node_Id)
   is

      List_Node : Node_Id;
      Node : Node_Id;
      Behavior_Variable : Node_Id;
      Behavior_State : Node_Id;
      Behavior_Transition : Node_Id;
   begin
      if not Is_Empty (Variables (BA_Root)) then
         Behavior_Variable := BATN.First_Node (Variables (BA_Root));
         while Present (Behavior_Variable) loop
            List_Node := BATN.First_Node
              (BATN.Identifiers (Behavior_Variable));

            while Present (List_Node) loop
               Set_Name_Table_Info (Name (List_Node), 0);
               List_Node := BATN.Next_Node (List_Node);
            end loop;
            Behavior_Variable := BATN.Next_Node (Behavior_Variable);
         end loop;
      end if;

      if not Is_Empty (States (BA_Root)) then
         Behavior_State := BATN.First_Node (States (BA_Root));
         while Present (Behavior_State) loop
            List_Node := BATN.First_Node
              (BATN.Identifiers (Behavior_State));

            while Present (List_Node) loop
               Set_Name_Table_Info (Name (List_Node), 0);
               List_Node := BATN.Next_Node (List_Node);
            end loop;
            Behavior_State := BATN.Next_Node (Behavior_State);
         end loop;
      end if;

      if not Is_Empty (Transitions (BA_Root)) then
         Behavior_Transition := BATN.First_Node (Transitions (BA_Root));
         while Present (Behavior_Transition) loop
            if Present (Behavior_Transition_Idt
                        (Transition (Behavior_Transition)))
            then
               Node := Behavior_Transition_Idt
                 (Transition (Behavior_Transition));
               Set_Name_Table_Info (Name (Node), 0);
            end if;
            Behavior_Transition := BATN.Next_Node (Behavior_Transition);
         end loop;
      end if;

   end Remove_All_Identifiers_From_Table;

   -----------------------------------------------
   -- Test_With_Identifiers_In_Parent_Component --
   -----------------------------------------------

   function Test_With_Identifiers_In_Parent_Component
     (Id         : Node_Id;
      Root       : Node_Id;
      Component  : Node_Id)
      return Boolean
   is

      use ATN;

      A_List            : List_Id;
      N1                : Node_Id;
      Success           : Boolean := True;
      Type_Of_Component : Node_Id;
   begin

      if ATN.Kind (Component) = ATN.K_Component_Type then
         Type_Of_Component := Component;
      else
         Type_Of_Component := Get_Component_Type_From_Impl
           (Root, Component);
      end if;

      if ATN.Kind (Component) = ATN.K_Component_Implementation then

         A_List := ATN.Subcomponents (Component);

         if not ANU.Is_Empty (A_List) then

            N1 := ATN.First_Node (A_List);
            while Present (N1) loop
               if  (ATN.Kind (N1) = ATN.k_Subcomponent and then
                    Component_Category'Val (Category (N1)) = CC_Data)
                 and then
                   Get_Name_String (Remove_Prefix_From_Name
                                    ("%ba%", BATN.Name (Id))) =
                 Get_Name_String (ATN.Name (ATN.Identifier (N1)))
               then
                  Error_Loc (1) := BATN.Loc (Id);
                  Error_Loc (2) := ATN.Loc (N1);

                  Error_Name (1) := BATN.Display_Name (Id);
                  DE ("#conflicts with declaration!");
                  Success := False;
                  exit;
               end if;
               N1 := ATN.Next_Node (N1);
            end loop;
         end if;
      end if;

      A_List := ATN.Features (Type_Of_Component);

      if not ANU.Is_Empty (A_List) then

         N1 := ATN.First_Node (A_List);
         while Present (N1) loop
            if (ATN.Kind (N1) = ATN.k_Port_Spec
                or else ATN.Kind (N1) = ATN.k_Parameter
                or else ATN.Kind (N1) = ATN.k_Feature_Group_Spec
                or else ATN.Kind (N1) = ATN.k_Subcomponent_Access)
              and then
                Get_Name_String (Remove_Prefix_From_Name
                                 ("%ba%", BATN.Name (Id))) =
              Get_Name_String (ATN.Name (ATN.Identifier (N1)))
            then
               Error_Loc (1) := BATN.Loc (Id);
               Error_Loc (2) := ATN.Loc (N1);

               Error_Name (1) := BATN.Display_Name (Id);
               DE ("#conflicts with declaration!");
               Success := False;
               exit;
            end if;
            N1 := ATN.Next_Node (N1);
         end loop;
      end if;

      A_List := ATN.Modes (Component);

      if not ANU.Is_Empty (A_List) then

         N1 := ATN.First_Node (A_List);
         while Present (N1) loop
            if (ATN.Kind (N1) = ATN.K_Mode)
              and then
                Get_Name_String (Remove_Prefix_From_Name
                                 ("%ba%", BATN.Name (Id))) =
              Get_Name_String (ATN.Name (ATN.Identifier (N1)))
            then
               Error_Loc (1) := BATN.Loc (Id);
               Error_Loc (2) := ATN.Loc (N1);

               Error_Name (1) := BATN.Display_Name (Id);
               DE ("#conflicts with declaration!");
               Success := False;
               exit;
            end if;
            N1 := ATN.Next_Node (N1);
         end loop;
      end if;

      return Success;
   end Test_With_Identifiers_In_Parent_Component;

   ------------
   -- Length --
   ------------

   function Length (L : Node_List) return Natural is
      N : Node_Id;
      C : Natural := 0;
   begin
      if Present (L.First) then
         N := L.First;
         while Present (N) loop
            C := C + 1;
            N := Next_Node (N);
         end loop;
      end if;

      return C;
   end Length;

   -----------------------
   -- Select_All_States --
   -----------------------

   procedure Select_All_States
     (BA_Root         : Node_Id;
      List_First_Node : in out Node_Id;
      List_Last_Node  : in out Node_Id)

   is
      Behavior_State : Node_Id;
      State_Node     : Node_Id;
      Node           : Node_Id;
   begin

      Behavior_State := First_Node (States (BA_Root));
      while Present (Behavior_State) loop
         if not Is_Empty (Identifiers (Behavior_State)) then
            State_Node := First_Node
              (Identifiers (Behavior_State));
            while Present (State_Node) loop
               Node := Make_Node_Container (State_Node);
               if No (List_First_Node) then
                  List_First_Node := Node;
                  List_Last_Node := Node;
               else
                  Set_Next_Node (List_Last_Node, Node);
                  Set_Next_Node (Node, No_Node);
                  List_Last_Node := Node;
               end if;
               State_Node := Next_Node (State_Node);
            end loop;
         end if;
         Behavior_State := Next_Node (Behavior_State);
      end loop;
   end Select_All_States;

   -------------------
   -- Select_States --
   -------------------

   procedure Select_States
     (BA_Root         : Node_Id;
      Kinds           : Behavior_State_Kind_Array;
      List_First_Node : in out Node_Id;
      List_Last_Node  : in out Node_Id)

   is
      Success        : Boolean;
      Behavior_State : Node_Id;
      State_Node     : Node_Id;
      Node           : Node_Id;
   begin

      Behavior_State := First_Node (States (BA_Root));
      while Present (Behavior_State) loop
         if not Is_Empty (Identifiers (Behavior_State)) then
            State_Node := First_Node
              (Identifiers (Behavior_State));
            while Present (State_Node) loop
               Node := Make_Node_Container (State_Node);
               Success := False;

               for K in Kinds'Range loop
                  Success := Success
                    or else (Behavior_State_Kind'Val
                             (State_Kind (Behavior_State)) = Kinds (K));
               end loop;

               if Success then
                  if No (List_First_Node) then
                     List_First_Node := Node;
                     List_Last_Node := Node;
                  else
                     Set_Next_Node (List_Last_Node, Node);
                     Set_Next_Node (Node, No_Node);
                     List_Last_Node := Node;
                  end if;
               end if;
               State_Node := Next_Node (State_Node);
            end loop;
         end if;
         Behavior_State := Next_Node (Behavior_State);
      end loop;
   end Select_States;

   -----------------
   -- Is_Complete --
   -----------------

   function Is_Complete
     (BA_Root : Node_Id;
      State   : Node_Id)
      return boolean
   is
      List_Of_Complete_States : Node_List;
      N1 : Node_Id;
      Success   : Boolean := False;
   begin
      Select_States (BA_Root, (BSK_Complete,
                     BSK_Initial_Complete,
                     BSK_Initial_Complete_Final,
                     BSK_Complete_Final),
                     List_Of_Complete_States.First,
                     List_Of_Complete_States.Last);
      N1 := List_Of_Complete_States.First;
      while Present (N1) loop
         if Get_Name_String (BATN.Name (State)) =
           Get_Name_String (BATN.Name (BATN.Item (N1)))
         then
            Success := True;
         end if;
         exit when Success;
         N1 := Next_Node (N1);
      end loop;
      return Success;
   end Is_Complete;

   ----------------
   -- Is_Initial --
   ----------------

   function Is_Initial
     (BA_Root : Node_Id;
      State   : Node_Id)
      return boolean
   is
      List_Of_Initial_States : Node_List;
      N1 : Node_Id;
      Success   : Boolean := False;
   begin
      Select_States (BA_Root, (BSK_Initial,
                     BSK_Initial_Complete,
                     BSK_Initial_Complete_Final,
                     BSK_Initial_Final),
                     List_Of_Initial_States.First,
                     List_Of_Initial_States.Last);
      N1 := List_Of_Initial_States.First;
      while Present (N1) loop
         if Get_Name_String (BATN.Name (State)) =
           Get_Name_String (BATN.Name (BATN.Item (N1)))
         then
            Success := True;
         end if;
         exit when Success;
         N1 := Next_Node (N1);
      end loop;
      return Success;
   end Is_Initial;

   ----------------
   -- Is_Final --
   ----------------

   function Is_Final
     (BA_Root : Node_Id;
      State   : Node_Id)
      return boolean
   is
      List_Of_Final_States : Node_List;
      N1 : Node_Id;
      Success   : Boolean := False;
   begin
      Select_States (BA_Root, (BSK_Final,
                     BSK_Complete_Final,
                     BSK_Initial_Complete_Final,
                     BSK_Initial_Final),
                     List_Of_Final_States.First,
                     List_Of_Final_States.Last);
      N1 := List_Of_Final_States.First;
      while Present (N1) loop
         if Get_Name_String (BATN.Name (State)) =
           Get_Name_String (BATN.Name (BATN.Item (N1)))
         then
            Success := True;
         end if;
         exit when Success;
         N1 := Next_Node (N1);
      end loop;
      return Success;
   end Is_Final;

   --------------------
   -- Exist_In_Modes --
   --------------------

   function Exist_In_Modes
     (Component : Node_Id;
      State     : Node_Id)
      return boolean
   is
      List_Of_Modes : List_Id;
      N1            : Node_Id;
      Success       : Boolean := False;
   begin

      List_Of_Modes := ATN.Modes (Component);

      if not ANU.Is_Empty (List_Of_Modes) then

         N1 := ATN.First_Node (List_Of_Modes);
         while Present (N1) loop
            if Get_Name_String (Remove_Prefix_From_Name
                                ("%ba%", BATN.Name (State))) =
              Get_Name_String (ATN.Name (ATN.Identifier (N1)))
            then
               Success := True;
            end if;
            exit when Success;
            N1 := ATN.Next_Node (N1);
         end loop;
      end if;

      return Success;

   end Exist_In_Modes;

   -----------------------------
   -- Build_Full_Package_Name --
   -----------------------------

   function Build_Full_Package_Name
     (Package_Name  : List_Id)
      return Name_Id
   is
      N : Node_Id;

   begin
      N := BATN.First_Node (Package_Name);
      Set_Str_To_Name_Buffer ("");
      while Present (N) loop
         Get_Name_String_And_Append (Display_Name (N));

         N := Next_Node (N);
         if Present (N) then
            Add_Str_To_Name_Buffer ("::");
         end if;

      end loop;

      return Name_Find;
   end Build_Full_Package_Name;

   -------------------
   -- Link_Variable --
   -------------------

   function Link_Variable
     (Root             : Node_Id;
      Node             : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);

      Success                 : Boolean := False;
      N1                      : Node_Id := No_Node;
      Full_Package_Name       : Name_Id;
      Corresponding_Package   : Node_Id;
      Corresponding_Component : Node_Id;
      List_Node               : Node_Id;
   begin

      if not Is_Empty (Package_Name (BATN.Classifier_Ref (Node)))
      then
         Full_Package_Name := Build_Full_Package_Name
           (Package_Name (BATN.Classifier_Ref (Node)));

         Success := False;

         if not ANU.Is_Empty (ATN.Declarations (Root))
         then
            List_Node := ATN.First_Node (ATN.Declarations (Root));

            while Present (List_Node) loop
               if ATN.Kind (List_Node) = ATN.K_Package_Specification then
                  if Get_Name_String
                    (Utils.To_Lower (Full_Package_Name))
                      = Get_Name_String
                    (Utils.To_Lower (ATN.Name (ATN.Identifier (List_Node))))
                  then
                     Success := True;
                     Corresponding_Package := List_Node;
                  end if;
               end if;
               exit when Success;
               List_Node := ATN.Next_Node (List_Node);
            end loop;

            List_Node := ATN.First_Node (ATN.Declarations (Root));
            if Present (List_Node) and then
              ATN.Kind (List_Node) = ATN.K_Package_Specification
            then
               N1 := List_Node;
            end if;

         end if;

         if not Success then

            Error_Loc (1) := BATN.Loc (BATN.First_Node
                                       (BATN.Package_Name
                                          (BATN.Classifier_Ref (Node))));
            if Present (N1) then
               DE ("(" &  Get_Name_String (Full_Package_Name) & ")"
                & " name not found in 'with' statements of "
                & Get_Name_String (ATN.Display_Name (N1))
                   & " (package specification)");
            else
               DE ("(" &  Get_Name_String (Full_Package_Name) & ")"
                & " name not found in 'with' statements.");
            end if;
         end if;

         if Present (BATN.Component_Type (BATN.Classifier_Ref (Node)))
         then

            Corresponding_Component := Find_Component_Classifier_By_Name
              (Root                 => Root,
               Package_Identifier   => ATN.Identifier (Corresponding_Package),
               Component_Name => Utils.To_Lower (BATN.Display_Name
                 (BATN.Component_Type (BATN.Classifier_Ref (Node)))));

            Set_Corresponding_Declaration
              (BATN.Classifier_Ref (Node), Corresponding_Component);
         elsif Present (BATN.Component_Impl (BATN.Classifier_Ref (Node))) then

            Corresponding_Component := Find_Component_Classifier_By_Name
              (Root                 => Root,
               Package_Identifier   => ATN.Identifier (Corresponding_Package),
               Component_Name => Utils.To_Lower (BATN.Display_Name
                 (BATN.Component_Impl (BATN.Classifier_Ref (Node)))));

            Set_Corresponding_Declaration
              (BATN.Classifier_Ref (Node), Corresponding_Component);
         end if;
      else
         if Present (BATN.Component_Impl (BATN.Classifier_Ref (Node)))
         then

            if not ANU.Is_Empty (ATN.Declarations (Root)) then
               List_Node := ATN.First_Node (ATN.Declarations (Root));

               while Present (List_Node) loop
                  if ATN.Kind (List_Node) = ATN.K_Package_Specification then

                     N1 := ATN.First_Node (Declarations (List_Node));
                     while Present (N1) loop
                        if (ATN.Kind (N1) = ATN.K_Component_Implementation)
                        then

                           if Component_Category'Val (Category (N1)) = CC_Data
                           then

                              if Get_Name_String
                                (Remove_Prefix_From_Name
                                   ("%ba%", BATN.Name (BATN.Component_Type
                                    (BATN.Classifier_Ref (Node)))))
                                & "."
                                & Get_Name_String
                                (Remove_Prefix_From_Name
                                   ("%ba%", BATN.Name (BATN.Component_Impl
                                    (BATN.Classifier_Ref (Node)))))
                                = Get_Name_String
                                (ATN.Name (ATN.Identifier (N1)))
                              then
                                 Set_Corresponding_Declaration
                                   (BATN.Classifier_Ref (Node), N1);
                                 Success := True;
                              end if;
                           end if;
                        end if;
                        exit when Success;
                        N1 := ATN.Next_Node (N1);
                     end loop;
                  end if;

                  List_Node := ATN.Next_Node (List_Node);
               end loop;
            end if;

            if not Success then
               Error_Loc (1) := BATN.Loc (BATN.Component_Type
                                          (BATN.Classifier_Ref (Node)));
               DE ("(" & Get_Name_String (Remove_Prefix_From_Name
                   ("%ba%", BATN.Name (BATN.Component_Type
                      (BATN.Classifier_Ref (Node)))))
                   & "." & Get_Name_String (Remove_Prefix_From_Name
                     ("%ba%", BATN.Name (BATN.Component_Impl
                        (BATN.Classifier_Ref (Node))))) & ")"
                   & " does not point to "
                   & "anything or to something unreachable.");
            end if;
         else
            if not ANU.Is_Empty (ATN.Declarations (Root)) then
               List_Node := ATN.First_Node (ATN.Declarations (Root));

               while Present (List_Node) loop
                  if ATN.Kind (List_Node) = ATN.K_Package_Specification then

                     N1 := ATN.First_Node (Declarations (List_Node));
                     while Present (N1) loop
                        if (ATN.Kind (N1) = ATN.K_Component_Type)
                        then

                           if Component_Category'Val (Category (N1)) = CC_Data
                           then

                              if Get_Name_String
                                (Remove_Prefix_From_Name
                                   ("%ba%", BATN.Name (BATN.Component_Type
                                    (BATN.Classifier_Ref (Node)))))
                                = Get_Name_String
                                (ATN.Name (ATN.Identifier (N1)))
                              then
                                 Set_Corresponding_Declaration
                                   (BATN.Classifier_Ref (Node), N1);
                                 Success := True;
                              end if;
                           end if;
                        end if;
                        exit when Success;
                        N1 := ATN.Next_Node (N1);
                     end loop;
                  end if;

                  List_Node := ATN.Next_Node (List_Node);
               end loop;
            end if;

            if not Success then
               Error_Loc (1) := BATN.Loc (BATN.Component_Type
                                          (BATN.Classifier_Ref (Node)));
               Error_Name (1) := BATN.Display_Name
                 (BATN.Component_Type (BATN.Classifier_Ref (Node)));
               DE ("(" & Get_Name_String (Remove_Prefix_From_Name
                   ("%ba%", BATN.Name (BATN.Component_Type
                      (BATN.Classifier_Ref (Node))))) & ")"
                   & " does not point to "
                   & "anything or to something unreachable.");
            end if;
         end if;
      end if;
      return Success;
   end Link_Variable;

end Ocarina.Analyzer.AADL_BA;
