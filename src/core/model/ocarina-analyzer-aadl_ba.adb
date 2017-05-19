------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . A N A L Y Z E R . A A D L _ B A              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
     return Boolean;

   function Analyze_Dispatch_Condition
     (Condition         : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
     return Boolean;

   function Enter_Name_In_Table (Identifier : Node_Id)
     return boolean;

   procedure Remove_All_Identifiers_From_Table
     (BA_Root          : Node_Id);

   function Test_With_Identifiers_In_Parent_Component
     (Id         : Node_Id;
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
      Component  : Node_Id)
     return Node_Id;

   function Link_Dispatch_Trigger_Event
     (Dispatch_Trigger_Event : Node_Id;
      Component              : Node_Id)
     return Node_Id;

   function Length (L : Node_List) return Natural;

   --  procedure Affiche (First_Node_List : Node_Id);

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
      use BAT;

      pragma Assert (ATN.Kind (Root) = ATN.K_AADL_Specification);

      Success : Boolean := True;
      L1 : Node_List;
      L2 : Node_List;
      N1 : Node_Id;
      N2 : Node_Id;
      BA_Root : Node_Id;
      Parent_Component : Node_Id;

   begin
   --  chercher les noeuds racines des annexes comportementales
   --  à partir de la racine AADL
      L1 := Find_All_Declarations (Root,
                                   (ATN.K_Component_Type,
                                    ATN.K_Component_Implementation));
      N1 := L1.First;
      while Present (N1) loop
         L2 := Find_All_Subclauses (N1, (1 => ATN.K_Annex_Subclause));

         N2 := L2.First;
         while Present (N2) loop
            if Get_Name_String
              (Utils.To_Lower
               (ATN.Name (ATN.Identifier (N2)))) =
              BAT.Language and then
              Present (ATN.Corresponding_Annex (N2))
            then
               BA_Root := ATN.Corresponding_Annex (N2);
               Parent_Component := Container_Component (N2);
               --  pour chaque annexe trouvée,
               --   on effectue l'analyse sémantique
               Success := Success and then
                 Analyze_Behavior_Specification
                          (Root, BA_Root, Parent_Component);
            end if;
            N2 := ATN.Next_Entity (N2);
         end loop;

         N1 := ATN.Next_Entity (N1);
      end loop;

      return Success;
   end Analyze_Model;

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
                --  vérifier l'unicité des identifiants des variables
                --  dans le composant parent (où l'annexe
                --  comportementale est définie)
                Test_With_Identifiers_In_Parent_Component
                   (List_Node, Parent_Component);
               List_Node := BATN.Next_Node (List_Node);
            end loop;
         end if;

         if Present (BATN.Classifier_Ref (Behavior_Variable)) then
            Success := Link_Variable (Root, Behavior_Variable);
            if not Success then
               Error_Loc (1) := BATN.Loc (BATN.First_Node
                              (BATN.Identifiers (Behavior_Variable)));
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
                  --  vérifier l'unicité des identifiants des states
                  --  dans le composant parent (où l'annexe
                  --  comportementale est définie)
                  Test_With_Identifiers_In_Parent_Component
                   (List_Node, Parent_Component);
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

         --  l'annexe comportementale pour un sous­-programme doit définir
         --  un seul état (state) initial, un seul état final
         --  et pas d'états complets

      then

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

         --  l'annexe comportementale pour un thread doit définir
         --  un seul état (state) initial, un ou plusieurs états finaux
         --  et un ou plusieurs états complets
      then
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
                --  vérifier l'unicité des identifiants des transitions
                --  dans le composant parent (où l'annexe
                --  comportementale est définie)
                Test_With_Identifiers_In_Parent_Component
                   (Behavior_Transition_Idt
                        (Transition (Behavior_Transition)), Parent_Component);
         end if;
         Success := Success and then Analyze_Behavior_Transition
                          (Transition (Behavior_Transition),
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
              and then Component_Category'Val (Category
                          (Parent_Component)) = CC_Subprogram
            then
               Error_Loc (1) := BATN.Loc (Node_Id (Source_Transition_List));
               DE ("Subprogram components must not contain" &
                   " a dispatch condition in any of its transitions");
               Success := False;
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
                                (Condition (Behavior_Condition (Transition)),
                                 BA_Root, Parent_Component);
      end if;
      return Success;
   end Analyze_Behavior_Transition;

   --------------------------------
   -- Analyze_Dispatch_Condition --
   --------------------------------

   function Analyze_Dispatch_Condition
     (Condition         : Node_Id;
      BA_Root           : Node_Id;
      Parent_Component  : Node_Id)
     return Boolean
   is
      use ATN;
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
         Dispatch_Conjunction_Node := BATN.First_Node (Dispatch_Conjunction
                                  (Dispatch_Trigger_Condition (Condition)));
         while Present (Dispatch_Conjunction_Node) loop
            Dispatch_Trigger_Event := BATN.First_Node (Dispatch_Triggers
                                         (Dispatch_Conjunction_Node));
            while Present (Dispatch_Trigger_Event) loop
               Pointed_Node := Link_Dispatch_Trigger_Event
                                 (Dispatch_Trigger_Event, Parent_Component);
               if Present (Pointed_Node) then
                  if ATN.Kind (Pointed_Node) = K_Port_Spec
                  then
                     if not Is_Event (Pointed_Node) or else
                      not Is_In (Pointed_Node)
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
            Pointed_Node := Link_Frozen_Port (Frozen_Port, Parent_Component);
            if Present (Pointed_Node) then
               if not Is_In (Pointed_Node)
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
      Component  : Node_Id)
     return Node_Id
   is
      List_Of_Port_Spec : Node_List;
      N1 : Node_Id;
      Node : Node_Id := No_Node;
   begin
      List_Of_Port_Spec := Find_All_Subclauses
                          (Component, (1 => ATN.K_Port_Spec));
      N1 := List_Of_Port_Spec.First;
      while Present (N1) loop
         if Get_Name_String (Remove_Prefix_From_Name
                              ("%ba%", BATN.Name (Frozen_Port))) =
             Get_Name_String (ATN.Name (ATN.Identifier (N1)))
         then
            Node := N1;
         end if;
         N1 := ATN.Next_Entity (N1);
      end loop;
      return Node;

   end Link_Frozen_Port;

   ---------------------------------
   -- Link_Dispatch_Trigger_Event --
   ---------------------------------

   function Link_Dispatch_Trigger_Event
     (Dispatch_Trigger_Event : Node_Id;
      Component              : Node_Id)
     return Node_Id
   is
      List_Of_Port_Spec_Or_Subcomp_Access : Node_List;
      N1 : Node_Id;
      Node : Node_Id := No_Node;
   begin
      List_Of_Port_Spec_Or_Subcomp_Access := Find_All_Subclauses
                                    (Component, (ATN.K_Port_Spec,
                                       ATN.K_Subcomponent_Access));
      N1 := List_Of_Port_Spec_Or_Subcomp_Access.First;
      while Present (N1) loop
         if Get_Name_String (Remove_Prefix_From_Name
             ("%ba%", BATN.Name (Dispatch_Trigger_Event))) =
             Get_Name_String (ATN.Name (ATN.Identifier (N1)))
         then
            Node := N1;
         end if;
         N1 := ATN.Next_Entity (N1);
      end loop;
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
      Component  : Node_Id)
     return Boolean
   is

      use ATN;

      List_Of_Identifiers_In_Component : Node_List;
      N1 : Node_Id;
      Success   : Boolean := True;
   begin
      List_Of_Identifiers_In_Component := Find_All_Subclauses
                          (Component, (ATN.K_Port_Spec,
                                       ATN.K_Parameter,
                                       ATN.K_Feature_Group_Spec,
                                       ATN.K_Subcomponent_Access,
                                       ATN.K_Mode,
                                       ATN.K_Subcomponent));

      N1 := List_Of_Identifiers_In_Component.First;
      while Present (N1) loop
         if  (ATN.Kind (N1) = ATN.k_Subcomponent and then
           Component_Category'Val (Category (N1)) = CC_Data)
           or else ATN.Kind (N1) = ATN.k_Port_Spec
           or else ATN.Kind (N1) = ATN.k_Parameter
           or else ATN.Kind (N1) = ATN.k_Feature_Group_Spec
           or else ATN.Kind (N1) = ATN.k_Subcomponent_Access
           or else ATN.Kind (N1) = ATN.k_Mode
         then
            if Get_Name_String (Remove_Prefix_From_Name
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
         end if;
         N1 := ATN.Next_Entity (N1);
      end loop;
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
      List_Of_Modes : Node_List;
      N1 : Node_Id;
      Success   : Boolean := False;
   begin

      if not ANU.Is_Empty (ATN.Modes (Component)) then
         List_Of_Modes := Find_All_Subclauses
                          (Component, (1 => ATN.K_Mode));
         N1 := List_Of_Modes.First;
         while Present (N1) loop
            if Get_Name_String (Remove_Prefix_From_Name
                                ("%ba%", BATN.Name (State))) =
                Get_Name_String (ATN.Name (ATN.Identifier (N1)))
            then
               Success := True;
            end if;
            exit when Success;
            N1 := ATN.Next_Entity (N1);
         end loop;
      end if;
      return Success;

   end Exist_In_Modes;

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

      Success            : Boolean := False;
      List_Of_Component_Types : Node_List;
      List_Of_Component_Impls : Node_List;
      N1 : Node_Id;
   begin
      if not Is_Empty (Package_Name (BATN.Classifier_Ref (Node)))
      then
         --  à revoir ce test
         Success := True;
      else
         if Present (BATN.Component_Impl (BATN.Classifier_Ref (Node)))
         then
            List_Of_Component_Impls := Find_All_Declarations
                          (Root, (1 => ATN.K_Component_Implementation),
                           No_Node);
            N1 := List_Of_Component_Impls.First;
            while Present (N1) loop
               if Component_Category'Val (Category (N1)) = CC_Data then
                  if Get_Name_String (Remove_Prefix_From_Name
                     ("%ba%", BATN.Name (BATN.Component_Type
                      (BATN.Classifier_Ref (Node)))))
                     & "." & Get_Name_String (Remove_Prefix_From_Name
                     ("%ba%", BATN.Name (BATN.Component_Impl
                      (BATN.Classifier_Ref (Node))))) =
                     Get_Name_String (ATN.Name (ATN.Identifier (N1)))
                  then
                     Success := True;
                  end if;
               end if;
               exit when Success;
               N1 := ATN.Next_Entity (N1);
            end loop;
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
            List_Of_Component_Types := Find_All_Declarations
                          (Root, (1 => ATN.K_Component_Type), No_Node);
            N1 := List_Of_Component_Types.First;
            while Present (N1) loop
               if Component_Category'Val (Category (N1)) = CC_Data then
                  if Get_Name_String (Remove_Prefix_From_Name
                     ("%ba%", BATN.Name (BATN.Component_Type
                       (BATN.Classifier_Ref (Node))))) =
                     Get_Name_String (ATN.Name (ATN.Identifier (N1)))
                  then
                     Success := True;
                  end if;
               end if;
               exit when Success;
               N1 := ATN.Next_Entity (N1);
            end loop;
            if not Success then
               Error_Loc (1) := BATN.Loc (BATN.Component_Type
                                (BATN.Classifier_Ref (Node)));
               Error_Name (1) := BATN.Display_Name (BATN.Component_Type
                                (BATN.Classifier_Ref (Node)));
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

   --  procedure Affiche (First_Node_List : Node_Id)
   --  is
   --   Id : Node_Id;
   --  begin
   --   Id := First_Node_List;
   --   while Present (Id) loop
   --      Put_Line (Get_Name_String
   --                (BATN.Name (BATN.Item (Id))));
   --      Id := Next_Node (Id);
   --   end loop;
   --  end Affiche;
end Ocarina.Analyzer.AADL_BA;
