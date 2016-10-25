pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.Backends.PN.Debug; use Ocarina.Backends.PN.Debug;

package body Ocarina.Backends.PN.Nodes is

   pragma Warnings (Off);
   use Entries;

   function Kind (N : Node_Id) return Node_Kind is
   begin
      return Table (Types.Node_Id (N)).Kind;
   end Kind;

   procedure Set_Kind (N : Node_Id; V : Node_Kind) is
   begin
      Table (Types.Node_Id (N)).Kind := V;
   end Set_Kind;

   function Loc (N : Node_Id) return Location is
   begin
      return Table (Types.Node_Id (N)).Loc;
   end Loc;

   procedure Set_Loc (N : Node_Id; V : Location) is
   begin
      Table (Types.Node_Id (N)).Loc := V;
   end Set_Loc;

   function Next_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Node
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Place
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Generated
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Classes
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Domains
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variable_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variables
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking_Token
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc_Valuation
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Processor_Priority
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Specific_Informations
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Guard
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Node
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Place
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Generated
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Classes
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Domains
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variable_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variables
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking_Token
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc_Valuation
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Processor_Priority
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Specific_Informations
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Guard
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Next_Node;

   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;

   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Name;

   function Ocarina_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Ocarina_Node;

   procedure Set_Ocarina_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Ocarina_Node;

   function Corresponding_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Corresponding_Entity;

   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Corresponding_Entity;

   function Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Node
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Place
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Domains
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variable_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking_Token
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc_Valuation
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Identifier;

   procedure Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Node
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Place
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Domains
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variable_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking_Token
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc_Valuation
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Identifier;

   function Scoped_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Node
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Place
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Domains
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variable_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking_Token
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc_Valuation
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Scoped_Name;

   procedure Set_Scoped_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Node
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Place
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Domains
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variable_Item
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking_Token
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc_Valuation
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Place
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Scoped_Name;

   function Pn_From (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Pn_From;

   procedure Set_Pn_From (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Pn_From;

   function Pn_To (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Pn_To;

   procedure Set_Pn_To (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Arc
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Pn_To;

   function Pn_Arcs_In (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Pn_Arcs_In;

   procedure Set_Pn_Arcs_In (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Pn_Arcs_In;

   function Pn_Arcs_Out (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Pn_Arcs_Out;

   procedure Set_Pn_Arcs_Out (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Transition
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Pn_Arcs_Out;

   function Public_Interfaces (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Public_Interfaces;

   procedure Set_Public_Interfaces (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Public_Interfaces;

   function Internal_Transitions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (7));
   end Internal_Transitions;

   procedure Set_Internal_Transitions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Internal_Transitions;

   function Internal_Places (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (8));
   end Internal_Places;

   procedure Set_Internal_Places (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Component
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Internal_Places;

   function Pn_Subcomponents (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Pn_Subcomponents;

   procedure Set_Pn_Subcomponents (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Pn_Subcomponents;

   function Pn_Interconnections (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Pn_Interconnections;

   procedure Set_Pn_Interconnections (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Box);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Pn_Interconnections;

   function Pn_Box (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Generated);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Pn_Box;

   procedure Set_Pn_Box (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Generated);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Pn_Box;

   function Pn_Formalism_Specific_Informations (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Generated);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Pn_Formalism_Specific_Informations;

   procedure Set_Pn_Formalism_Specific_Informations (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Generated);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Pn_Formalism_Specific_Informations;

   function Formalism (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Generated);

      return Value_Id (Table (Types.Node_Id (N)).L (4));
   end Formalism;

   procedure Set_Formalism (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pn_Generated);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Formalism;

   function Low (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Low;

   procedure Set_Low (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Low;

   function High (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range);

      return Value_Id (Table (Types.Node_Id (N)).L (2));
   end High;

   procedure Set_High (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Range);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_High;

   function Enum (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Enum;

   procedure Set_Enum (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Class_Item_Enum);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Enum;

   function Class_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Classes);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Class_List;

   procedure Set_Class_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Classes);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Class_List;

   function Domain_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Domains);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Domain_List;

   procedure Set_Domain_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Domains);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Domain_List;

   function Class_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variables);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Class_Type;

   procedure Set_Class_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variables);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Class_Type;

   function Variable_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variables);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Variable_List;

   procedure Set_Variable_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Formalism_Variables);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Variable_List;

   function Aadl_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Aadl_Instance;

   procedure Set_Aadl_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Aadl_Instance;

   function Pn_Id (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id);

      return Value_Id (Table (Types.Node_Id (N)).L (2));
   end Pn_Id;

   procedure Set_Pn_Id (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Aadl_Id);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Pn_Id;

   function Classes (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Classes;

   procedure Set_Classes (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Classes;

   function Domains (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Domains;

   procedure Set_Domains (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Domains;

   function Variables (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Variables;

   procedure Set_Variables (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Variables;

   function Threads_Count (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      return Value_Id (Table (Types.Node_Id (N)).L (5));
   end Threads_Count;

   procedure Set_Threads_Count (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Threads_Count;

   function Threads_Ids (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Threads_Ids;

   procedure Set_Threads_Ids (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Threads_Ids;

   function Ports_Ids (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      return List_Id (Table (Types.Node_Id (N)).L (7));
   end Ports_Ids;

   procedure Set_Ports_Ids (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Specific_Informations);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Ports_Ids;

   function Tokens (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Tokens;

   procedure Set_Tokens (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Marking);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Tokens;

   function Domain (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Domain;

   procedure Set_Domain (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Domain;

   function Marking (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Marking;

   procedure Set_Marking (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Marking;

   function Nb_T (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place);

      return Value_Id (Table (Types.Node_Id (N)).L (6));
   end Nb_T;

   procedure Set_Nb_T (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Place);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Nb_T;

   function Operator (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Operator;

   procedure Set_Operator (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Operator;

   function Left_Op (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard);

      return Value_Id (Table (Types.Node_Id (N)).L (2));
   end Left_Op;

   procedure Set_Left_Op (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Left_Op;

   function Right_Op (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard);

      return Value_Id (Table (Types.Node_Id (N)).L (6));
   end Right_Op;

   procedure Set_Right_Op (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition_Guard);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Right_Op;

   function Guards (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Guards;

   procedure Set_Guards (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Transition);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Guards;

   function Is_Colored (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc_Valuation);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Colored;

   procedure Set_Is_Colored (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc_Valuation);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Colored;

   function Valuations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Valuations;

   procedure Set_Valuations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_CPN_Arc);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Valuations;

   function P_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Processor_Priority);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end P_Instance;

   procedure Set_P_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Processor_Priority);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_P_Instance;

   function Bounded_Trans (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Processor_Priority);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Bounded_Trans;

   procedure Set_Bounded_Trans (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Processor_Priority);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Bounded_Trans;

   function Th_Number (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Specific_Informations);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Th_Number;

   procedure Set_Th_Number (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Specific_Informations);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Th_Number;

   function Hyperperiod (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Specific_Informations
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      return Value_Id (Table (Types.Node_Id (N)).L (2));
   end Hyperperiod;

   procedure Set_Hyperperiod (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Specific_Informations
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Hyperperiod;

   function Priorities (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Specific_Informations);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Priorities;

   procedure Set_Priorities (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Specific_Informations);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Priorities;

   function Lower_Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Guard);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Lower_Value;

   procedure Set_Lower_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Guard);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Lower_Value;

   function Higher_Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Guard);

      return Value_Id (Table (Types.Node_Id (N)).L (2));
   end Higher_Value;

   procedure Set_Higher_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Guard);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Higher_Value;

   function Braces_Mode (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Guard);

      return Value_Id (Table (Types.Node_Id (N)).L (4));
   end Braces_Mode;

   procedure Set_Braces_Mode (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Guard);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Braces_Mode;

   function Tokens_Number (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Place);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Tokens_Number;

   procedure Set_Tokens_Number (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Place);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Tokens_Number;

   function Guard (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Guard;

   procedure Set_Guard (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Guard;

   function Priority (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition);

      return Value_Id (Table (Types.Node_Id (N)).L (7));
   end Priority;

   procedure Set_Priority (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Transition);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Priority;

   function Valuation (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc);

      return Value_Id (Table (Types.Node_Id (N)).L (7));
   end Valuation;

   procedure Set_Valuation (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Valuation;

   function is_Priority (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end is_Priority;

   procedure Set_is_Priority (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_TPN_Arc);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_is_Priority;

   function In_Ports (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end In_Ports;

   procedure Set_In_Ports (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_In_Ports;

   function Out_Ports (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (9));
   end Out_Ports;

   procedure Set_Out_Ports (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Out_Ports;

   function Call_Seq (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (10));
   end Call_Seq;

   procedure Set_Call_Seq (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Call_Seq;

   function Th_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Th_Instance;

   procedure Set_Th_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Thread_Pattern);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Th_Instance;

   function Port_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Port_Instance;

   procedure Set_Port_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Port_Instance;

   function Source_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Source_Instance;

   procedure Set_Source_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Source_Instance;

   function Target_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Target_Instance;

   procedure Set_Target_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Port_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Target_Instance;

   function Queue_Size (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      return Value_Id (Table (Types.Node_Id (N)).L (12));
   end Queue_Size;

   procedure Set_Queue_Size (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Queue_Size;

   function Has_CEP (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Has_CEP;

   procedure Set_Has_CEP (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Has_CEP;

   function Dispatch_Port (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Dispatch_Port;

   procedure Set_Dispatch_Port (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Data_Event_Port_Pattern);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Dispatch_Port;

   function Spg_Call (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Spg_Call;

   procedure Set_Spg_Call (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Call_Sequence_Pattern);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Spg_Call;

   function Param_In (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Param_In;

   procedure Set_Param_In (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Param_In;

   function Param_Out (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Param_Out;

   procedure Set_Param_Out (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Pattern);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Param_Out;

   function Par_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Par_Instance;

   procedure Set_Par_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Spg_Parameter_Pattern);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Par_Instance;

   function Proc_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Proc_Instance;

   procedure Set_Proc_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processor_Pattern);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Proc_Instance;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Identifier =>
            W_Identifier
              (Node_Id (N));
         when K_Pn_Node =>
            W_Pn_Node
              (Node_Id (N));
         when K_Pn_Arc =>
            W_Pn_Arc
              (Node_Id (N));
         when K_Pn_Place =>
            W_Pn_Place
              (Node_Id (N));
         when K_Pn_Transition =>
            W_Pn_Transition
              (Node_Id (N));
         when K_Pn_Component =>
            W_Pn_Component
              (Node_Id (N));
         when K_Pn_Box =>
            W_Pn_Box
              (Node_Id (N));
         when K_Pn_Generated =>
            W_Pn_Generated
              (Node_Id (N));
         when K_CPN_Formalism_Class_Item_Range =>
            W_CPN_Formalism_Class_Item_Range
              (Node_Id (N));
         when K_CPN_Formalism_Class_Item_Enum_Item =>
            W_CPN_Formalism_Class_Item_Enum_Item
              (Node_Id (N));
         when K_CPN_Formalism_Class_Item_Enum =>
            W_CPN_Formalism_Class_Item_Enum
              (Node_Id (N));
         when K_CPN_Formalism_Classes =>
            W_CPN_Formalism_Classes
              (Node_Id (N));
         when K_CPN_Formalism_Domains =>
            W_CPN_Formalism_Domains
              (Node_Id (N));
         when K_CPN_Formalism_Variable_Item =>
            W_CPN_Formalism_Variable_Item
              (Node_Id (N));
         when K_CPN_Formalism_Variables =>
            W_CPN_Formalism_Variables
              (Node_Id (N));
         when K_CPN_Aadl_Id =>
            W_CPN_Aadl_Id
              (Node_Id (N));
         when K_CPN_Specific_Informations =>
            W_CPN_Specific_Informations
              (Node_Id (N));
         when K_CPN_Marking_Token =>
            W_CPN_Marking_Token
              (Node_Id (N));
         when K_CPN_Marking =>
            W_CPN_Marking
              (Node_Id (N));
         when K_CPN_Place =>
            W_CPN_Place
              (Node_Id (N));
         when K_CPN_Transition_Guard =>
            W_CPN_Transition_Guard
              (Node_Id (N));
         when K_CPN_Transition =>
            W_CPN_Transition
              (Node_Id (N));
         when K_CPN_Arc_Valuation =>
            W_CPN_Arc_Valuation
              (Node_Id (N));
         when K_CPN_Arc =>
            W_CPN_Arc
              (Node_Id (N));
         when K_TPN_Processor_Priority =>
            W_TPN_Processor_Priority
              (Node_Id (N));
         when K_TPN_Specific_Informations =>
            W_TPN_Specific_Informations
              (Node_Id (N));
         when K_TPN_Guard =>
            W_TPN_Guard
              (Node_Id (N));
         when K_TPN_Place =>
            W_TPN_Place
              (Node_Id (N));
         when K_TPN_Transition =>
            W_TPN_Transition
              (Node_Id (N));
         when K_TPN_Arc =>
            W_TPN_Arc
              (Node_Id (N));
         when K_Thread_Pattern =>
            W_Thread_Pattern
              (Node_Id (N));
         when K_Port_Pattern =>
            W_Port_Pattern
              (Node_Id (N));
         when K_Data_Port_Pattern =>
            W_Data_Port_Pattern
              (Node_Id (N));
         when K_Data_Event_Port_Pattern =>
            W_Data_Event_Port_Pattern
              (Node_Id (N));
         when K_Call_Sequence_Pattern =>
            W_Call_Sequence_Pattern
              (Node_Id (N));
         when K_Subprogram_Call_Pattern =>
            W_Subprogram_Call_Pattern
              (Node_Id (N));
         when K_Spg_Parameter_Pattern =>
            W_Spg_Parameter_Pattern
              (Node_Id (N));
         when K_Processor_Pattern =>
            W_Processor_Pattern
              (Node_Id (N));
         when others =>
            null;
      end case;
   end W_Node;

   procedure W_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Ocarina_Node",
         "Node_Id",
         Image (Ocarina_Node (N)),
         Int (Ocarina_Node (N)));
      W_Node_Attribute
        ("Corresponding_Entity",
         "Node_Id",
         Image (Corresponding_Entity (N)),
         Int (Corresponding_Entity (N)));
   end W_Identifier;

   procedure W_Pn_Node (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
   end W_Pn_Node;

   procedure W_Pn_Arc (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Pn_From",
         "Node_Id",
         Image (Pn_From (N)),
         Int (Pn_From (N)));
      W_Node_Attribute
        ("Pn_To",
         "Node_Id",
         Image (Pn_To (N)),
         Int (Pn_To (N)));
   end W_Pn_Arc;

   procedure W_Pn_Place (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
   end W_Pn_Place;

   procedure W_Pn_Transition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Pn_Arcs_In",
         "List_Id",
         Image (Pn_Arcs_In (N)),
         Int (Pn_Arcs_In (N)));
      W_Node_Attribute
        ("Pn_Arcs_Out",
         "List_Id",
         Image (Pn_Arcs_Out (N)),
         Int (Pn_Arcs_Out (N)));
   end W_Pn_Transition;

   procedure W_Pn_Component (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Public_Interfaces",
         "List_Id",
         Image (Public_Interfaces (N)),
         Int (Public_Interfaces (N)));
      W_Node_Attribute
        ("Internal_Transitions",
         "List_Id",
         Image (Internal_Transitions (N)),
         Int (Internal_Transitions (N)));
      W_Node_Attribute
        ("Internal_Places",
         "List_Id",
         Image (Internal_Places (N)),
         Int (Internal_Places (N)));
   end W_Pn_Component;

   procedure W_Pn_Box (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Pn_Subcomponents",
         "List_Id",
         Image (Pn_Subcomponents (N)),
         Int (Pn_Subcomponents (N)));
      W_Node_Attribute
        ("Pn_Interconnections",
         "List_Id",
         Image (Pn_Interconnections (N)),
         Int (Pn_Interconnections (N)));
   end W_Pn_Box;

   procedure W_Pn_Generated (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Pn_Box",
         "Node_Id",
         Image (Pn_Box (N)),
         Int (Pn_Box (N)));
      W_Node_Attribute
        ("Pn_Formalism_Specific_Informations",
         "Node_Id",
         Image (Pn_Formalism_Specific_Informations (N)),
         Int (Pn_Formalism_Specific_Informations (N)));
      W_Node_Attribute
        ("Formalism",
         "Value_Id",
         Image (Formalism (N)));
   end W_Pn_Generated;

   procedure W_CPN_Formalism_Class_Item_Range (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Low",
         "Value_Id",
         Image (Low (N)));
      W_Node_Attribute
        ("High",
         "Value_Id",
         Image (High (N)));
   end W_CPN_Formalism_Class_Item_Range;

   procedure W_CPN_Formalism_Class_Item_Enum_Item (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
   end W_CPN_Formalism_Class_Item_Enum_Item;

   procedure W_CPN_Formalism_Class_Item_Enum (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Enum",
         "List_Id",
         Image (Enum (N)),
         Int (Enum (N)));
   end W_CPN_Formalism_Class_Item_Enum;

   procedure W_CPN_Formalism_Classes (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Class_List",
         "List_Id",
         Image (Class_List (N)),
         Int (Class_List (N)));
   end W_CPN_Formalism_Classes;

   procedure W_CPN_Formalism_Domains (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Domain_List",
         "List_Id",
         Image (Domain_List (N)),
         Int (Domain_List (N)));
   end W_CPN_Formalism_Domains;

   procedure W_CPN_Formalism_Variable_Item (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
   end W_CPN_Formalism_Variable_Item;

   procedure W_CPN_Formalism_Variables (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Class_Type",
         "Node_Id",
         Image (Class_Type (N)),
         Int (Class_Type (N)));
      W_Node_Attribute
        ("Variable_List",
         "List_Id",
         Image (Variable_List (N)),
         Int (Variable_List (N)));
   end W_CPN_Formalism_Variables;

   procedure W_CPN_Aadl_Id (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Aadl_Instance",
         "Node_Id",
         Image (Aadl_Instance (N)),
         Int (Aadl_Instance (N)));
      W_Node_Attribute
        ("Pn_Id",
         "Value_Id",
         Image (Pn_Id (N)));
   end W_CPN_Aadl_Id;

   procedure W_CPN_Specific_Informations (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Classes",
         "Node_Id",
         Image (Classes (N)),
         Int (Classes (N)));
      W_Node_Attribute
        ("Domains",
         "List_Id",
         Image (Domains (N)),
         Int (Domains (N)));
      W_Node_Attribute
        ("Variables",
         "List_Id",
         Image (Variables (N)),
         Int (Variables (N)));
      W_Node_Attribute
        ("Threads_Count",
         "Value_Id",
         Image (Threads_Count (N)));
      W_Node_Attribute
        ("Threads_Ids",
         "List_Id",
         Image (Threads_Ids (N)),
         Int (Threads_Ids (N)));
      W_Node_Attribute
        ("Ports_Ids",
         "List_Id",
         Image (Ports_Ids (N)),
         Int (Ports_Ids (N)));
   end W_CPN_Specific_Informations;

   procedure W_CPN_Marking_Token (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
   end W_CPN_Marking_Token;

   procedure W_CPN_Marking (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Tokens",
         "List_Id",
         Image (Tokens (N)),
         Int (Tokens (N)));
   end W_CPN_Marking;

   procedure W_CPN_Place (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Domain",
         "Node_Id",
         Image (Domain (N)),
         Int (Domain (N)));
      W_Node_Attribute
        ("Marking",
         "Node_Id",
         Image (Marking (N)),
         Int (Marking (N)));
      W_Node_Attribute
        ("Nb_T",
         "Value_Id",
         Image (Nb_T (N)));
   end W_CPN_Place;

   procedure W_CPN_Transition_Guard (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Operator",
         "Value_Id",
         Image (Operator (N)));
      W_Node_Attribute
        ("Left_Op",
         "Value_Id",
         Image (Left_Op (N)));
      W_Node_Attribute
        ("Right_Op",
         "Value_Id",
         Image (Right_Op (N)));
   end W_CPN_Transition_Guard;

   procedure W_CPN_Transition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Pn_Arcs_In",
         "List_Id",
         Image (Pn_Arcs_In (N)),
         Int (Pn_Arcs_In (N)));
      W_Node_Attribute
        ("Pn_Arcs_Out",
         "List_Id",
         Image (Pn_Arcs_Out (N)),
         Int (Pn_Arcs_Out (N)));
      W_Node_Attribute
        ("Guards",
         "List_Id",
         Image (Guards (N)),
         Int (Guards (N)));
   end W_CPN_Transition;

   procedure W_CPN_Arc_Valuation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Is_Colored",
         "Boolean",
         Image (Is_Colored (N)));
   end W_CPN_Arc_Valuation;

   procedure W_CPN_Arc (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Pn_From",
         "Node_Id",
         Image (Pn_From (N)),
         Int (Pn_From (N)));
      W_Node_Attribute
        ("Pn_To",
         "Node_Id",
         Image (Pn_To (N)),
         Int (Pn_To (N)));
      W_Node_Attribute
        ("Valuations",
         "List_Id",
         Image (Valuations (N)),
         Int (Valuations (N)));
   end W_CPN_Arc;

   procedure W_TPN_Processor_Priority (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("P_Instance",
         "Node_Id",
         Image (P_Instance (N)),
         Int (P_Instance (N)));
      W_Node_Attribute
        ("Bounded_Trans",
         "List_Id",
         Image (Bounded_Trans (N)),
         Int (Bounded_Trans (N)));
   end W_TPN_Processor_Priority;

   procedure W_TPN_Specific_Informations (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Th_Number",
         "Value_Id",
         Image (Th_Number (N)));
      W_Node_Attribute
        ("Hyperperiod",
         "Value_Id",
         Image (Hyperperiod (N)));
      W_Node_Attribute
        ("Priorities",
         "List_Id",
         Image (Priorities (N)),
         Int (Priorities (N)));
   end W_TPN_Specific_Informations;

   procedure W_TPN_Guard (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Lower_Value",
         "Value_Id",
         Image (Lower_Value (N)));
      W_Node_Attribute
        ("Higher_Value",
         "Value_Id",
         Image (Higher_Value (N)));
      W_Node_Attribute
        ("Braces_Mode",
         "Value_Id",
         Image (Braces_Mode (N)));
   end W_TPN_Guard;

   procedure W_TPN_Place (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Tokens_Number",
         "Value_Id",
         Image (Tokens_Number (N)));
   end W_TPN_Place;

   procedure W_TPN_Transition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Pn_Arcs_In",
         "List_Id",
         Image (Pn_Arcs_In (N)),
         Int (Pn_Arcs_In (N)));
      W_Node_Attribute
        ("Pn_Arcs_Out",
         "List_Id",
         Image (Pn_Arcs_Out (N)),
         Int (Pn_Arcs_Out (N)));
      W_Node_Attribute
        ("Guard",
         "Node_Id",
         Image (Guard (N)),
         Int (Guard (N)));
      W_Node_Attribute
        ("Priority",
         "Value_Id",
         Image (Priority (N)));
   end W_TPN_Transition;

   procedure W_TPN_Arc (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Pn_From",
         "Node_Id",
         Image (Pn_From (N)),
         Int (Pn_From (N)));
      W_Node_Attribute
        ("Pn_To",
         "Node_Id",
         Image (Pn_To (N)),
         Int (Pn_To (N)));
      W_Node_Attribute
        ("Valuation",
         "Value_Id",
         Image (Valuation (N)));
      W_Node_Attribute
        ("is_Priority",
         "Boolean",
         Image (is_Priority (N)));
   end W_TPN_Arc;

   procedure W_Thread_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Public_Interfaces",
         "List_Id",
         Image (Public_Interfaces (N)),
         Int (Public_Interfaces (N)));
      W_Node_Attribute
        ("Internal_Transitions",
         "List_Id",
         Image (Internal_Transitions (N)),
         Int (Internal_Transitions (N)));
      W_Node_Attribute
        ("Internal_Places",
         "List_Id",
         Image (Internal_Places (N)),
         Int (Internal_Places (N)));
      W_Node_Attribute
        ("Hyperperiod",
         "Value_Id",
         Image (Hyperperiod (N)));
      W_Node_Attribute
        ("In_Ports",
         "List_Id",
         Image (In_Ports (N)),
         Int (In_Ports (N)));
      W_Node_Attribute
        ("Out_Ports",
         "List_Id",
         Image (Out_Ports (N)),
         Int (Out_Ports (N)));
      W_Node_Attribute
        ("Call_Seq",
         "List_Id",
         Image (Call_Seq (N)),
         Int (Call_Seq (N)));
      W_Node_Attribute
        ("Th_Instance",
         "Node_Id",
         Image (Th_Instance (N)),
         Int (Th_Instance (N)));
   end W_Thread_Pattern;

   procedure W_Port_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Public_Interfaces",
         "List_Id",
         Image (Public_Interfaces (N)),
         Int (Public_Interfaces (N)));
      W_Node_Attribute
        ("Internal_Transitions",
         "List_Id",
         Image (Internal_Transitions (N)),
         Int (Internal_Transitions (N)));
      W_Node_Attribute
        ("Internal_Places",
         "List_Id",
         Image (Internal_Places (N)),
         Int (Internal_Places (N)));
      W_Node_Attribute
        ("Port_Instance",
         "Node_Id",
         Image (Port_Instance (N)),
         Int (Port_Instance (N)));
      W_Node_Attribute
        ("Source_Instance",
         "Node_Id",
         Image (Source_Instance (N)),
         Int (Source_Instance (N)));
      W_Node_Attribute
        ("Target_Instance",
         "Node_Id",
         Image (Target_Instance (N)),
         Int (Target_Instance (N)));
   end W_Port_Pattern;

   procedure W_Data_Port_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Public_Interfaces",
         "List_Id",
         Image (Public_Interfaces (N)),
         Int (Public_Interfaces (N)));
      W_Node_Attribute
        ("Internal_Transitions",
         "List_Id",
         Image (Internal_Transitions (N)),
         Int (Internal_Transitions (N)));
      W_Node_Attribute
        ("Internal_Places",
         "List_Id",
         Image (Internal_Places (N)),
         Int (Internal_Places (N)));
      W_Node_Attribute
        ("Port_Instance",
         "Node_Id",
         Image (Port_Instance (N)),
         Int (Port_Instance (N)));
      W_Node_Attribute
        ("Source_Instance",
         "Node_Id",
         Image (Source_Instance (N)),
         Int (Source_Instance (N)));
      W_Node_Attribute
        ("Target_Instance",
         "Node_Id",
         Image (Target_Instance (N)),
         Int (Target_Instance (N)));
   end W_Data_Port_Pattern;

   procedure W_Data_Event_Port_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Public_Interfaces",
         "List_Id",
         Image (Public_Interfaces (N)),
         Int (Public_Interfaces (N)));
      W_Node_Attribute
        ("Internal_Transitions",
         "List_Id",
         Image (Internal_Transitions (N)),
         Int (Internal_Transitions (N)));
      W_Node_Attribute
        ("Internal_Places",
         "List_Id",
         Image (Internal_Places (N)),
         Int (Internal_Places (N)));
      W_Node_Attribute
        ("Port_Instance",
         "Node_Id",
         Image (Port_Instance (N)),
         Int (Port_Instance (N)));
      W_Node_Attribute
        ("Source_Instance",
         "Node_Id",
         Image (Source_Instance (N)),
         Int (Source_Instance (N)));
      W_Node_Attribute
        ("Target_Instance",
         "Node_Id",
         Image (Target_Instance (N)),
         Int (Target_Instance (N)));
      W_Node_Attribute
        ("Queue_Size",
         "Value_Id",
         Image (Queue_Size (N)));
      W_Node_Attribute
        ("Has_CEP",
         "Boolean",
         Image (Has_CEP (N)));
      W_Node_Attribute
        ("Dispatch_Port",
         "Boolean",
         Image (Dispatch_Port (N)));
   end W_Data_Event_Port_Pattern;

   procedure W_Call_Sequence_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Public_Interfaces",
         "List_Id",
         Image (Public_Interfaces (N)),
         Int (Public_Interfaces (N)));
      W_Node_Attribute
        ("Internal_Transitions",
         "List_Id",
         Image (Internal_Transitions (N)),
         Int (Internal_Transitions (N)));
      W_Node_Attribute
        ("Internal_Places",
         "List_Id",
         Image (Internal_Places (N)),
         Int (Internal_Places (N)));
      W_Node_Attribute
        ("Spg_Call",
         "List_Id",
         Image (Spg_Call (N)),
         Int (Spg_Call (N)));
   end W_Call_Sequence_Pattern;

   procedure W_Subprogram_Call_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Public_Interfaces",
         "List_Id",
         Image (Public_Interfaces (N)),
         Int (Public_Interfaces (N)));
      W_Node_Attribute
        ("Internal_Transitions",
         "List_Id",
         Image (Internal_Transitions (N)),
         Int (Internal_Transitions (N)));
      W_Node_Attribute
        ("Internal_Places",
         "List_Id",
         Image (Internal_Places (N)),
         Int (Internal_Places (N)));
      W_Node_Attribute
        ("Param_In",
         "List_Id",
         Image (Param_In (N)),
         Int (Param_In (N)));
      W_Node_Attribute
        ("Param_Out",
         "List_Id",
         Image (Param_Out (N)),
         Int (Param_Out (N)));
   end W_Subprogram_Call_Pattern;

   procedure W_Spg_Parameter_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Public_Interfaces",
         "List_Id",
         Image (Public_Interfaces (N)),
         Int (Public_Interfaces (N)));
      W_Node_Attribute
        ("Internal_Transitions",
         "List_Id",
         Image (Internal_Transitions (N)),
         Int (Internal_Transitions (N)));
      W_Node_Attribute
        ("Internal_Places",
         "List_Id",
         Image (Internal_Places (N)),
         Int (Internal_Places (N)));
      W_Node_Attribute
        ("Par_Instance",
         "Node_Id",
         Image (Par_Instance (N)),
         Int (Par_Instance (N)));
   end W_Spg_Parameter_Pattern;

   procedure W_Processor_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Scoped_Name",
         "Node_Id",
         Image (Scoped_Name (N)),
         Int (Scoped_Name (N)));
      W_Node_Attribute
        ("Public_Interfaces",
         "List_Id",
         Image (Public_Interfaces (N)),
         Int (Public_Interfaces (N)));
      W_Node_Attribute
        ("Internal_Transitions",
         "List_Id",
         Image (Internal_Transitions (N)),
         Int (Internal_Transitions (N)));
      W_Node_Attribute
        ("Internal_Places",
         "List_Id",
         Image (Internal_Places (N)),
         Int (Internal_Places (N)));
      W_Node_Attribute
        ("Proc_Instance",
         "Node_Id",
         Image (Proc_Instance (N)),
         Int (Proc_Instance (N)));
   end W_Processor_Pattern;

end Ocarina.Backends.PN.Nodes;
