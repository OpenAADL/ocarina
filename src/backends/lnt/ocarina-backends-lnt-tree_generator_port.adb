------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BACKENDS.LNT.TREE_GENERATOR_PORT                  --
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

with Ocarina.Namet; use Ocarina.Namet;

with Ocarina.Backends;
with Ocarina.Backends.LNT.Nutils;
with Ocarina.Backends.LNT.Nodes;
with Ocarina.Backends.LNT.Components;

use Ocarina.Backends.LNT.Components;

with Ada.Text_IO; use Ada.Text_IO;

package body Ocarina.Backends.LNT.Tree_Generator_Port is

   package BLN renames Ocarina.Backends.LNT.Nodes;
   package BLNu renames Ocarina.Backends.LNT.Nutils;
   use BLN;
   use BLNu;

   procedure Add_LNT_Data_Port (L_Ports : List_Id);
   procedure Add_LNT_Event_Port (L_Ports : List_Id; Is_Sporadic : boolean);

   function Make_Loop_Select (In_Select : List_Id)
     return Node_Id;

   function Make_Loop_Select (In_Select : List_Id)
     return Node_Id is
      N : Node_Id;
   begin
      N := Make_Loop_Statement
        (New_List (Make_Select_Statement (In_Select)));
      return N;
   end Make_Loop_Select;

   procedure Add_LNT_Data_Port (L_Ports : List_Id) is
      N : Node_Id;
      In_Select : List_Id;
   begin
      In_Select := New_List (
        Make_Select_Statement_Alternative
         (New_List (Make_Communication_Statement
                 (Make_Identifier ("Input"),
                  New_List (Make_Offer_Statement
                      (No_Node,
                       Make_Identifier ("AADLDATA"),
                       false)),
                  false,
                  No_Node))),
        Make_Select_Statement_Alternative
         (New_List (Make_Communication_Statement
                 (Make_Identifier ("Output"),
                  New_List (Make_Offer_Statement
                      (No_Node,
                       Make_Identifier ("AADLDATA"),
                       false)),
                  false,
                  No_Node)))
      );
      N :=  Make_Process_Definition
      (No_Node, Make_Identifier ("Data_Port"),
       No_List,
       New_List (
         Make_Gate_Declaration
           (Make_Identifier ("LNT_Channel_Data"),
            Make_Identifier ("Input")),
         Make_Gate_Declaration
           (Make_Identifier ("LNT_Channel_Data"),
            Make_Identifier ("Output"))),
       No_List,
       No_List,
       New_List (Make_Loop_Select (In_Select)));

      BLNu.Append_Node_To_List (N, L_Ports);
   end Add_LNT_Data_Port;

   procedure Add_LNT_Event_Port (L_Ports : List_Id;
      Is_Sporadic : boolean) is
      N : Node_Id;
      N_Port : Node_Id;
      Notify : Node_Id;
      L_Parameters : List_Id;
      L_Gates : List_Id;
      Var_Dec : List_Id;
      Out_Loop : List_Id;
      In_Select : List_Id;
      In_Select_1 : List_Id;

   begin
      L_Parameters := New_List;

      BLNu.Append_Node_To_List (
       Make_Parameter_Specification (
         Make_Identifier ("Queue_Size"),
         Make_Identifier ("Nat")),
         L_Parameters);

      Var_Dec := New_List (
         Make_Var_Declaration (
          Make_Identifier ("Nb_Input"),
          Make_Identifier ("Nat")));
      if (Is_Sporadic) then
         BLNu.Append_Node_To_List (
           Make_Var_Declaration (
             Make_Identifier ("Is_New"),
             Make_Identifier ("bool")),
           Var_Dec);
      end if;
      Out_Loop := New_List (
         Make_Assignment_Statement (
          Make_Identifier ("Nb_Input"),
          Make_Identifier ("0"))
      );
      if (Is_Sporadic) then
         BLNu.Append_Node_To_List (
           Make_Assignment_Statement (
             Make_Identifier ("Is_New"),
             Make_Identifier ("false")),
           Out_Loop);
      end if;

      In_Select_1 := New_List (
            Make_Communication_Statement (
              Make_Identifier ("Input"),
              New_List (Make_Identifier ("AADLDATA"))),

            Make_If_Statement (
              Make_Infix_Function_Call_Expression (
                      Make_Predefined_Function (
                         K_Less_Than, false),
                      Make_Identifier ("Nb_Input"),
                      Make_Identifier ("Queue_Size")),

              New_List (
                Make_Assignment_Statement (
                  Make_Identifier ("Nb_Input"),
                    Make_Infix_Function_Call_Expression (
                      Make_IDentifier ("+"),
                      Make_Identifier ("Nb_Input"),
                      Make_Nat (1)))),
              No_List,
              No_List));
      if (Is_Sporadic) then
         BLNu.Append_Node_To_List (
           Make_Assignment_Statement (
               Make_Identifier ("Is_New"),
               Make_Identifier ("true")),
           In_Select_1);
      end if;

      In_Select := New_List (
        Make_Select_Statement_Alternative (In_Select_1),

        Make_Select_Statement_Alternative (
          New_List (
            Make_If_Statement (
              Make_Infix_Function_Call_Expression (
                      Make_Predefined_Function (
                         K_Greater_Than, false),
                      Make_Identifier ("Nb_Input"),
                      Make_Identifier ("0")),
              New_List (
                 Make_Communication_Statement (
                  Make_Identifier ("Output"),
                  New_List (Make_Identifier ("AADLDATA"))),
                 Make_Assignment_Statement (
                  Make_Identifier ("Nb_Input"),
                    Make_Infix_Function_Call_Expression (
                      Make_IDentifier ("-"),
                      Make_Identifier ("Nb_Input"),
                      Make_Nat (1)))),
              No_List,
              No_List)))
      );
      if (Is_Sporadic) then
         Notify := Make_Select_Statement_Alternative (
          New_List (
            Make_If_Statement (
              Make_Identifier ("Is_New"),
              New_List (
                 Make_Communication_Statement (
                  Make_Identifier ("Notify"),
                  New_List (Make_Identifier ("Incoming_Event"))),
                 Make_Assignment_Statement (
                  Make_Identifier ("Is_New"),
                  Make_Identifier ("false"))),
              No_List,
              New_List (
                 Make_Communication_Statement (
                  Make_Identifier ("Notify"),
                  New_List (Make_Identifier ("No_Event"))))
              )));
         BLNu.Append_Node_To_List (Notify, In_Select);
      end if;
      L_Gates := New_List (
         Make_Gate_Declaration
           (Make_Identifier ("LNT_Channel_Data"),
            Make_Identifier ("Input")),
         Make_Gate_Declaration
           (Make_Identifier ("LNT_Channel_Data"),
            Make_Identifier ("Output")));
      if (Is_Sporadic) then
         BLNu.Append_Node_To_List (
           Make_Gate_Declaration
             (Make_Identifier ("LNT_Channel_Event"),
              Make_Identifier ("Notify")),
           L_Gates);
      end if;

      if (Is_Sporadic) then
         N_Port := Make_Identifier ("Event_Port_Sporadic");
      else
         N_Port := Make_Identifier ("Event_Port_Periodic");
      end if;

      N :=  Make_Process_Definition
      (No_Node, N_Port,
       L_Parameters,
       L_Gates,
       No_List,
       No_List,
       New_List (Make_Var_Loop_Select (Var_Dec, Out_Loop, In_Select)));

      BLNu.Append_Node_To_List (N, L_Ports);
   end Add_LNT_Event_Port;

   Module_Node : Node_Id := No_Node;
   Definitions_List : List_Id := No_List;
   Modules_List : List_Id := No_List;
   Predefined_Functions_List : List_Id := No_List;

   --  Module_Pragma : List_Id := No_List;

   function Generate_LNT_Port
     return Node_Id is
   begin
      Put_Line ("Begin Port");

      Module_Node := Make_Module_Definition
       (New_Identifier (Get_String_Name ("_Ports"),
                        Get_Name_String (System_Name)));
      Definitions_List := New_List;
      Add_LNT_Data_Port (Definitions_List);
      Add_LNT_Event_Port (Definitions_List, false);
      if (Sporadic_Thread_Number > 0) then
         Add_LNT_Event_Port (Definitions_List, true);
      end if;

      Modules_List := New_List (New_Identifier (
                        Get_String_Name ("_Types"),
                        Get_Name_String (System_Name)));
      Predefined_Functions_List := New_List;

      Set_Definitions (Module_Node, Definitions_List);
      Set_Modules (Module_Node, Modules_List);
      Set_Predefined_Functions (Module_Node, Predefined_Functions_List);
      return Module_Node;
   end Generate_LNT_Port;

end Ocarina.Backends.LNT.Tree_Generator_Port;
