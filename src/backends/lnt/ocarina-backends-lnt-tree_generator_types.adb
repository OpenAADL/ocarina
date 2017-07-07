------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BACKENDS.LNT.TREE_GENERATOR_TYPES                 --
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

with Ocarina.Backends;
with Ocarina.Backends.LNT.Nutils;
with Ocarina.Backends.LNT.Nodes;
with Ocarina.Backends.LNT.Components;

use Ocarina.Backends.LNT.Components;

with Ada.Text_IO; use Ada.Text_IO;

package body Ocarina.Backends.LNT.Tree_Generator_Types is

   --  LNT_Component_AADLIdt : LNT_Thread_Producer

   --  LNT_Type_LNTIdt : LNT_Type_Time
   --  LNT_Channel_LNTIdt : LNT_Channel_Request
   --  LNT_Function_LNTIdt : LNT_Function_Update

   package BLN renames Ocarina.Backends.LNT.Nodes;
   package BLNu renames Ocarina.Backends.LNT.Nutils;
   use BLN;
   use BLNu;

   procedure Add_LNT_Types (L_Types : List_Id;
       Thread_Number : Integer;
       PPCM : Integer);
   procedure Add_LNT_Channels (L_Channels : List_Id);
   procedure Add_LNT_Functions (L_Functions : List_Id;
       Thread_Number : Integer;
       PPCM : Integer);

   procedure Add_LNT_Types (L_Types : List_Id;
       Thread_Number : Integer;
       PPCM : Integer)
   is
      N : Node_Id;
   begin
      N := Make_Type_Def
       (Make_Identifier ("LNT_Type_Time_Constraint"),
        Make_Type_Exp
            (Make_Identifier ("Nat"),
             No_List, false, false, false, false, false,
             true, Make_RangeLNT (Make_Nat (0),
                                  Make_Nat (PPCM * 2))
            ),
        No_List,
        No_List);
      BLNu.Append_Node_To_List (N, L_Types);

      N := Make_Type_Def
       (Make_Identifier ("LNT_Type_Thread"),
          Make_Type_Exp (
             Make_Identifier ("LNT_Type_Time_Constraint"),
             No_List,
             false, false, false, false, true, false,
             Make_RangeLNT (Make_Nat (0),
                            Make_Nat (11))),
        No_List,
        No_List);
      BLNu.Append_Node_To_List (N, L_Types);

      N := Make_Type_Def
       (Make_Identifier ("LNT_Type_Thread_Array"),
          Make_Type_Exp (
             Make_Identifier ("LNT_Type_Thread"),
             No_List,
             false, false, false, false, true, false,
             Make_RangeLNT (Make_Nat (1),
                            Make_Nat (Thread_Number))),
        No_List,
        No_List);
      BLNu.Append_Node_To_List (N, L_Types);

      N := Make_Type_Def
       (Make_Identifier ("LNT_Type_Dispatch"),
          Make_Type_Exp (
             No_Node,
             New_List (
               Make_Type_Constructor (Make_Identifier
                ("T_Dispatch_Preemption"), No_List, No_List),
               Make_Type_Constructor (Make_Identifier
                ("T_Preemption_Completion"), No_List, No_List),
               Make_Type_Constructor (Make_Identifier
                ("T_Dispatch_Completion"), No_List, No_List),
               Make_Type_Constructor (Make_Identifier
                ("T_Preemption"), No_List, No_List),
               Make_Type_Constructor (Make_Identifier ("T_Stop"),
                                      No_List, No_List),
               Make_Type_Constructor (Make_Identifier ("T_Error"),
                                      No_List, No_List),
               Make_Type_Constructor (Make_Identifier ("T_Complete"),
                                      No_List, No_List)),
             false, false, false, false, false, false),
        No_List,
        No_List);
      BLNu.Append_Node_To_List (N, L_Types);

      N := Make_Type_Def
       (Make_Identifier ("LNT_Type_Event"),
          Make_Type_Exp (
             No_Node,
             New_List (
               Make_Type_Constructor (Make_Identifier ("Incoming_Event"),
                                      No_List, No_List),
               Make_Type_Constructor (Make_Identifier ("No_Event"),
                                      No_List, No_List)),
             false, false, false, false, false, false),
        No_List,
        No_List);
      BLNu.Append_Node_To_List (N, L_Types);
      --  When BA
      if not Is_Empty (LNT_States_List) then
         N := Make_Type_Def
         (Make_Identifier ("LNT_Type_States"),
          Make_Type_Exp (
             No_Node,
             LNT_States_List,
             false, false, false, false, false, false),
           No_List,
           No_List);
         BLNu.Append_Node_To_List (N, L_Types);
      end if;
   end Add_LNT_Types;

   procedure Add_LNT_Channels (L_Channels : List_Id) is
      N : Node_Id;
   begin

      N := Make_Channel
        (Make_Identifier ("LNT_Channel_Dispatch"),
         New_List (
         Make_Gate_Profile (New_List (Make_Identifier
                         ("LNT_Type_Dispatch")))));

      BLNu.Append_Node_To_List (N, L_Channels);

      N := Make_Channel
        (Make_Identifier ("LNT_Channel_Event"),
         New_List (
         Make_Gate_Profile (New_List (Make_Identifier ("LNT_Type_Event")))));

      BLNu.Append_Node_To_List (N, L_Channels);
      --  When BA
      if not Is_Empty (LNT_States_List) then
         N := Make_Channel (
            Make_Identifier ("LNT_Channel_Event_Port"),
            New_List (
            Make_Gate_Profile (New_List (Make_Identifier
                         ("Bool")))));
         BLNu.Append_Node_To_List (N, L_Channels);

         N := Make_Channel (
            Make_Identifier ("LNT_Channel_Event_Data_Port"),
            New_List (
            Make_Gate_Profile (New_List (
               Make_Identifier ("LNT_Type_Data"),
               Make_Identifier ("Bool")
               ))));
         BLNu.Append_Node_To_List (N, L_Channels);

         N := Make_Channel (
            Make_Identifier ("LNT_Channel_Data_Port"),
            New_List (
            Make_Gate_Profile (New_List (Make_Identifier
                         ("LNT_Type_Data")))));
         BLNu.Append_Node_To_List (N, L_Channels);
      end if;
   end Add_LNT_Channels;

   procedure Add_LNT_Functions (L_Functions : List_Id;
                                Thread_Number : Integer;
                                PPCM : Integer)
   is
      N : Node_Id;
   begin
      N := Make_Function_Definition
        (Make_Identifier ("Thread_Number"),
         No_List,
         Make_Identifier ("Nat"),
         No_List,
         No_List,
         New_List (Make_Return_Statement
          (Make_Nat (Thread_Number), true)));
      BLNu.Append_Node_To_List (N, L_Functions);

      N := Make_Function_Definition
        (Make_Identifier ("PPCM_THREAD"),
         No_List,
         Make_Identifier ("LNT_Type_Time_Constraint"),
         No_List,
         No_List,
         New_List (Make_Return_Statement (
            Make_Function_Call_Expression (
             Make_Identifier ("LNT_Type_Time_Constraint"),
             New_List (Make_Identifier (Integer'Image (PPCM)))),
            true)));
      BLNu.Append_Node_To_List (N, L_Functions);

      N :=  Make_Function_Definition
        (Make_Identifier ("_+_"),
         New_List (
              Make_Parameter_Specification
               (Make_Identifier ("n1"),
                Make_Identifier ("LNT_Type_Time_Constraint")),
              Make_Parameter_Specification
               (Make_Identifier ("n2"),
                Make_Identifier ("LNT_Type_Time_Constraint"))),
         Make_Identifier ("LNT_Type_Time_Constraint"),
         No_List,
         No_List,
         New_List (Make_Return_Statement (
         Make_Function_Call_Expression
         (Make_Identifier ("LNT_Type_Time_Constraint"),

          New_List (
           Make_Infix_Function_Call_Expression
          (Make_Identifier ("mod"),

          Make_Infix_Function_Call_Expression
           (Make_Identifier ("+"),
            Make_Function_Call_Expression
            (Make_Identifier ("Nat"),
             New_List (Make_Identifier ("n1"))),
            Make_Function_Call_Expression
            (Make_Identifier ("Nat"),
             New_List (Make_Identifier ("n2")))),
          Make_Nat ((Hyperperiod * 2))))), true)));
      BLNu.Append_Node_To_List (N, L_Functions);

      N :=  Make_Function_Definition
        (Make_Identifier ("_-_"),
         New_List (
              Make_Parameter_Specification
               (Make_Identifier ("n1"),
                Make_Identifier ("LNT_Type_Time_Constraint")),
              Make_Parameter_Specification
               (Make_Identifier ("n2"),
                Make_Identifier ("LNT_Type_Time_Constraint"))),
         Make_Identifier ("LNT_Type_Time_Constraint"),
         No_List,
         No_List,
         New_List (
           Make_If_Statement
            (Make_Infix_Function_Call_Expression
              (Make_Predefined_Function (K_Less_Than, false),
               Make_Identifier ("n1"),
               Make_Identifier ("n2")),
             New_List (
               Make_Return_Statement (
                Make_Function_Call_Expression (
                 Make_Identifier ("LNT_Type_Time_Constraint"),
                 New_List (
                 Make_Infix_Function_Call_Expression (
                   Make_Identifier ("mod"),
                   Make_Parenthesized_Expression (
                     Make_Infix_Function_Call_Expression (
                       Make_Identifier ("+"),
                       Make_Parenthesized_Expression (
                         Make_Infix_Function_Call_Expression (
                           Make_Identifier ("-"),
                           Make_Nat (Hyperperiod * 2),
                           Make_Function_Call_Expression (
                             Make_Identifier ("Nat"),
                             New_List (Make_Identifier ("n2"))))),
                       Make_Function_Call_Expression (
                         Make_Identifier ("Nat"),
                         New_List (Make_Identifier ("n1"))))),
                   Make_Nat (Hyperperiod * 2)))), true)),
             No_List,
             New_List (Make_Return_Statement (
             Make_Function_Call_Expression
             (Make_Identifier ("LNT_Type_Time_Constraint"),
              New_List (
                Make_Infix_Function_Call_Expression
                 (Make_Identifier ("mod"),
                  Make_Infix_Function_Call_Expression
                   (Make_Identifier ("-"),
                    Make_Function_Call_Expression
                     (Make_Identifier ("Nat"),
                      New_List (Make_Identifier ("n1"))),
                      Make_Function_Call_Expression
                       (Make_Identifier ("Nat"),
                 New_List (Make_Identifier ("n2")))),
                 Make_Nat (Hyperperiod * 2)))), true)))));
      BLNu.Append_Node_To_List (N, L_Functions);

      N :=  Make_Function_Definition
        (Make_Identifier ("_*_"),
         New_List (
              Make_Parameter_Specification
               (Make_Identifier ("n1"),
                Make_Identifier ("LNT_Type_Time_Constraint")),
              Make_Parameter_Specification
               (Make_Identifier ("n2"),
                Make_Identifier ("LNT_Type_Time_Constraint"))),
         Make_Identifier ("LNT_Type_Time_Constraint"),
         No_List,
         No_List,
         New_List (Make_Return_Statement (
         Make_Function_Call_Expression
         (Make_Identifier ("LNT_Type_Time_Constraint"),

          New_List (
           Make_Infix_Function_Call_Expression
          (Make_Identifier ("mod"),

          Make_Infix_Function_Call_Expression
           (Make_Identifier ("*"),
            Make_Function_Call_Expression
            (Make_Identifier ("Nat"),
             New_List (Make_Identifier ("n1"))),
            Make_Function_Call_Expression
            (Make_Identifier ("Nat"),
             New_List (Make_Identifier ("n2")))),
          Make_Nat (Hyperperiod * 2)))), true)));

      BLNu.Append_Node_To_List (N, L_Functions);

   end Add_LNT_Functions;

   Module_Node : Node_Id := No_Node;
   Definitions_List : List_Id := No_List;
   Modules_List : List_Id := No_List;
   Predefined_Functions_List : List_Id := No_List;

   --  Module_Pragma : List_Id := No_List;

   function Generate_LNT_Types return Node_Id is
      N : Node_Id;
   begin
      Put_Line ("Begin Types");
      Module_Node := Make_Module_Definition
       (Make_Identifier ("Types"));
      Definitions_List := New_List;
      --  Data generic type
      N := Make_Type_Def
       (Make_Identifier ("LNT_Type_Data"),
          Make_Type_Exp (
             No_Node,
             New_List (
               Make_Type_Constructor (Make_Identifier ("AADLDATA"),
                                      No_List, No_List),
               Make_Type_Constructor (Make_Identifier ("EMPTY"),
                                      No_List, No_List)),
             false, false, false, false, false, false),
        No_List,
        No_List);
      BLNu.Append_Node_To_List (N, Definitions_List);
      --  generic list of Data
      N := Make_Type_Def
       (Make_Identifier ("LNT_Type_Data_FIFO"),
          Make_Type_Exp (
             Make_Identifier ("LNT_Type_Data"),
             No_List,
             false, false, true, false, false, false),
        No_List,
        New_List (Make_Predefined_Function (K_Equality, true),
           Make_Predefined_Function (K_Length, true),
           Make_Predefined_Function (K_Append, true),
           Make_Predefined_Function (K_Tail, true),
           Make_Predefined_Function (K_Head, true),
           Make_Predefined_Function (K_Inequality, true)
        ));
      BLNu.Append_Node_To_List (N, Definitions_List);
      --  generic channel
      N := Make_Channel
        (Make_Identifier ("LNT_Channel_Port"),
         New_List (
         Make_Gate_Profile (New_List (
           Make_Identifier ("LNT_Type_Data")))));
      BLNu.Append_Node_To_List (N, Definitions_List);

      Add_LNT_Types (Definitions_List, Thread_Number, Hyperperiod);
      Add_LNT_Channels (Definitions_List);
      Add_LNT_Functions (Definitions_List, Thread_Number, Hyperperiod);

      Modules_List := New_List;
      Predefined_Functions_List := New_List;
      --  "<", ">", "==", "<=", ">="
      BLNu.Append_Node_To_List (Make_Predefined_Function (K_Equality, true),
           Predefined_Functions_List);
      BLNu.Append_Node_To_List (Make_Predefined_Function (K_Less_Than, true),
           Predefined_Functions_List);
      BLNu.Append_Node_To_List (
           Make_Predefined_Function (K_Less_Than_Or_Equal_To, true),
           Predefined_Functions_List);
      BLNu.Append_Node_To_List (
           Make_Predefined_Function (K_Greater_Than, true),
           Predefined_Functions_List);
      BLNu.Append_Node_To_List (
           Make_Predefined_Function (K_Greater_Than_Or_Equal_To, true),
           Predefined_Functions_List);

      Set_Definitions (Module_Node, Definitions_List);
      Set_Modules (Module_Node, Modules_List);
      Set_Predefined_Functions (Module_Node, Predefined_Functions_List);
      return Module_Node;
   end Generate_LNT_Types;

end Ocarina.Backends.LNT.Tree_Generator_Types;
