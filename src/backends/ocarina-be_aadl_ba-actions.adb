------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B E _ A A D L _ B A . A C T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Output;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.BE_AADL_BA.Identifiers;
with Ocarina.BE_AADL_BA.Expressions;

package body Ocarina.BE_AADL_BA.Actions is

   use Output;
   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.BE_AADL_BA.Identifiers;
   use Ocarina.BE_AADL_BA.Expressions;

   package BAN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   procedure Print_Behavior_Action           (Node         : Node_Id);
   procedure Print_Conditional_Statement     (Node         : Node_Id;
                                              Is_While     : Boolean);
   procedure Print_If_Cond_Struct            (Node         : Node_Id);
   procedure Print_For_Cond_Struct           (Node         : Node_Id);
   procedure Print_While_Cond_Struct         (Node         : Node_Id);
   procedure Print_Range                     (Node         : Node_Id);
   procedure Print_Assignment_Action         (Node         : Node_Id);
   procedure Print_Communication_Action      (Node         : Node_Id);
   procedure Print_Communication_Kind        (Comm_Kind    : Byte);
   procedure Print_Timed_Action              (Node         : Node_Id);
   procedure Print_Distribution_Kind         (Distrib_Kind : Byte);
   procedure Print_Subprogram_Parameter_List (List         : List_Id);
   procedure Print_Parameter_Label           (Node         : Node_Id);

   ----------------------------
   -- Print_Behavior_Actions --
   ----------------------------

   procedure Print_Behavior_Actions (List : List_Id) is
      pragma Assert (not Is_Empty (List));

      List_Node : Node_Id;
   begin
      Write_Eol;
      Write_Indentation (+4);

      List_Node := First_Node (List);
      Print_Behavior_Action (List_Node);

      List_Node := Next_Node (List_Node);
      while Present (List_Node) loop
         Print_Token (T_Semicolon);
         Write_Eol;
         Write_Indentation (+4);

         Print_Behavior_Action (List_Node);

         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Behavior_Actions;

   ---------------------------
   -- Print_Behavior_Action --
   ---------------------------

   procedure Print_Behavior_Action (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Behavior_Action);
      pragma Assert (Kind (Action (Node)) = K_If_Cond_Struct
                       or else Kind (Action (Node)) = K_For_Cond_Struct
                       or else Kind (Action (Node)) = K_While_Cond_Struct
                       or else Kind (Action (Node)) = K_Assignment_Action
                       or else Kind (Action (Node)) = K_Communication_Action
                       or else Kind (Action (Node)) = K_Timed_Action);

      Action_Node : constant Node_Id := Action (Node);
   begin
      case Kind (Action_Node) is
         when K_If_Cond_Struct       => Print_If_Cond_Struct    (Action_Node);
         when K_For_Cond_Struct      => Print_For_Cond_Struct   (Action_Node);
         when K_While_Cond_Struct    => Print_While_Cond_Struct (Action_Node);
         when K_Assignment_Action    => Print_Assignment_Action (Action_Node);
         when K_Communication_Action => Print_Communication_Action
                                                                (Action_Node);
         when K_Timed_Action         => Print_Timed_Action      (Action_Node);

         when others                 => Write_Line (Bug_Str);
      end case;
   end Print_Behavior_Action;

   ---------------------------------
   -- Print_Conditional_Statement --
   ---------------------------------

   procedure Print_Conditional_Statement
     (Node     : Node_Id;
      Is_While : Boolean)
   is
      pragma Assert (Kind (Node) = K_Conditional_Statement);

   begin
      if Present (Logical_Expr (Node)) then
         Print_Token (T_Left_Parenthesis);
         Print_Value_Expression (Logical_Expr (Node));
         Print_Token (T_Right_Parenthesis);
      end if;

      if Is_While then
         Print_Token (T_Left_Curly_Bracket);
         Print_Behavior_Actions (Behavior_Actions (Node));
         Print_Token (T_Right_Curly_Bracket);
      else
         Print_Behavior_Actions (Behavior_Actions (Node));
      end if;
   end Print_Conditional_Statement;

   --------------------------
   -- Print_If_Cond_Struct --
   --------------------------

   procedure Print_If_Cond_Struct (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_If_Cond_Struct);

      Is_While : constant Boolean := False;
   begin
      Write_Space;
      Print_Token (T_If);
      Write_Space;
      Print_Conditional_Statement (If_Statement (Node), Is_While);

      if Present (Elsif_Statement (Node)) then
         Write_Eol;
         Print_Conditional_Statement (Elsif_Statement (Node), Is_While);
      end if;

      if Present (Else_Statement (Node)) then
         Write_Eol;
         Print_Conditional_Statement (Else_Statement (Node), Is_While);
      end if;
   end Print_If_Cond_Struct;

   --------------------------
   -- Print_For_Cond_Struct --
   --------------------------

   procedure Print_For_Cond_Struct (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_For_Cond_Struct);

   begin
      Write_Space;
      Print_Token (T_For);
      Write_Space;

      Print_Token (T_Left_Parenthesis);
      Print_Identifier (Var_Identifier (Node));
      Write_Space;
      Print_Token (T_In);
      Write_Space;
      Print_Range (In_Range (Node));
      Print_Token (T_Right_Parenthesis);

      Write_Eol;
      Print_Token (T_Left_Curly_Bracket);
      Print_Behavior_Actions (Behavior_Actions (Node));
      Print_Token (T_Right_Curly_Bracket);
   end Print_For_Cond_Struct;

   -----------------------------
   -- Print_While_Cond_Struct --
   -----------------------------

   procedure Print_While_Cond_Struct (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_While_Cond_Struct);

      Is_While : constant Boolean := True;
   begin
      Print_Token (T_While);
      Write_Space;
      Print_Conditional_Statement (While_Statement (Node), Is_While);
   end Print_While_Cond_Struct;

   -----------------
   -- Print_Range --
   -----------------

   procedure Print_Range (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Range);
      pragma Assert (Kind (Entity (Node)) = K_Id
                       or else Kind (Entity (Node)) = K_Integer_Range
                       or else Kind (Entity (Node)) =
                                           K_Data_Component_Reference);

      Entity_Node : constant Node_Id := Entity (Node);
   begin
      case Kind (Entity_Node) is
         when K_Id =>
            Print_Id (Entity_Node);

         when K_Integer_Range =>
            Print_Integer_Range (Entity_Node);

         when K_Data_Component_Reference =>
            Print_Data_Component_Reference (Entity_Node);

         when others =>
            Write_Line (Bug_Str);
      end case;
   end Print_Range;

   -----------------------------
   -- Print_Assignment_Action --
   -----------------------------

   procedure Print_Assignment_Action (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Assignment_Action);

   begin
      Print_Id (Target (Node));
      Write_Space;
      Print_Token (T_Assignment);
      Write_Space;

      if Present (Value_Expression (Node)) then
         Print_Value_Expression (Value_Expression (Node));
      end if;

      if Is_Any (Node) then
         Print_Token (T_Any);
      end if;
   end Print_Assignment_Action;

   --------------------------------
   -- Print_Communication_Action --
   --------------------------------

   procedure Print_Communication_Action (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Communication_Action);

   begin
      Write_Space;
      Print_Id (BAN.Identifier (Node));

      Print_Communication_Kind (Comm_Kind (Node));

      if not Is_Empty (Subprogram_Parameter_List (Node)) then
         Print_Token (T_Left_Parenthesis);
         Print_Subprogram_Parameter_List (Subprogram_Parameter_List (Node));
         Print_Token (T_Right_Parenthesis);
      end if;

      if Present (Target (Node)) then
         Write_Space;
         Print_Token (T_Left_Parenthesis);
         Print_Id (Target (Node));
         Print_Token (T_Right_Parenthesis);
      end if;
   end Print_Communication_Action;

   ------------------------------
   -- Print_Communication_Kind --
   ------------------------------

   procedure Print_Communication_Kind (Comm_Kind : Byte) is
   begin
      case Communication_Kind'Val (Comm_Kind) is
         when CK_Exclamation      => Print_Token (T_Exclamation);
         when CK_Interrogative    => Print_Token (T_Interrogative);
         when CK_Greater_Greater  => Print_Token (T_Greater_Greater_Than);
         when others              => Write_Line  (Bug_Str);
      end case;
   end Print_Communication_Kind;

   ------------------------
   -- Print_Timed_Action --
   ------------------------

   procedure Print_Timed_Action (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Timed_Action);

   begin
      Write_Space;

      if Is_Computation (Node) then
         Print_Token (T_Computation);
      else
         Print_Token (T_Delay);
      end if;

      Write_Space;
      Print_Token (T_Left_Parenthesis);

      Print_Behavior_Time (Fst_Behavior_Time (Node));

      if Present (Scd_Behavior_Time (Node)) then
         Print_Token (T_Comma);
         Write_Space;
         Print_Behavior_Time (Scd_Behavior_Time (Node));
      end if;

      if Distribution_Kind'Val (Distrib_Kind (Node)) /= DK_No_Kind then
         Print_Token (T_Comma);
         Write_Space;
         Print_Distribution_Kind (Distrib_Kind (Node));
      end if;

      Print_Token (T_Right_Parenthesis);
   end Print_Timed_Action;

   -----------------------------
   -- Print_Distribution_Kind --
   -----------------------------

   procedure Print_Distribution_Kind (Distrib_Kind : Byte) is
   begin
      case Distribution_Kind'Val (Distrib_Kind) is
         when DK_Fixed   => Print_Token (T_Fixed);
         when DK_Normal  => Print_Token (T_Normal);
         when DK_Poisson => Print_Token (T_Poisson);
         when DK_Random  => Print_Token (T_Random);
         when others     => Write_Line  (Bug_Str);
      end case;
   end Print_Distribution_Kind;

   ------------------------------------
   -- Print_Suprogram_Parameter_List --
   ------------------------------------

   procedure Print_Subprogram_Parameter_List (List : List_Id) is
      pragma Assert (not Is_Empty (List));

      List_Node : Node_Id;
   begin
      List_Node := First_Node (List);
      Print_Parameter_Label (List_Node);

      List_Node := Next_Node (List_Node);
      while Present (List_Node) loop
         Print_Token (T_Comma);
         Write_Space;
         Print_Parameter_Label (List_Node);

         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Subprogram_Parameter_List;

   ---------------------------
   -- Print_Parameter_Label --
   ---------------------------

   procedure Print_Parameter_Label (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Parameter_Label);

      Param_Node : constant Node_Id := Parameter (Node);
   begin
      case Kind (Param_Node) is
         when K_Value_Expression => Print_Value_Expression (Param_Node);
         when K_Id               => Print_Id (Param_Node);
         when others             => Write_Line  (Bug_Str);
      end case;
   end Print_Parameter_Label;

   ------------------------------------
   -- Print_Data_Component_Reference --
   ------------------------------------

   procedure Print_Data_Component_Reference (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Data_Component_Reference);
      pragma Assert (not Is_Empty (BAN.Identifiers (Node)));

      List_Node : Node_Id;
   begin
      List_Node := First_Node (BAN.Identifiers (Node));
      Print_Id (List_Node);

      List_Node := Next_Node (List_Node);
      while Present (List_Node) loop
         Print_Token (T_Dot);
         Print_Id (List_Node);

         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Data_Component_Reference;

   --------------
   -- Print_Id --
   --------------

   procedure Print_Id (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Id);

   begin
      Print_Identifier (BAN.Identifier (Node));

      if Present (Int_Value_Holder (Node)) then
         Write_Space;
         Print_Token (T_Left_Square_Bracket);
         Print_Value_Holder (Node);
         Print_Token (T_Right_Square_Bracket);
      end if;
   end Print_Id;

end Ocarina.BE_AADL_BA.Actions;
