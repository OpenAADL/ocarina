------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . L N T . P R I N T E R          --
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
with Ocarina.Output;
with Outfiles;

with Ocarina.Backends.LNT.Nodes;
with Ocarina.Backends.LNT.Nutils;
with Ocarina.Options;
use Ocarina.Options;
with GNAT.OS_Lib;

use Ocarina.Namet;
use Ocarina.Output;
use Outfiles;

use Ocarina.Backends.LNT.Nodes;
use Ocarina.Backends.LNT.Nutils;

use GNAT.OS_Lib;

package body Ocarina.Backends.LNT.Printer is

   procedure Write (T : Token_Type; Need_Space : boolean := true);
   procedure Write_Line (T : Token_Type);
   procedure Print_LNT_Node_Container (N : Node_Id);
   procedure Print_LNT (N : Node_Id);
   procedure Print_LNT_Identifier (N : Node_Id);
   procedure Print_LNT_Module_Definition (N : Node_Id);
   procedure Print_LNT_Type_Def (N : Node_Id);
   procedure Print_LNT_Type_Exp (N : Node_Id);
   procedure Print_LNT_Range (N : Node_Id);
   procedure Print_LNT_Type_Constructor (N : Node_Id);
   procedure Print_LNT_Parameter_Specification (N : Node_Id);
   procedure Print_LNT_Predefined_Function (N : Node_Id);
   procedure Print_LNT_Function_Definition (N : Node_Id);
   procedure Print_LNT_Module_Pragma (N : Node_Id);

   procedure Print_LNT_Actual_Parameter (N : Node_Id);
   procedure Print_LNT_Process_Definition (N : Node_Id);
   procedure Print_LNT_Gate_Declaration (N : Node_Id);

   procedure Print_LNT_Null_Statement;
   procedure Print_LNT_Stop_Statement;
   procedure Print_LNT_Return_Statement (N : Node_Id);
   procedure Print_LNT_Assignment_Statement (N : Node_Id);
   procedure Print_LNT_Case_Statement (N : Node_Id);
   procedure Print_LNT_Case_Statement_Alternative (N : Node_Id);
   procedure Print_LNT_Select_Statement (N : Node_Id);
   procedure Print_LNT_Select_Statement_Alternative (N : Node_Id);
   procedure Print_LNT_If_Statement (N : Node_Id);
   procedure Print_LNT_Elsif_Statement (N : Node_Id);
   procedure Print_LNT_While_Statement (N : Node_Id);
   procedure Print_LNT_Loop_Statement (N : Node_Id);
   procedure Print_LNT_Array_Element_Assignment_Statement (N : Node_Id);
   procedure Print_LNT_Var_Declaration (N : Node_Id);
   procedure Print_LNT_Var_Statement (N : Node_Id);
   procedure Print_LNT_Process_Instantiation_Statement (N : Node_Id);
   procedure Print_LNT_Communication_Statement (N : Node_Id);
   procedure Print_LNT_Offer_Statement (N : Node_Id);
   procedure Print_LNT_Parallel_Composition_Statement (N : Node_Id);
   procedure Print_LNT_Interface_Synchronisation (N : Node_Id);
   procedure Print_LNT_Hide_Statement (N : Node_Id);

   procedure Print_LNT_List_Of (N : Node_Id);
   procedure Print_LNT_Parenthesized (N : Node_Id);
   procedure Print_LNT_Call (N : Node_Id);
   procedure Print_LNT_Infix_Call (N : Node_Id);

   procedure Print_LNT_Field_Selection_Expression (N : Node_Id);
   procedure Print_LNT_Field_Update_Expression (N : Node_Id);
   procedure Print_LNT_Element_Association (N : Node_Id);
   procedure Print_LNT_Array_Elt_Access_Expression (N : Node_Id);

   procedure Print_LNT_Pattern (N : Node_Id);

   procedure Print_LNT_Channel (N : Node_Id);
   procedure Print_LNT_Gate_Profile (N : Node_Id);

   function Get_File_Name (N : Node_Id) return Name_Id;

   --  Generate an LNT file name from the package node given as
   --  parameter

   -------------------
   -- Get_File_Name --
   -------------------
   function Get_File_Name (N : Node_Id) return Name_Id is
      pragma Assert (Kind (N) = K_Module_Definition);

      Module_Suffix : constant String := ".lnt";
   begin
      Get_Name_String (Name (Identifier (N)));
      Add_Str_To_Name_Buffer (Module_Suffix);
      return Name_Find;
   end Get_File_Name;

   --------------------------
   --  Print_LNT_Generated --
   --------------------------

   procedure Print_LNT_Generated (LNT_Generated : Node_Id) is
      Fd : File_Descriptor;
   begin
      if Present (LNT_Generated) then
         if Output_Filename /= No_Name and then
            Get_Name_String (Output_Filename) = "-"
         then
            Set_Standard_Output;
            Print_LNT (LNT_Generated);
         else
            Fd := Set_Output (Get_File_Name (LNT_Generated));
            Set_Space_Increment (3);
            Print_LNT (LNT_Generated);
            Close (Fd);
         end if;
      end if;
   end Print_LNT_Generated;

   ---------------
   -- Print_LNT --
   ---------------
   procedure Print_LNT (N : Node_Id) is
   begin
      case Kind (N) is

         --  Declarations
         when K_Node_Container =>
            Print_LNT_Node_Container (N);
         when K_Module_Definition =>
            Print_LNT_Module_Definition (N);
         when K_Type_Def =>
            Print_LNT_Type_Def (N);
         when K_Type_Exp =>
            Print_LNT_Type_Exp (N);
         when K_Identifier =>
            Print_LNT_Identifier (N);
         when K_Base_Type =>
            Write_Name (Image (N));
         when K_RangeLNT =>
            Print_LNT_Range (N);
         when K_Type_Constructor =>
            Print_LNT_Type_Constructor (N);
         when K_Parameter_Specification =>
            Print_LNT_Parameter_Specification (N);
         when K_Predefined_Function =>
            Print_LNT_Predefined_Function (N);
         when K_Module_Pragma =>
            Print_LNT_Module_Pragma (N);
         when K_Function_Definition =>
            Print_LNT_Function_Definition (N);
         when K_Actual_Parameter =>
            Print_LNT_Actual_Parameter (N);
         when K_Process_Definition =>
            Print_LNT_Process_Definition (N);
         when K_Gate_Declaration =>
            Print_LNT_Gate_Declaration (N);

         --  Statements
         when K_Null_Statement =>
            Print_LNT_Null_Statement;
         when K_Stop_Statement =>
            Print_LNT_Stop_Statement;
         when K_Return_Statement =>
            Print_LNT_Return_Statement (N);
         when K_Assignment_Statement =>
            Print_LNT_Assignment_Statement (N);
         when K_Case_Statement =>
            Print_LNT_Case_Statement (N);
         when K_Case_Statement_Alternative =>
            Print_LNT_Case_Statement_Alternative (N);
         when K_Select_Statement =>
            Print_LNT_Select_Statement (N);
         when K_Select_Statement_Alternative =>
            Print_LNT_Select_Statement_Alternative (N);
         when K_If_Statement =>
            Print_LNT_If_Statement (N);
         when K_Elsif_Statement =>
            Print_LNT_Elsif_Statement (N);
         when K_While_Statement =>
            Print_LNT_While_Statement (N);
         when K_Loop_Statement =>
            Print_LNT_Loop_Statement (N);
         when K_Array_Element_Assignment_Statement =>
            Print_LNT_Array_Element_Assignment_Statement (N);
         when K_Var_Declaration =>
            Print_LNT_Var_Declaration (N);
         when K_Var_Statement =>
            Print_LNT_Var_Statement (N);
         when K_Process_Instantiation_Statement =>
            Print_LNT_Process_Instantiation_Statement (N);
         when K_Communication_Statement =>
            Print_LNT_Communication_Statement (N);
         when K_Offer_Statement =>
            Print_LNT_Offer_Statement (N);
         when K_Parallel_Composition_Statement =>
            Print_LNT_Parallel_Composition_Statement (N);
         when K_Interface_Synchronisation =>
            Print_LNT_Interface_Synchronisation (N);
         when K_Hide_Statement =>
            Print_LNT_Hide_Statement (N);

         --  Expressions
         when K_Expressions | K_Patterns =>
            Print_LNT_List_Of (N);
         when K_Parenthesized_Expression | K_Parenthesized_Pattern =>
            Print_LNT_Parenthesized (N);
         when K_Function_Call_Expression | K_Constructed_Pattern =>
            Print_LNT_Call (N);
         when K_Infix_Function_Call_Expression | K_Constant_Pattern_Infixed =>
            Print_LNT_Infix_Call (N);
         when K_Field_Selection_Expression =>
            Print_LNT_Field_Selection_Expression (N);
         when K_Field_Update_Expression =>
            Print_LNT_Field_Update_Expression (N);
         when K_Element_Association =>
            Print_LNT_Element_Association (N);
         when K_Array_Elt_Access_Expression =>
            Print_LNT_Array_Elt_Access_Expression (N);

         --  Patterns
         when K_Pattern =>
            Print_LNT_Pattern (N);

         --  Channel
         when K_Channel =>
            Print_LNT_Channel (N);
         when K_Gate_Profile =>
            Print_LNT_Gate_Profile (N);

         when others =>
            null;
      end case;
   end Print_LNT;

   ---------------------------------
   -- Print_LNT_Module_Definition --
   ---------------------------------
   procedure Print_LNT_Module_Definition (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Module);
      Print_LNT (Identifier (N));
      Write_Space;
      if not Is_Empty (Modules (N)) then
         P := First_Node (Modules (N));
         Write (Tok_Left_Paren, false);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
         Write (Tok_Right_Paren);
      end if;
      if not Is_Empty (Predefined_Functions (N)) then
         P := First_Node (Predefined_Functions (N));
         Write_Eol;
         Write_Indentation;
         Write (Tok_With);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
      end if;
      Write (Tok_Is);
      if not Is_Empty (Module_Pragmas (N)) then
         P := First_Node (Module_Pragmas (N));
         loop
            Write_Eol;
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            end if;
         end loop;
      end if;
      if not Is_Empty (Definitions (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         P := First_Node (Definitions (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write_Eol;
               Write_Indentation;
            end if;
         end loop;
         Decrement_Indentation;
         Write_Eol;
         --  Write_Indentation;
      end if;
      Write (Tok_End);
      Write_Line (Tok_Module);
   end Print_LNT_Module_Definition;

   ------------------------
   -- Print_LNT_Type_Def --
   ------------------------
   procedure Print_LNT_Type_Def (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Type);
      Print_LNT (Identifier (N));
      Write_Space;
      Write (Tok_Is);
      if not Is_Empty (Type_Pragma (N)) then
         P := First_Node (Type_Pragma (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            end if;
         end loop;
      end if;
      Write_Eol;
      Increment_Indentation;
      Write_Indentation;
      Print_LNT (Type_Exp (N));
      if not Is_Empty (Predefined_Functions (N)) then
         P := First_Node (Predefined_Functions (N));
         Write_Eol;
         Write_Indentation;
         Write (Tok_With);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
      end if;
      Write_Eol;
      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_End);
      Write_Line (Tok_Type);
   end Print_LNT_Type_Def;

   ------------------------
   -- Print_LNT_Type_Def --
   ------------------------
   procedure Print_LNT_Type_Exp (N : Node_Id) is
      P : Node_Id;
   begin
      if not Is_Empty (Type_Constructors (N)) then
         P := First_Node (Type_Constructors (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
      else
         if Is_Set (N) then
            Write (Tok_Set);
         elsif Is_Sorted_Set (N) then
            Write (Tok_Sorted);
            Write (Tok_Set);
         elsif Is_List (N) then
            Write (Tok_List);
         elsif Is_Sorted_List (N) then
            Write (Tok_Sorted);
            Write (Tok_List);
         elsif Is_Array (N) then
            Write (Tok_Array);
            if Present (RangeLNT (N)) then
               Write_Str ("[");
               Print_LNT (RangeLNT (N));
               Write_Str ("]");
            end if;
         elsif Is_Range (N) then
            Write (Tok_Range);
            Print_LNT (RangeLNT (N));
         end if;
         Write (Tok_Of);
         Print_LNT (Identifier (N));
      end if;
   end Print_LNT_Type_Exp;

   ---------------------
   -- Print_LNT_Range --
   ---------------------
   procedure Print_LNT_Range (N : Node_Id) is
   begin
      Print_LNT (Low_Bound (N));
      Write_Space;
      Write (Tok_Dot_Dot);
      Print_LNT (High_Bound (N));
      Write_Space;
   end Print_LNT_Range;

   --------------------------------
   -- Print_LNT_Type_Constructor --
   --------------------------------
   procedure Print_LNT_Type_Constructor (N : Node_Id) is
      P : Node_Id;
   begin
      Print_LNT (Identifier (N));
      if not Is_Empty (Constructor_Parameters (N)) then
         P := First_Node (Constructor_Parameters (N));
         Write (Tok_Left_Paren, false);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
               Write_Eol;
            end if;
         end loop;
         Write (Tok_Right_Paren);
      end if;
      if not Is_Empty (Constructor_Pragma (N)) then
         P := First_Node (Constructor_Pragma (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            end if;
         end loop;
      end if;
   end Print_LNT_Type_Constructor;

   ---------------------------------------
   -- Print_LNT_Parameter_Specification --
   ---------------------------------------
   procedure Print_LNT_Parameter_Specification (N : Node_Id) is
   begin
      case Parameter_Mode (N) is
         when Mode_In =>
            null;
         when Mode_Out =>
            --  TODO
            Write (Tok_In);
            Write (Tok_Var);
         when Mode_Inout =>
            Write (Tok_In);
            Write (Tok_Out);
      end case;
      Print_LNT (Parameter_Var (N));
      Write (Tok_Colon);
      Print_LNT (Parameter_Type (N));
   end Print_LNT_Parameter_Specification;

   ---------------------------------------
   -- Print_LNT_Predefined_Function --
   ---------------------------------------
   procedure Print_LNT_Predefined_Function (N : Node_Id) is
      Is_With : constant boolean := Is_With_Clause (N);
      procedure L_Write (Str : String) is
      begin
         if Is_With then
            Write_Quoted_Str (Str);
         else
            Write_Str (Str);
         end if;
      end L_Write;
   begin
      case Kind (LNT_Function (N)) is
         when K_Equality =>
            L_Write ("==");
            if Is_With then
               Write (Tok_Comma);
               Write_Space;
               L_Write ("eq");
            end if;
         when K_Inequality =>
            L_Write ("<>");
            if Is_With then
               Write (Tok_Comma);
               Write_Space;
               L_Write ("ne");
               Write (Tok_Comma);
               Write_Space;
               L_Write ("!=");
            end if;
         when K_Less_Than =>
            L_Write ("<");
            if Is_With then
               Write (Tok_Comma);
               Write_Space;
               L_Write ("lt");
            end if;
         when K_Less_Than_Or_Equal_To =>
            L_Write ("<=");
            if Is_With then
               Write (Tok_Comma);
               Write_Space;
               L_Write ("le");
            end if;
         when K_Greater_Than =>
            L_Write (">");
            if Is_With then
               Write (Tok_Comma);
               Write_Space;
               L_Write ("gt");
            end if;
         when K_Greater_Than_Or_Equal_To =>
            L_Write (">=");
            if Is_With then
               Write (Tok_Comma);
               Write_Space;
               L_Write ("ge");
            end if;
         when K_Ordinal =>
            L_Write ("ord");
         when K_Value =>
            L_Write ("val");
         when K_Field_Selection =>
            L_Write ("get");
         when K_Field_Update =>
            L_Write ("update");
            --  Write (Tok_Comma);
            --  Write_Space;
            --  L_Write ("set");
         when K_Type_Cardinality =>
            L_Write ("card");
         when K_First_Element =>
            L_Write ("first");
         when K_Last_Element =>
            L_Write ("last");
         when K_And =>
            L_Write ("and");
         when K_Or =>
            L_Write ("or");
         when K_Head =>
            L_Write ("head");
         when K_Tail =>
            L_Write ("tail");
         when K_Append =>
            L_Write ("append");
         when K_Length =>
            L_Write ("length");
         when K_Reverse =>
            L_Write ("reverse");
         when others =>
            null;
      end case;
      Write_Space;
   end Print_LNT_Predefined_Function;

   -----------------------------
   -- Print_LNT_Module_Pragma --
   -----------------------------
   procedure Print_LNT_Module_Pragma (N : Node_Id) is

   begin
      case Kind (Module_Pragma_Type (N)) is
         when K_Nat_Bits =>
            Write_Str ("!nat_bits");
            Write_Space;
            Print_LNT (Number_Constant (N));
         when others =>
            null;
      end case;
   end Print_LNT_Module_Pragma;

   -----------------------------------
   -- Print_LNT_Function_Definition --
   -----------------------------------
   procedure Print_LNT_Function_Definition (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Function);
      Print_LNT (Identifier (N));
      Write_Space;
      if not Is_Empty (Function_Parameters (N)) then
         P := First_Node (Function_Parameters (N));
         Write (Tok_Left_Paren, false);
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
               Write_Eol;
               Write_Indentation;
            end if;
         end loop;
         Write (Tok_Right_Paren);
         Decrement_Indentation;
      end if;
      if Present (Function_Return_Type (N)) then
         Write (Tok_Colon);
         Print_LNT (Function_Return_Type (N));
         Write_Space;
      end if;
      if not Is_Empty (Function_Exceptions (N)) then
         P := First_Node (Function_Exceptions (N));
         Write_Eol;
         Write_Indentation;
         Write (Tok_Raises);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
      end if;
      Write_Eol;
      Write_Indentation;
      Write (Tok_Is);
      if not Is_Empty (Function_Pragma (N)) then
         P := First_Node (Function_Pragma (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            end if;
         end loop;
      end if;
      if not Is_Empty (Statements (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         P := First_Node (Statements (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Semicolon, true);
               Write_Eol;
               Write_Indentation;
            end if;
         end loop;
         Write_Eol;
         Decrement_Indentation;
         Write_Indentation;
      end if;
      Write (Tok_End);
      Write_Line (Tok_Function);
   end Print_LNT_Function_Definition;

   ------------------------------
   -- Print_LNT_Null_Statement --
   ------------------------------
   procedure Print_LNT_Null_Statement is
   begin
      Write (Tok_Null);
   end Print_LNT_Null_Statement;

   ------------------------------
   -- Print_LNT_Stop_Statement --
   ------------------------------
   procedure Print_LNT_Stop_Statement is
   begin
      Write (Tok_Stop);
   end Print_LNT_Stop_Statement;

   -------------------------------
   -- Print_LNT_Return_Statement --
   -------------------------------
   procedure Print_LNT_Return_Statement (N : Node_Id) is
   begin
      Write (Tok_Return);

      if Is_Function (N) then
         Print_LNT (Expression (N));
      end if;
   end Print_LNT_Return_Statement;

   -------------------------------------
   --  Print_LNT_Assignment_Statement --
   -------------------------------------
   procedure Print_LNT_Assignment_Statement (N : Node_Id) is
   begin
      Print_LNT (Identifier (N));
      Write_Space;
      Write (Tok_Colon_Equal);
      Print_LNT (Expression (N));
   end Print_LNT_Assignment_Statement;

   ------------------------------
   --  Print_LNT_Case_Statement -
   ------------------------------
   procedure Print_LNT_Case_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Case);
      Print_LNT (Expression (N));
      Write_Space;
      Write_Line (Tok_In);
      if not Is_Empty (Variable_Declarations (N)) then
         P := First_Node (Variable_Declarations (N));
         Write_Eol;
         Write_Indentation;
         Write (Tok_Var);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
         Write (Tok_In);
      end if;

      if not Is_Empty (Case_Statement_Alternatives (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         P := First_Node (Case_Statement_Alternatives (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write_Eol;
               Write_Indentation;
               Write (Tok_Vertical_Bar);
               Write_Eol;
               Write_Indentation;
            end if;
         end loop;
      end if;
      Decrement_Indentation;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write (Tok_Case);
   end Print_LNT_Case_Statement;

   -------------------------------------------
   --  Print_LNT_Case_Statement_Alternative --
   -------------------------------------------
   procedure Print_LNT_Case_Statement_Alternative (N : Node_Id) is
      P : Node_Id;
   begin
      if Is_Empty (Pattern_List (N)) then
         Write (Tok_Any);
      else
         P := First_Node (Pattern_List (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            Write_Space;
            Write (Tok_Vertical_Bar);
         end loop;
      end if;
      Write_Space;
      Write (Tok_Arrow);
      if not (Is_Empty (Statements (N))) then
         Increment_Indentation;
         Write_Eol;
         Write_Indentation;
         P := First_Node (Statements (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            Write_Space;
            Write (Tok_Semicolon);
            Write_Eol;
            Write_Indentation;
         end loop;
         Decrement_Indentation;
      end if;
   end Print_LNT_Case_Statement_Alternative;

   ---------------------------
   -- Print_LNT_If_Statement --
   ---------------------------
   procedure Print_LNT_If_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_If);
      Write (Tok_Left_Paren, false);
      Print_LNT (Condition (N));
      Write (Tok_Right_Paren, false);
      Write_Eol;
      Write_Indentation;
      Write (Tok_Then);
      Write_Eol;

      Increment_Indentation;
      P := First_Node (Then_Statements (N));

      loop
         Write_Indentation;
         Print_LNT (P);
         P := Next_Node (P);
         exit when No (P);
         Write (Tok_SemiColon);
         Write_Eol;
      end loop;

      Decrement_Indentation;

      --  Elsif_Statements

      if not Is_Empty (Elsif_Statements (N)) then
         P := First_Node (Elsif_Statements (N));
         loop
            Write_Eol;
            Write_Indentation;
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            --  Write (Tok_SemiColon);
         end loop;
      end if;

      --  Else_Statement can be empty

      if not Is_Empty (Else_Statements (N)) then
         Write_Eol;
         Write_Indentation;
         Write (Tok_Else);
         Write_Eol;
         Increment_Indentation;
         P := First_Node (Else_Statements (N));
         loop
            Write_Indentation;
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            Write (Tok_SemiColon);
            Write_Eol;
         end loop;
         Decrement_Indentation;
      end if;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write (Tok_If, false);
   end Print_LNT_If_Statement;

   -------------------------------
   -- Print_LNT_Elsif_Statement --
   -------------------------------
   procedure Print_LNT_Elsif_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Elsif);
      Write (Tok_Left_Paren, false);
      Print_LNT (Condition (N));
      Write (Tok_Right_Paren, false);
      Write_Eol;
      Write_Indentation;
      Write_Line (Tok_Then);
      Write_Eol;

      Increment_Indentation;
      P := First_Node (Then_Statements (N));
      loop
         Write_Indentation;
         Print_LNT (P);
         P := Next_Node (P);
         exit when No (P);
         Write (Tok_SemiColon);
         Write_Eol;
      end loop;
      Decrement_Indentation;

   end Print_LNT_Elsif_Statement;

   -----------------------------
   -- Print_LNT_While_Statement --
   -----------------------------
   procedure Print_LNT_While_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_While);
      Print_LNT (Expression (N));
      Write (Tok_Loop, false);
      Write_Eol;
      Increment_Indentation;
      P := First_Node (Statements (N));
      loop
         Write_Indentation;
         Print_LNT (P);
         P := Next_Node (P);
         exit when No (P);
         Write (Tok_SemiColon);
         Write_Eol;
      end loop;
      Decrement_Indentation;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write (Tok_Loop, false);
   end Print_LNT_While_Statement;

   -----------------------------
   -- Print_LNT_Loop_Statement --
   -----------------------------
   procedure Print_LNT_Loop_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Loop);
      Write_Eol;
      Increment_Indentation;
      P := First_Node (Statements (N));
      loop
         Write_Indentation;
         Print_LNT (P);
         P := Next_Node (P);
         exit when No (P);
         Write (Tok_SemiColon);
         Write_Eol;
      end loop;
      Decrement_Indentation;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write (Tok_Loop, false);
   end Print_LNT_Loop_Statement;

   --------------------------------
   -- Print_LNT_Actual_Parameter --
   --------------------------------
   procedure Print_LNT_Actual_Parameter (N : Node_Id) is
   begin
      if Is_Out (N) then
         Write_Str ("?");
      elsif Is_InOut (N) then
         Write_Str ("!?");
      end if;
      Print_LNT (Expression (N));
   end Print_LNT_Actual_Parameter;

   --------------------------------------------------
   -- Print_LNT_Array_Element_Assignment_Statement --
   --------------------------------------------------
   procedure Print_LNT_Array_Element_Assignment_Statement
             (N : Node_Id) is
   begin
      Print_LNT (Identifier (N));
      Write_Str ("[");
      Print_LNT (Expression_Index (N));
      Write_Str ("]");
      Write_Space;
      Write (Tok_Colon_Equal);
      Print_LNT (Expression (N));
   end Print_LNT_Array_Element_Assignment_Statement;

   -------------------------------
   -- Print_LNT_Var_Declaration --
   -------------------------------
   procedure Print_LNT_Var_Declaration (N : Node_Id) is
   begin
      Print_LNT (Identifier (N));
      Write_Space;
      Write (Tok_Colon);
      Print_LNT (Var_Type (N));
   end Print_LNT_Var_Declaration;

   -----------------------------
   -- Print_LNT_Var_Statement --
   -----------------------------
   procedure Print_LNT_Var_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Var);
      if not Is_Empty (Variable_Declarations (N)) then
         P := First_Node (Variable_Declarations (N));
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         loop
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            Write (Tok_Comma);
            Write_Eol;
            Write_Indentation;
         end loop;
         Decrement_Indentation;
         Write_Eol;
         Write_Indentation;
      end if;
      Write (Tok_In);
      Write_Eol;
      Increment_Indentation;
      P := First_Node (Statements (N));
      loop
         Write_Indentation;
         Print_LNT (P);
         P := Next_Node (P);
         exit when No (P);
         Write (Tok_SemiColon);
         Write_Eol;
      end loop;
      Decrement_Indentation;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write (Tok_Var, false);
   end Print_LNT_Var_Statement;

   ---------------------------
   -- Print_LNT_Expressions --
   ---------------------------
   procedure Print_LNT_List_Of (N : Node_Id) is
      P : Node_Id;
   begin
      Write_Str ("{");
      P := First_Node (The_List (N));
      loop
         Print_LNT (P);
         P := Next_Node (P);
         exit when No (P);
         Write (Tok_Comma);
      end loop;
      Write_Str ("}");
   end Print_LNT_List_Of;

   ----------------------------------------
   -- Print_LNT_Parenthesized_Expression --
   ----------------------------------------
   procedure Print_LNT_Parenthesized (N : Node_Id) is
   begin
      Write (Tok_Left_Paren, false);
      Print_LNT (Variable (N));
      Write (Tok_Right_Paren);
   end Print_LNT_Parenthesized;

   --------------------
   -- Print_LNT_Call --
   --------------------
   procedure Print_LNT_Call (N : Node_Id) is
      P : Node_Id;
   begin
      Print_LNT (Identifier (N));
      Write_Space;
      if not Is_Empty (Parameters (N)) then
         P := First_Node (Parameters (N));
         Write (Tok_Left_Paren, false);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
               --  Write_Eol;
               --  Write_Indentation;
            end if;
         end loop;
         Write (Tok_Right_Paren, false);
      end if;
   end Print_LNT_Call;

   --------------------------
   -- Print_LNT_Infix_Call --
   --------------------------
   procedure Print_LNT_Infix_Call (N : Node_Id) is
   begin
      if Present (Left_Part (N)) then
         Print_LNT (Left_Part (N));
         Write_Space;
      end if;
      Print_LNT (Operator (N));
      Write_Space;
      Print_LNT (Right_Part (N));
   end Print_LNT_Infix_Call;

   ------------------------------------------
   -- Print_LNT_Field_Selection_Expression --
   ------------------------------------------
   procedure Print_LNT_Field_Selection_Expression (N : Node_Id) is
   begin
      Print_LNT (Expression (N));
      Write (Tok_Dot, false);
      Print_LNT (Field (N));
   end Print_LNT_Field_Selection_Expression;

   ------------------------------------------
   -- Print_LNT_Field_Update_Expression --
   ------------------------------------------
   procedure Print_LNT_Field_Update_Expression (N : Node_Id) is
      P : Node_Id;
   begin
      Print_LNT (Expression (N));
      Write (Tok_Dot, false);
      Write_Str ("{");
      P := First_Node (Field_Association (N));
      loop
         Print_LNT (P);
         P := Next_Node (P);
         exit when No (P);
         Write (Tok_Comma);
      end loop;
      Write_Str ("}");
   end Print_LNT_Field_Update_Expression;

   -----------------------------------
   -- Print_LNT_Element_Association --
   -----------------------------------
   procedure Print_LNT_Element_Association (N : Node_Id) is
   begin
      Print_LNT (Field (N));
      Write (Tok_Arrow_Double);
      Print_LNT (Expression (N));
   end Print_LNT_Element_Association;

   -------------------------------------------
   -- Print_LNT_Array_Elt_Access_Expression --
   -------------------------------------------
   procedure Print_LNT_Array_Elt_Access_Expression (N : Node_Id) is
   begin
      Print_LNT (Expression (N));
      Write_Str ("[");
      Print_LNT (Index (N));
      Write_Str ("]");
   end Print_LNT_Array_Elt_Access_Expression;

   ------------------------
   -- Print_LNT_Pattern --
   ------------------------
   procedure Print_LNT_Pattern (N : Node_Id) is
   begin
      if Present (Sub_Pattern (N)) then
         Print_LNT (Sub_Pattern (N));
         Write_Space;
         if (Is_Of (N) and then Present (Pattern_Type (N))) then
            Write (Tok_Of);
            Print_LNT (Pattern_Type (N));
         end if;
      elsif (Is_Any (N) and then Present (Pattern_Type (N))) then
         Write (Tok_Any);
         Print_LNT (Pattern_Type (N));
      end if;
   end Print_LNT_Pattern;

   -----------------------
   -- Print_LNT_Channel --
   -----------------------
   procedure Print_LNT_Channel (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Channel);
      Print_LNT (Identifier (N));
      Write_Space;
      Write (Tok_Is);
      if not Is_Empty (Gate_Profiles (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         P := First_Node (Gate_Profiles (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
               Write_Eol;
            end if;
         end loop;
         Decrement_Indentation;
         Write_Eol;
         Write_Indentation;
      end if;
      Write (Tok_End);
      Write (Tok_Channel, false);
   end Print_LNT_Channel;

   ----------------------------
   -- Print_LNT_Gate_Profile --
   ----------------------------
   procedure Print_LNT_Gate_Profile (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Left_Paren, false);
      P := First_Node (Gate_Types (N));
      loop
         Print_LNT (P);
         P := Next_Node (P);
         exit when No (P);
         Write (Tok_Comma);
      end loop;
      Write (Tok_Right_Paren);
   end Print_LNT_Gate_Profile;

   -----------------------------------
   -- Print_LNT_Process_Definition --
   -----------------------------------
   procedure Print_LNT_Process_Definition (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Process);
      Print_LNT (Identifier (N));
      Write_Space;
      if not Is_Empty (Process_Gate_Declarations (N)) then
         Write_Str ("[");
         Write_Eol;
         Write_Indentation;
         P := First_Node (Process_Gate_Declarations (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
               Write_Eol;
               Write_indentation;
            end if;
         end loop;
         Write_Str ("]");
      end if;
      if not Is_Empty (Process_Parameters (N)) then
         Write (Tok_Left_Paren, false);
         Write_Eol;
         Write_Indentation;
         P := First_Node (Process_Parameters (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            Write (Tok_Comma, false);
            Write_Eol;
            Write_Indentation;
         end loop;
         Write (Tok_Right_Paren);
      end if;
      if not Is_Empty (Process_Exceptions (N)) then
         P := First_Node (Process_Exceptions (N));
         Write_Eol;
         Write_Indentation;
         Write (Tok_Raises);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
      end if;
      Write_Eol;
      Write_Indentation;
      Write (Tok_Is);
      if not Is_Empty (Process_Pragma (N)) then
         P := First_Node (Process_Pragma (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            end if;
         end loop;
      end if;
      if not Is_Empty (Statements (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         P := First_Node (Statements (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Semicolon);
               Write_Eol;
               Write_Indentation;
            end if;
         end loop;
         Write_Eol;
         Decrement_Indentation;
         Write_Indentation;
      end if;
      Write (Tok_End);
      Write_Line (Tok_Process);
   end Print_LNT_Process_Definition;

   --------------------------------
   -- Print_LNT_Gate_Declaration --
   --------------------------------
   procedure Print_LNT_Gate_Declaration (N : Node_Id) is
   begin
      Print_LNT (Gate (N));
      Write (Tok_Colon);
      if Is_Any (N) then
         Write (Tok_Any);
      else
         Print_LNT (Channel_Name (N));
      end if;
   end Print_LNT_Gate_Declaration;

   -----------------------------------------------
   -- Print_LNT_Process_Instantiation_Statement --
   -----------------------------------------------
   procedure Print_LNT_Process_Instantiation_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Print_LNT (Identifier (N));
      Write_Space;
      if not Is_Empty (Actual_Gates (N)) then
         P := First_Node (Actual_Gates (N));
         Write_Str ("[");
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
         Write_Str ("]");
      end if;
      if not Is_Empty (Actual_Parameters (N)) then
         Write_Space;
         P := First_Node (Actual_Parameters (N));
         Write (Tok_Left_Paren, false);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
         Write (Tok_Right_Paren, false);
      end if;
   end Print_LNT_Process_Instantiation_Statement;

   ---------------------------------------
   -- Print_LNT_Communication_Statement --
   ---------------------------------------
   procedure Print_LNT_Communication_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Print_LNT (Identifier (N));
      Write_Space;
      if not Is_Empty (Offers (N)) then
         P := First_Node (Offers (N));
         Write (Tok_Left_Paren, false);
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
         Write (Tok_Right_Paren, false);
      end if;
      if (Has_Where (N) and then Present (Expression (N))) then
         Write_Space;
         Write (Tok_Where);
         Print_LNT (Expression (N));
      end if;
   end Print_LNT_Communication_Statement;

   --------------------------------
   -- Print_LNT_Offer_Statement --
   --------------------------------
   procedure Print_LNT_Offer_Statement (N : Node_Id) is
   begin
      if Present (Expression (N)) then
         Print_LNT (Expression (N));
         Write_Space;
         Write (Tok_Arrow);
      end if;
      if Is_Input (N) then
         Write_Str ("?");
      end if;
      Print_LNT (Pattern (N));
   end Print_LNT_Offer_Statement;

   ---------------------------------
   --  Print_LNT_Select_Statement --
   ---------------------------------
   procedure Print_LNT_Select_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Select);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation;
      if not Is_Empty (Select_Statement_Alternatives (N)) then
         --  Write_Eol;
         --  Increment_Indentation;
         --  Write_Indentation;
         P := First_Node (Select_Statement_Alternatives (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write_Eol;
               Write_Indentation;
               Write_Str ("[]");
               Write_Eol;
               Write_Indentation;
            end if;
         end loop;
      end if;
      Decrement_Indentation;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write (Tok_Select);
   end Print_LNT_Select_Statement;

   -------------------------------------------
   --  Print_LNT_Select_Statement_Alternative --
   -------------------------------------------
   procedure Print_LNT_Select_Statement_Alternative (N : Node_Id) is
      P : Node_Id;
   begin
      if not (Is_Empty (Statements (N))) then

         P := First_Node (Statements (N));
         loop
            --  Write_Indentation;
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            Write (Tok_Semicolon);
            Write_Eol;
            Write_Indentation;
         end loop;
      end if;
   end Print_LNT_Select_Statement_Alternative;

   -----------------------------------------------
   --  Print_LNT_Parallel_Composition_Statement --
   -----------------------------------------------
   procedure Print_LNT_Parallel_Composition_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Par);
      if not Is_Empty (Global_Synchronisation_Gates (N)) then
         P := First_Node (Global_Synchronisation_Gates (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
         Write (Tok_In);
      end if;
      Write_Eol;
      Increment_Indentation;
      Write_Indentation;
      if not Is_Empty (Interface_Synchronisations (N)) then
         --  Write_Eol;
         --  Increment_Indentation;
         --  Write_Indentation;
         P := First_Node (Interface_Synchronisations (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write_Eol;
               Write_Indentation;
               Write_Str ("||");
               Write_Eol;
               Write_Indentation;
            end if;
         end loop;
      end if;
      Decrement_Indentation;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write (Tok_Par);
   end Print_LNT_Parallel_Composition_Statement;

   ------------------------------------------
   --  Print_LNT_Interface_Synchronisation --
   ------------------------------------------
   procedure Print_LNT_Interface_Synchronisation (N : Node_Id) is
      P : Node_Id;
   begin
      if not Is_Empty (Interface_Synchronisation_Gates (N)) then
         P := First_Node (Interface_Synchronisation_Gates (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            Write_Space;
            Write (Tok_Comma);
         end loop;
         Write_Space;
         Write (Tok_Arrow);
      end if;
      if not (Is_Empty (Statements (N))) then
         P := First_Node (Statements (N));
         loop
            Write_Eol;
            Write_Indentation;
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            Write (Tok_Semicolon);
         end loop;
      end if;
   end Print_LNT_Interface_Synchronisation;

   -------------------------------
   --  Print_LNT_Hide_Statement --
   -------------------------------
   procedure Print_LNT_Hide_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Hide);
      if not Is_Empty (Hide_Gate_Declarations (N)) then
         P := First_Node (Hide_Gate_Declarations (N));
         loop
            Print_LNT (P);
            P := Next_Node (P);
            if No (P) then
               exit;
            else
               Write (Tok_Comma);
            end if;
         end loop;
      end if;
      Write (Tok_In);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation;
      if not (Is_Empty (Statements (N))) then
         Increment_Indentation;
         Write_Eol;
         Write_Indentation;
         P := First_Node (Statements (N));
         loop
            Write_Indentation;
            Print_LNT (P);
            P := Next_Node (P);
            exit when No (P);
            Write_Space;
            Write (Tok_Semicolon);
         end loop;
      end if;
      Decrement_Indentation;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write (Tok_Hide);
   end Print_LNT_Hide_Statement;
   --------------------------
   -- Print_LNT_Identifier --
   --------------------------
   procedure Print_LNT_Identifier (N : Node_Id) is
   begin
      Write_Name (Name (N));
   end Print_LNT_Identifier;
   ------------------------------
   -- Print_LNT_Node_Container --
   ------------------------------
   procedure Print_LNT_Node_Container (N : Node_Id) is
   begin
      Print_LNT (Item (N));
   end Print_LNT_Node_Container;

   -----------
   -- Write --
   -----------
   procedure Write (T : Token_Type; Need_Space : boolean := true) is
   begin
      Write_Name (Token_Image (T));
      if Need_Space then
         Write_Space;
      end if;
   end Write;

   ----------------
   -- Write_Line --
   ----------------
   procedure Write_Line (T : Token_Type) is
   begin
      Write (T);
      Write_Eol;
      Write_Indentation;
   end Write_Line;

end Ocarina.Backends.LNT.Printer;
