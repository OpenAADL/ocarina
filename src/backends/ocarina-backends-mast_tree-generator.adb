------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.MAST_TREE.GENERATOR                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2010, European Space Agency (ESA).              --
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

with Namet;  use Namet;
with Output; use Output;
--  with Utils;  use Utils;

with GNAT.OS_Lib; use GNAT.OS_Lib;

--  with Ocarina.Backends.Utils;
with Ocarina.Backends.MAST_Tree.Nodes;
with Ocarina.Backends.MAST_Tree.Nutils;
with Ocarina.Backends.MAST_Values;
with Ocarina.Backends.Messages;

package body Ocarina.Backends.MAST_Tree.Generator is

--   use Ocarina.Backends.Utils;
   use Ocarina.Backends.MAST_Tree.Nodes;
   use Ocarina.Backends.MAST_Tree.Nutils;
   use Ocarina.Backends.MAST_Values;
   use Ocarina.Backends.Messages;

   package MTN renames Ocarina.Backends.MAST_Tree.Nodes;

   procedure Generate_Defining_Identifier (N : Node_Id);
   procedure Generate_Literal (N : Node_Id);
   procedure Generate_MAST_File (N : Node_Id);
   procedure Generate_Processing_Resource (N : Node_Id);
   procedure Generate_Scheduling_Server (N : Node_Id);
   procedure Generate_Transaction (N : Node_Id);
   procedure Generate_Event (N : Node_Id);
   procedure Generate_Event_Handler (N : Node_Id);
   procedure Generate_Operation (N : Node_Id);
   procedure Generate_Event_Timing_Requirements (N : Node_Id);
   procedure Generate_Scheduling_Server_Parameters (N : Node_Id);

   procedure Write (T : Token_Type);
   procedure Write_Line (T : Token_Type);

   function Get_File_Name (N : Node_Id) return Name_Id;
   --  Generate a file name from the package node given as parameter

   procedure Release_Output (Fd : File_Descriptor);
   --  Releases the output by closing the opened files

   function Set_Output (N : Node_Id) return File_Descriptor;
   --  Adjust the output depending on the command line options and
   --  return a file descriptor in order to be able to close it.

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (N : Node_Id) return Name_Id is
      Suffix : constant String := ".txt";
   begin
      --  The File name corresponding is the lowerd name of N

      Get_Name_String
        (Conventional_Base_Name
         (Name
          (Defining_Identifier
           (N))));

      --  Adding file suffix

      Add_Str_To_Name_Buffer (Suffix);

      return Name_Find;
   end Get_File_Name;

   ----------------
   -- Set_Output --
   ----------------

   function Set_Output (N : Node_Id) return File_Descriptor is
   begin
      if not Print_On_Stdout then
         declare
            File_Name : constant Name_Id
              := Get_File_Name (N);
            Fd : File_Descriptor;
         begin
            Get_Name_String (File_Name);

            --  Create a new file and overwrites existing file with
            --  the same name

            Fd := Create_File
               (Name_Buffer (1 .. Name_Len), Text);

            if Fd = Invalid_FD then
               raise Program_Error;
            end if;

            --  Setting the output

            Set_Output (Fd);
            return Fd;
         end;
      end if;

      return Invalid_FD;
   end Set_Output;

   --------------------
   -- Release_Output --
   --------------------

   procedure Release_Output (Fd : File_Descriptor) is
   begin
      if not Print_On_Stdout and then Fd /= Invalid_FD then
         Set_Standard_Output;
         Close (Fd);
      end if;
   end Release_Output;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is
         when K_MAST_File =>
            Generate_MAST_File (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when K_Literal =>
            Generate_Literal (N);

         when K_Event =>
            Generate_Event (N);

         when K_Event_Handler =>
            Generate_Event_Handler (N);

         when K_Processing_Resource =>
            Generate_Processing_Resource (N);

         when K_Scheduling_Server =>
            Generate_Scheduling_Server (N);

         when K_Transaction =>
            Generate_Transaction (N);

         when K_Operation =>
            Generate_Operation (N);

         when K_Event_Timing_Requirements =>
            Generate_Event_Timing_Requirements (N);

         when K_Scheduling_Server_Parameters =>
            Generate_Scheduling_Server_Parameters (N);

         when others =>
            Display_Error ("other element in generator", Fatal => False);
            null;
      end case;
   end Generate;

   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------

   procedure Generate_Defining_Identifier (N : Node_Id) is
   begin
      Write_Name (Name (N));
   end Generate_Defining_Identifier;

   -----------
   -- Write --
   -----------

   procedure Write (T : Token_Type) is
   begin
      Write_Name (Token_Image (T));
   end Write;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (T : Token_Type) is
   begin
      Write (T);
      Write_Eol;
   end Write_Line;

   ----------------------
   -- Generate_Literal --
   ----------------------

   procedure Generate_Literal (N : Node_Id) is
   begin
      Write_Str (Image (Value (N)));
   end Generate_Literal;

   -----------------------
   -- Generate_MAST_File --
   -----------------------

   procedure Generate_MAST_File (N : Node_Id) is
      Fd : File_Descriptor;
      F : Node_Id;
   begin
      if No (N) then
         return;
      end if;
      Fd := Set_Output (N);
      if not Is_Empty (Declarations (N)) then
         F := First_Node (Declarations (N));
         while Present (F) loop
            Generate (F);
            F := Next_Node (F);
         end loop;
      end if;

      Release_Output (Fd);
   end Generate_MAST_File;

   ----------------------------------
   -- Generate_Processing_Resource --
   ----------------------------------

   procedure Generate_Processing_Resource (N : Node_Id) is
   begin
      Write_Line ("Processing_Resource (");
      Increment_Indentation;

      Write_Indentation (-1);
      if Regular_Processor (N) then
         Write_Line ("Type => Regular_Processor,");
      elsif Fixed_Priority_Processor (N) then
         Write_Line ("Type => Fixed_Priority_Processor,");
      else
         Write_Line ("Type => Packet_Based_Network,");
      end if;

      Write_Indentation (-1);
      Write_Str ("Name => ");
      Write_Name (Node_Name (N));
      Write_Line (Tok_Colon);

      Write_Indentation (-1);
      Write (Tok_Avg_ISR_Switch);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      if Avg_ISR_Switch (N) /= No_Node then
         Generate (Avg_ISR_Switch (N));
      else
         Write_Str ("0.00");
      end if;
      Write_Line (Tok_Colon);

      Write_Indentation (-1);
      Write (Tok_Best_ISR_Switch);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      if Best_ISR_Switch (N) /= No_Node then
         Generate (Best_ISR_Switch (N));
      else
         Write_Str ("0.00");
      end if;
      Write_Line (Tok_Colon);

      Write_Indentation (-1);
      Write (Tok_Worst_ISR_Switch);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      if Worst_ISR_Switch (N) /= No_Node then
         Generate (Worst_ISR_Switch (N));
      else
         Write_Str ("0.00");
      end if;

      Write_Line (");");
      Decrement_Indentation;
   end Generate_Processing_Resource;

   -------------------------------------------
   -- Generate_Scheduling_Server_Parameters --
   -------------------------------------------

   procedure Generate_Scheduling_Server_Parameters (N : Node_Id) is
   begin
      Write (Tok_Type);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      if MTN.Fixed_Priority (N) then
         Write (Tok_Fixed_Priority_Policy);
      else
         Write (Tok_Unknown);
      end if;
      Write (Tok_Colon);
      Write_Eol;

      Write_Indentation (-1);
      Write (Tok_The_Priority);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      Generate (Priority (N));
   end Generate_Scheduling_Server_Parameters;

   --------------------------------
   -- Generate_Scheduling_Server --
   --------------------------------

   procedure Generate_Scheduling_Server (N : Node_Id) is
   begin
      Write_Line ("Scheduling_Server (");
      Increment_Indentation;

      Write_Indentation (-1);
      if Is_Regular (N) then
         Write_Line ("Type => Regular,");
      else
         Write_Str ("Type => ");
         Write_Name (Associated_Scheduler (N));
         Write_Line (Tok_Colon);
      end if;

      Write_Indentation (-1);
      Write_Str ("Name => ");
      Write_Name (Node_Name (N));
      Write_Line (Tok_Colon);

      if Parameters (N) /= No_Node then
         Write_Indentation (-1);
         Write (Tok_Server_Sched_Parameters);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Write (Tok_Left_Paren);
         Increment_Indentation;
         Write_Eol;
         Write_Indentation (-1);
         Generate (Parameters (N));
         Write (Tok_Right_Paren);
         Write (Tok_Colon);
         Decrement_Indentation;
         Write_Eol;
      end if;

      Write_Indentation (-1);
      Write (Tok_Server_Processing_Resource);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      Write_Name (MTN.Server_Processing_Resource (N));
      Write_Line (");");
   end Generate_Scheduling_Server;

   --------------------------
   -- Generate_Transaction --
   --------------------------

   procedure Generate_Transaction (N : Node_Id) is
      F : Node_Id;
   begin
      Write_Line ("Transaction (");
      Increment_Indentation;

      Write_Indentation (-1);
      if Is_Regular (N) then
         Write_Line ("Type => Regular,");
      elsif Is_Sporadic (N) then
         Write_Line ("Type => Sporadic,");
      else
         Write_Line ("Type => Periodic,");
      end if;

      Write_Indentation (-1);
      Write_Str ("Name => ");
      Write_Name (Node_Name (N));
      Write (Tok_Colon);

      if not Is_Empty (External_Events (N))then
         Write_Eol;
         Write_Indentation (-1);
         Write (Tok_External_Events);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Write (Tok_Left_Paren);
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);

         F := First_Node (External_Events (N));
         while Present (F) loop
            Generate (F);
            if Present (Next_Node (F)) then
               Write_Eol;
            else

               Write (Tok_Right_Paren);
               Write_Line (Tok_Colon);
            end if;
            F := Next_Node (F);
         end loop;
         Decrement_Indentation;
      end if;

      if not Is_Empty (Internal_Events (N))then
         Write_Indentation (-1);
         Write (Tok_Internal_Events);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Write_Eol;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);

         Increment_Indentation;
         F := First_Node (Internal_Events (N));
         while Present (F) loop
            Generate (F);
            if Present (Next_Node (F)) then
               Write_Eol;
            else

               Write (Tok_Right_Paren);
               Write_Line (Tok_Colon);
            end if;
            F := Next_Node (F);
         end loop;
         Decrement_Indentation;
      end if;

      if not Is_Empty (Event_Handlers (N))then
         Write_Indentation (-1);
         Write (Tok_Event_Handlers);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Write_Line (Tok_Left_Paren);

         Increment_Indentation;
         F := First_Node (Event_Handlers (N));
         while Present (F) loop
            Write_Indentation (-1);
            Generate (F);
            if Present (Next_Node (F)) then
               Write_Eol;
            else
               Write (Tok_Right_Paren);
            end if;
            F := Next_Node (F);
         end loop;
         Decrement_Indentation;
      end if;
      Write_Line (");");
   end Generate_Transaction;

   --------------------
   -- Generate_Event --
   --------------------

   procedure Generate_Event (N : Node_Id) is
   begin
      Write (Tok_Left_Paren);
      Write (Tok_Type);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      if Is_Periodic (N) then
         Write_Str ("periodic");
      elsif Is_Sporadic (N) then
         Write_Str ("sporadic");
      else
         Write_Str ("regular");
      end if;
      Write_Line (Tok_Colon);

      Write_Indentation (-1);
      Write (Tok_Name);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      Write_Name (Node_Name (N));
      Write (Tok_Colon);

      if MTN.Period (N) /= No_Node then
         Write_Eol;
         Write_Indentation (-1);
         Write (Tok_Period);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Generate (Period (N));
      end if;

      if MTN.Min_Interarrival (N) /= No_Node then
         Write_Eol;
         Write_Indentation (-1);
         Write (Tok_Min_Interarrival);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Generate (Min_Interarrival (N));
      end if;

      if Timing_Requirements (N) /= No_Node then
         Write_Eol;
         Write_Indentation (-1);
         Write (Tok_Timing_Requirements);
         Write_Space;
         Write (Tok_Assign);
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);
         Generate (Timing_Requirements (N));
         Write (Tok_Right_Paren);
         Decrement_Indentation;
      end if;
      Write (Tok_Right_Paren);
   end Generate_Event;

   ----------------------------------------
   -- Generate_Event_Timing_Requirements --
   ----------------------------------------

   procedure Generate_Event_Timing_Requirements (N : Node_Id) is
   begin
      Write (Tok_Type);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      if Is_Hard_Deadline (N) then
         Write (Tok_Hard_Global_Deadline);
      else
         Write_Str ("unknown");
      end if;
      Write (Tok_Colon);
      Write_Eol;

      Write_Indentation (-1);
      Write (Tok_Deadline);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      Generate (Deadline (N));
      Write (Tok_Colon);
      Write_Eol;

      Write_Indentation (-1);
      Write (Tok_Referenced_Event);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      Write_Name (Referenced_Event (N));
   end Generate_Event_Timing_Requirements;

   ----------------------------
   -- Generate_Event_Handler --
   ----------------------------

   procedure Generate_Event_Handler (N : Node_Id) is
   begin
      Write (Tok_Left_Paren);
      Write (Tok_Type);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      if Is_Activity (N) then
         Write_Str ("activity");
      else
         Write_Str ("unknown");
      end if;
      Write_Line (Tok_Colon);

      Write_Indentation (-1);
      Write (Tok_Output_Event);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      Write_Name (Output_Name (N));
      Write_Line (Tok_Colon);

      Write_Indentation (-1);
      Write (Tok_Activity_Operation);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      Write_Name (Operation_Name (N));
      Write_Line (Tok_Colon);

      Write_Indentation (-1);
      Write (Tok_Activity_Server);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      Write_Name (Server_Name (N));
      Write_Line (Tok_Colon);

      Write_Indentation (-1);
      Write (Tok_Input_Event);
      Write_Space;
      Write (Tok_Assign);
      Write_Space;
      Write_Name (Input_Name (N));
      Write (Tok_Right_Paren);
   end Generate_Event_Handler;

   ------------------------
   -- Generate_Operation --
   ------------------------

   procedure Generate_Operation (N : Node_Id) is
      Op : Node_Id;
   begin
      Write_Line ("Operation (");
      Increment_Indentation;

      Write_Indentation (-1);
      if Is_Simple (N) then
         Write_Line ("Type => Simple,");
      elsif Is_Composite (N) then
         Write_Line ("Type => Composite,");
      else
         Write_Line ("Type => Enclosing,");
      end if;

      Write_Indentation (-1);
      Write_Str ("Name => ");
      Write_Name (Node_Name (N));

      if Worst_Case_Execution_Time (N) /= No_Node then
         Write (Tok_Colon);
         Write_Eol;
         Write_Indentation (-1);
         Write (Tok_Worst_Case_Execution_Time);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Generate (Worst_Case_Execution_Time (N));
      end if;

      if Avg_Case_Execution_Time (N) /= No_Node then
         Write (Tok_Colon);
         Write_Eol;
         Write_Indentation (-1);
         Write (Tok_Avg_Case_Execution_Time);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Generate (Avg_Case_Execution_Time (N));
      end if;

      if Best_Case_Execution_Time (N) /= No_Node then
         Write (Tok_Colon);
         Write_Eol;
         Write_Indentation (-1);
         Write (Tok_Best_Case_Execution_Time);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Generate (Best_Case_Execution_Time (N));
      end if;

      if not Is_Empty (Operations (N)) then
         Write (Tok_Colon);
         Write_Eol;
         Write_Indentation (-1);
         Write (Tok_Composite_Operation_List);
         Write_Space;
         Write (Tok_Assign);
         Write_Space;
         Write (Tok_Left_Paren);

         Op := First_Node (Operations (N));
         while Present (Op) loop
            Generate (Op);
            if Next_Node (Op) /= No_Node then
               Write (Tok_Colon);
            end if;
            Op := Next_Node (Op);
         end loop;

         Write (Tok_Right_Paren);
      end if;
      Write_Line (");");
   end Generate_Operation;

end Ocarina.Backends.MAST_Tree.Generator;
