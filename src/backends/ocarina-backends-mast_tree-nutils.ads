------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . M A S T _ T R E E . N U T I L S     --
--                                                                          --
--                                 S p e c                                  --
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

with Ocarina.Backends.MAST_Tree.Nodes; use Ocarina.Backends.MAST_Tree.Nodes;

package Ocarina.Backends.MAST_Tree.Nutils is

   Int0_Val             : Value_Id;
   Int1_Val             : Value_Id;

   Var_Suffix           : constant String := "_mast";
   Initialized          : Boolean  := False;

   Output_Tree_Warnings : Boolean := False;
   Output_Unit_Withing  : Boolean := False;
   --  Control flags

   type Token_Type is
     (
      --   Token name      Token type
      --   Keywords
      Tok_First_Keyword,
      Tok_Avg_Case_Execution_Time,
      Tok_Avg_Context_Switch,
      Tok_Best_Case_Execution_time,
      Tok_Best_Context_Switch,
      Tok_Host,
      Tok_List_Of_Drivers,
      Tok_Max_Blocking,
      Tok_Max_Packet_Size,
      Tok_Max_Priority,
      Tok_Min_Packet_Size,
      Tok_Min_Priority,
      Tok_Name,
      Tok_Packet_Server,
      Tok_Packet_Send_Operation,
      Tok_Packet_Receive_Operation,
      Tok_Processing_Resource,
      Tok_RTA_Overhead_Model,
      Tok_Scheduler,
      Tok_Scheduling_Server,
      Tok_Server_Sched_Parameters,
      Tok_Speed_Factor,
      Tok_Throughput,
      Tok_Type,
      Tok_Worst_Case_Execution_Time,
      Tok_Worst_Context_Switch,
      Tok_Last_Keyword,

      Tok_Assign,          -- =>
      Tok_Left_Paren,      -- (
      Tok_Right_Paren,     -- )
      Tok_Semicolon);      -- ;

   Token_Image : array (Token_Type) of Name_Id;

   subtype Keyword_Type is Token_Type
     range Tok_First_Keyword .. Tok_Last_Keyword;

   procedure Reset;

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id)
      return Name_Id;

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id)
     return Name_Id;

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id)
     return Name_Id;
   --  This function returns a new name without the suffix. If the
   --  suffix does not exist, the returned name is equal to the given
   --  name.

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   procedure Insert_After_Node (E : Node_Id; N : Node_Id);
   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id);

   procedure Push_Entity (E : Node_Id);
   procedure Pop_Entity;
   function  Current_Entity return Node_Id;
   function  Current_File return Node_Id;

   function Make_Defining_Identifier
     (Name         : Name_Id)
     return  Node_Id;

   function Copy_Node
     (N : Node_Id)
     return Node_Id;

   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return Node_Id;

   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return List_Id;

   function Image (T : Token_Type) return String;

   procedure Initialize;

   procedure New_Token (T : Token_Type; I : String := "");

   function Length (L : List_Id) return Natural;

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return True when L is empty

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node)
     return List_Id;

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id;
   --  This function executes Next_Node Num times

   function Conventional_Base_Name (N : Name_Id) return Name_Id;
   --  Return a lower case name of N

   function Make_MAST_File (Identifier : Node_Id) return Node_Id;

   function Make_Literal (Value : Value_Id) return Node_Id;

   function Make_Container (Content : Node_Id) return Node_Id;

--   function Make_Processing_Resource
--      (Resource_Name : Name_Id; Resource_Type : Name_Id) return Node_Id;

end Ocarina.Backends.MAST_Tree.Nutils;
