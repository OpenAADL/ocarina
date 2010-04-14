------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . X M L _ T R E E . N U T I L S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2008-2009, GET-Telecom Paris.                --
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

with Ocarina.Backends.XML_Tree.Nodes; use Ocarina.Backends.XML_Tree.Nodes;

package Ocarina.Backends.XML_Tree.Nutils is

   Int0_Val             : Value_Id;
   Int1_Val             : Value_Id;

   Var_Suffix           : constant String := "_j";
   Initialized          : Boolean  := False;

   Output_Tree_Warnings : Boolean := False;
   Output_Unit_Withing  : Boolean := False;
   --  Control flags

   type Token_Type is
     (
      --   Token name      Token type
      --   Keywords
      Tok_Null,            -- NULL   **** First Keyword
      Tok_Typedef,         -- TYPEDEF

      Tok_Slash,           -- /
      Tok_Less,            -- <
      Tok_Equal,           -- =
      Tok_Greater);        -- >

   Token_Image : array (Token_Type) of Name_Id;

   subtype Keyword_Type is Token_Type
     range Tok_Null .. Tok_Typedef;

   type Component_Id is
     (C_Address,
      C_Dispatcher,
      C_From,
      C_Los,
      C_Name,
      C_Pid,
      C_Proc_Id,
      C_Switch,
      C_Conf_Table,
      C_Priority,
      C_Operation);

   CN : array (Component_Id) of Name_Id;

   type Type_Id is
     (T_Char,
      T_Float,
      T_Int,
      T_Int8_T,
      T_Int16_T,
      T_Int32_T,
      T_Int64_T,
      T_Void,
      T_Unsigned);

   TN : array (Type_Id) of Name_Id;

   type XML_New_Node_Kind is
   (K_String,
    K_Nameid);

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

   function Make_XML_Comment
     (N                 : Name_Id)
     return Node_Id;
   --  This function does only the fllowing thing: it creates a node
   --  whose name is the full text of the comment. It does not split
   --  the comment into many lines. This is done in the code
   --  generation phase

   function Make_Defining_Identifier
     (Name         : Name_Id)
     return  Node_Id;

   --  No_Node as Type_Definition made type declaration without actual
   --  definition (eg. "type X;").

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node)
     return List_Id;

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id;
   --  This function executes Next_Node Num times

   function Message_Comment (M : Name_Id) return Node_Id;
   function Message_Comment (M : String) return Node_Id;
   --  Return a comment message. Used by all the tree
   --  converters
   function To_XML_Name (N : Name_Id) return Name_Id;
   --  Convert N to a valid Ada identifier (no clashing with keywords,
   --  no consecutive '_', no heading '_'...).

   function Conventional_Base_Name (N : Name_Id) return Name_Id;
   --  Return a lower case name of N

   function Make_XML_File (Identifier : Node_Id) return Node_Id;

   function Make_Literal (Value : Value_Id) return Node_Id;

   function Make_Container (Content : Node_Id) return Node_Id;

   function Make_XML_Node (Name_String : String := "";
                           Name_Nameid : Name_Id := No_Name;
                           Kind : XML_New_Node_Kind := K_String)
                           return Node_Id;

   function Make_Assignement (Left : Node_Id; Right : Node_Id)
                              return Node_Id;

end Ocarina.Backends.XML_Tree.Nutils;
