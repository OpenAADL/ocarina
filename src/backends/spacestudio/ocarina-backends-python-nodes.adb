------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . B A C K E N D S . P Y T H O N . N O D E S         --
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

pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.Backends.python.Debug; use Ocarina.Backends.python.Debug;

package body Ocarina.Backends.python.Nodes is

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
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Node_Id
         or else Table (Types.Node_Id (N)).Kind = K_Definition
         or else Table (Types.Node_Id (N)).Kind = K_python_Comment
         or else Table (Types.Node_Id (N)).Kind = K_Literal
         or else Table (Types.Node_Id (N)).Kind = K_Assignement
         or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
         or else Table (Types.Node_Id (N)).Kind = K_python_File
         or else Table (Types.Node_Id (N)).Kind = K_python_Node
         or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
         or else Table (Types.Node_Id (N)).Kind = K_HI_Node
         or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
         or else Table (Types.Node_Id (N)).Kind = K_Container
         or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Node_Id
         or else Table (Types.Node_Id (N)).Kind = K_Definition
         or else Table (Types.Node_Id (N)).Kind = K_python_Comment
         or else Table (Types.Node_Id (N)).Kind = K_Literal
         or else Table (Types.Node_Id (N)).Kind = K_Assignement
         or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
         or else Table (Types.Node_Id (N)).Kind = K_python_File
         or else Table (Types.Node_Id (N)).Kind = K_python_Node
         or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
         or else Table (Types.Node_Id (N)).Kind = K_HI_Node
         or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
         or else Table (Types.Node_Id (N)).Kind = K_Container
         or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Next_Node;

   function Frontend_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Node_Id
         or else Table (Types.Node_Id (N)).Kind = K_Definition
         or else Table (Types.Node_Id (N)).Kind = K_python_Comment
         or else Table (Types.Node_Id (N)).Kind = K_Literal
         or else Table (Types.Node_Id (N)).Kind = K_Assignement
         or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
         or else Table (Types.Node_Id (N)).Kind = K_python_File
         or else Table (Types.Node_Id (N)).Kind = K_python_Node
         or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
         or else Table (Types.Node_Id (N)).Kind = K_HI_Node
         or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
         or else Table (Types.Node_Id (N)).Kind = K_Container
         or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Frontend_Node;

   procedure Set_Frontend_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Node_Id
         or else Table (Types.Node_Id (N)).Kind = K_Definition
         or else Table (Types.Node_Id (N)).Kind = K_python_Comment
         or else Table (Types.Node_Id (N)).Kind = K_Literal
         or else Table (Types.Node_Id (N)).Kind = K_Assignement
         or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
         or else Table (Types.Node_Id (N)).Kind = K_python_File
         or else Table (Types.Node_Id (N)).Kind = K_python_Node
         or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
         or else Table (Types.Node_Id (N)).Kind = K_HI_Node
         or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
         or else Table (Types.Node_Id (N)).Kind = K_Container
         or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Frontend_Node;

   function Defining_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Definition
         or else Table (Types.Node_Id (N)).Kind = K_python_Comment
         or else Table (Types.Node_Id (N)).Kind = K_python_File
         or else Table (Types.Node_Id (N)).Kind = K_python_Node);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Defining_Identifier;

   procedure Set_Defining_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Definition
         or else Table (Types.Node_Id (N)).Kind = K_python_Comment
         or else Table (Types.Node_Id (N)).Kind = K_python_File
         or else Table (Types.Node_Id (N)).Kind = K_python_Node);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Defining_Identifier;

   function Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Value;

   procedure Set_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Value;

   function Left_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Assignement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Left_Expression;

   procedure Set_Left_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Assignement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Left_Expression;

   function Right_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Assignement);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Right_Expression;

   procedure Set_Right_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Assignement);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Right_Expression;

   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;

   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
         or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
         or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
         or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
         or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Name;

   function Corresponding_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Corresponding_Node;

   procedure Set_Corresponding_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Corresponding_Node;

   function Compile_Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Compile_Unit;

   procedure Set_Compile_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Compile_Unit;

   function Distributed_Application_Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_File);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Distributed_Application_Unit;

   procedure Set_Distributed_Application_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_File);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Distributed_Application_Unit;

   function Root_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_File);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Root_Node;

   procedure Set_Root_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_File);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Root_Node;

   function Is_HTML (N : Node_Id) return Boolean is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_File);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_HTML;

   procedure Set_Is_HTML (N : Node_Id; V : Boolean) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_File);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_HTML;

   function python_DTD (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_File);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end python_DTD;

   procedure Set_python_DTD (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_File);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_python_DTD;

   function Items (N : Node_Id) return List_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_Node);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Items;

   procedure Set_Items (N : Node_Id; V : List_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_Node);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Items;

   function Subitems (N : Node_Id) return List_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_Node);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Subitems;

   procedure Set_Subitems (N : Node_Id; V : List_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_Node);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Subitems;

   function Node_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_Node);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Node_Value;

   procedure Set_Node_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_python_Node);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Node_Value;

   function Units (N : Node_Id) return List_Id is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
         or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Units;

   procedure Set_Units (N : Node_Id; V : List_Id) is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
         or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Units;

   function HI_Nodes (N : Node_Id) return List_Id is
   begin
      pragma Assert
        (False
         or else
           Table (Types.Node_Id (N)).Kind =
           K_HI_Distributed_Application);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end HI_Nodes;

   procedure Set_HI_Nodes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert
        (False
         or else
           Table (Types.Node_Id (N)).Kind =
           K_HI_Distributed_Application);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_HI_Nodes;

   function Distributed_Application (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Distributed_Application;

   procedure Set_Distributed_Application (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Distributed_Application;

   function Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Entity;

   procedure Set_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Entity;

   function python_File (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end python_File;

   procedure Set_python_File (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_python_File;

   function Image (N : Base_Type) return Name_Id is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Base_Type
         or else Table (Types.Node_Id (N)).Kind = K_String
         or else Table (Types.Node_Id (N)).Kind = K_Numeric
         or else Table (Types.Node_Id (N)).Kind = K_Float);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Image;

   procedure Set_Image (N : Base_Type; V : Name_Id) is
   begin
      pragma Assert
        (False
         or else Table (Types.Node_Id (N)).Kind = K_Base_Type
         or else Table (Types.Node_Id (N)).Kind = K_String
         or else Table (Types.Node_Id (N)).Kind = K_Numeric
         or else Table (Types.Node_Id (N)).Kind = K_Float);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Image;

   function Content (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Container);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Content;

   procedure Set_Content (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_Container);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Content;

   function Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Unit;

   procedure Set_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Unit;

   function Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Node;

   procedure Set_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Node;

   function Processes (N : Node_Id) return List_Id is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Processes;

   procedure Set_Processes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert
        (False or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Processes;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Definition =>
            W_Definition (Node_Id (N));
         when K_python_Comment =>
            W_python_Comment (Node_Id (N));
         when K_Literal =>
            W_Literal (Node_Id (N));
         when K_Assignement =>
            W_Assignement (Node_Id (N));
         when K_Defining_Identifier =>
            W_Defining_Identifier (Node_Id (N));
         when K_python_File =>
            W_python_File (Node_Id (N));
         when K_python_Node =>
            W_python_Node (Node_Id (N));
         when K_HI_Distributed_Application =>
            W_HI_Distributed_Application (Node_Id (N));
         when K_HI_Node =>
            W_HI_Node (Node_Id (N));
         when K_HI_Unit =>
            W_HI_Unit (Node_Id (N));
         when K_Container =>
            W_Container (Node_Id (N));
         when K_String =>
            W_String (Base_Type (N));
         when K_Numeric =>
            W_Numeric (Base_Type (N));
         when K_Float =>
            W_Float (Base_Type (N));
         when K_HI_Tree_Bindings =>
            W_HI_Tree_Bindings (Node_Id (N));
         when others =>
            null;
      end case;
   end W_Node;

   procedure W_Definition (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
   end W_Definition;

   procedure W_python_Comment (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
   end W_python_Comment;

   procedure W_Literal (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute ("Value", "Value_Id", Image (Value (N)));
   end W_Literal;

   procedure W_Assignement (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Left_Expression",
         "Node_Id",
         Image (Left_Expression (N)),
         Int (Left_Expression (N)));
      W_Node_Attribute
        ("Right_Expression",
         "Node_Id",
         Image (Right_Expression (N)),
         Int (Right_Expression (N)));
   end W_Assignement;

   procedure W_Defining_Identifier (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute ("Name", "Name_Id", Image (Name (N)));
      W_Node_Attribute
        ("Corresponding_Node",
         "Node_Id",
         Image (Corresponding_Node (N)),
         Int (Corresponding_Node (N)));
      W_Node_Attribute
        ("Compile_Unit",
         "Node_Id",
         Image (Compile_Unit (N)),
         Int (Compile_Unit (N)));
   end W_Defining_Identifier;

   procedure W_python_File (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Distributed_Application_Unit",
         "Node_Id",
         Image (Distributed_Application_Unit (N)),
         Int (Distributed_Application_Unit (N)));
      W_Node_Attribute
        ("Root_Node",
         "Node_Id",
         Image (Root_Node (N)),
         Int (Root_Node (N)));
      W_Node_Attribute ("Is_HTML", "Boolean", Image (Is_HTML (N)));
      W_Node_Attribute
        ("python_DTD",
         "Node_Id",
         Image (python_DTD (N)),
         Int (python_DTD (N)));
   end W_python_File;

   procedure W_python_Node (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Items",
         "List_Id",
         Image (Items (N)),
         Int (Items (N)));
      W_Node_Attribute
        ("Subitems",
         "List_Id",
         Image (Subitems (N)),
         Int (Subitems (N)));
      W_Node_Attribute
        ("Node_Value",
         "Node_Id",
         Image (Node_Value (N)),
         Int (Node_Value (N)));
   end W_python_Node;

   procedure W_HI_Distributed_Application (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute ("Name", "Name_Id", Image (Name (N)));
      W_Node_Attribute
        ("Units",
         "List_Id",
         Image (Units (N)),
         Int (Units (N)));
      W_Node_Attribute
        ("HI_Nodes",
         "List_Id",
         Image (HI_Nodes (N)),
         Int (HI_Nodes (N)));
   end W_HI_Distributed_Application;

   procedure W_HI_Node (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute ("Name", "Name_Id", Image (Name (N)));
      W_Node_Attribute
        ("Units",
         "List_Id",
         Image (Units (N)),
         Int (Units (N)));
      W_Node_Attribute
        ("Distributed_Application",
         "Node_Id",
         Image (Distributed_Application (N)),
         Int (Distributed_Application (N)));
   end W_HI_Node;

   procedure W_HI_Unit (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("python_File",
         "Node_Id",
         Image (python_File (N)),
         Int (python_File (N)));
   end W_HI_Unit;

   procedure W_Container (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Content",
         "Node_Id",
         Image (Content (N)),
         Int (Content (N)));
   end W_Container;

   procedure W_String (N : Base_Type) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute ("Image", "Name_Id", Image (Image (N)));
   end W_String;

   procedure W_Numeric (N : Base_Type) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute ("Image", "Name_Id", Image (Image (N)));
   end W_Numeric;

   procedure W_Float (N : Base_Type) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute ("Image", "Name_Id", Image (Image (N)));
   end W_Float;

   procedure W_HI_Tree_Bindings (N : Node_Id) is
   begin
      W_Node_Header (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute ("Unit", "Node_Id", Image (Unit (N)), Int (Unit (N)));
      W_Node_Attribute ("Node", "Node_Id", Image (Node (N)), Int (Node (N)));
      W_Node_Attribute
        ("Processes",
         "List_Id",
         Image (Processes (N)),
         Int (Processes (N)));
   end W_HI_Tree_Bindings;

end Ocarina.Backends.python.Nodes;
