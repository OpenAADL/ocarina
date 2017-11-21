------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                               P A R S E R                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Locations;
with Types;

package Parser is

   procedure Main;

private

   type Node_Kind is
     (K_Boolean,
      K_Octet,
      K_Long,
      K_Interface_Declaration,
      K_Typedef,
      K_Attribute,
      K_None);

   type Node_Type is record
      Kind         : Node_Kind;
      Loc          : Locations.Location;
      Identifier   : Types.Name_Id;
      Type_Spec    : Types.Node_Id;
      Scope_Entity : Types.Node_Id;
      First_Entity : Types.Node_Id;
      Last_Entity  : Types.Node_Id;
      Next_Entity  : Types.Node_Id;
   end record;

   Default_Node : constant Node_Type :=
     Node_Type'
       (K_None,
        Locations.No_Location,
        Types.No_Name,
        Types.No_Node,
        Types.No_Node,
        Types.No_Node,
        Types.No_Node,
        Types.No_Node);

   --------------------------
   -- Node tree facilities --
   --------------------------

   function New_Node
     (Kind : Node_Kind;
      Loc  : Locations.Location) return Types.Node_Id;

   function Kind (N : Types.Node_Id) return Node_Kind;
   function Loc (N : Types.Node_Id) return Locations.Location;

   function First_Entity (N : Types.Node_Id) return Types.Node_Id;
   procedure Set_First_Entity (N : Types.Node_Id; V : Types.Node_Id);

   function Identifier (N : Types.Node_Id) return Types.Name_Id;
   procedure Set_Identifier (N : Types.Node_Id; V : Types.Name_Id);

   function Last_Entity (N : Types.Node_Id) return Types.Node_Id;
   procedure Set_Last_Entity (N : Types.Node_Id; V : Types.Node_Id);

   function Next_Entity (N : Types.Node_Id) return Types.Node_Id;
   procedure Set_Next_Entity (N : Types.Node_Id; V : Types.Node_Id);

   function Scope_Entity (N : Types.Node_Id) return Types.Node_Id;
   procedure Set_Scope_Entity (N : Types.Node_Id; V : Types.Node_Id);

   function Type_Spec (N : Types.Node_Id) return Types.Node_Id;
   procedure Set_Type_Spec (N : Types.Node_Id; V : Types.Node_Id);

   -----------------------
   -- Naming facilities --
   -----------------------

   function GNS (N : Types.Name_Id) return String;
   function Quote (S : String) return String;
   function WS (S : String) return String;
   function W (S : String) return String;

   ------------------------
   -- Parsing facilities --
   ------------------------

   function P_Attribute return Types.Node_Id;
   function P_Interface return Types.Node_Id;
   function P_Definition return Types.Node_Id;
   function P_Typedef return Types.Node_Id;

   ----------------------------------
   -- Semantic analysis facilities --
   ----------------------------------

   function Base_Kind (T : Types.Node_Id) return Node_Kind;
   --  As interfaces are represented as Node_Id and as Node_Id is
   --  represented as a long integer, the base type of an interface is
   --  K_Long. For defined types, return the kind of T base type.

   function Resolve_Type (N : Types.Name_Id) return Types.Node_Id;

   function Has_Attribute (I : Types.Node_Id) return Boolean;

   procedure Add_Attribute_To_Interface
     (Attribute : Types.Node_Id;
      Intf      : Types.Node_Id);
   --  Add attribute into interface using First_Entity, Last_Entity of
   --  Interfaces and Next_Entity of Attributes.

   function Is_Attribute_In_Interface
     (Attribute : Types.Node_Id;
      Intf      : Types.Node_Id) return Boolean;
   --  Return True when interface I has at least on attribute Look for
   --  attribute through a depth exploration of the inheritance spec
   --  of interface.

   procedure Declare_Attribute (A : Types.Node_Id);
   procedure Declare_Type (N : Types.Node_Id);

   subtype Node_Array_Range is Natural range 1 .. 16;
   type Node_Array is array (Node_Array_Range) of Types.Node_Id;

   function Inheritance_Tree (I : Types.Node_Id) return Node_Array;
   --  Return the inheritance tree. The oldest ancestors are
   --  first. The interface itself is last.

   --------------------------------
   -- Code generation facilities --
   --------------------------------

   Max_Color : constant Types.Int := 127;
   subtype Color_Type is Types.Int range 0 .. Max_Color;
   No_Color : constant Color_Type := 0;

   subtype Color_Flag_Range is Color_Type;
   type Color_Flag_Array is array (Color_Flag_Range) of Boolean;

   function Are_Adjacent (A, B : Types.Node_Id) return Boolean;
   --  Return True if the attribute A and B have been marked as
   --  adjacent. The order of passed parameters is with no importance
   --  for this function.

   procedure Mark_As_Adjacent (A : Types.Node_Id; B : Types.Node_Id);
   --  Mark attributes A and B as adjacent. Adjacent attributes are
   --  attributes belonging to a same interface. Therefore, they
   --  cannot have the same color. The order of passed parameters is
   --  with no importance for this procedure.

   function Color (N : Types.Node_Id) return Color_Type;
   procedure Set_Color (N : Types.Node_Id; V : Color_Type);
   --  To allocate a slot for an attribute in a base type array, we
   --  use classical coloration algorithm. The base types are also
   --  colored to store the greatest color used for them.

   procedure Assign_Color_To_Attribute (Attribute : Types.Node_Id);
   --  Compute adjacent attributes that is attributes included in the
   --  same interfaces than Attribute. Then find a color not already
   --  assigned to these adjacent attributes.

   function Generated (N : Types.Node_Id) return Boolean;
   procedure Set_Generated (N : Types.Node_Id; B : Boolean);
   --  Set to True when we already have generated code for this node.

   -----------------------
   -- Output facilities --
   -----------------------

   procedure W_Pragma_Assert (Attribute : Types.Node_Id);
   procedure W_Attribute_Body (A : String; N : String; T : String);
   procedure W_Attribute_Spec (A : String; N : String; T : String);
   procedure W_Attribute_Body (A : Types.Node_Id);
   procedure W_Attribute_Spec (A : Types.Node_Id);
   procedure W_Indentation (N : Natural);
   procedure W_Comment_Message;
   procedure W_Package_Body;
   procedure W_Package_Spec;
   procedure W_Subprogram_Call (I : Natural; F : String; PN1 : String);
   procedure W_Subprogram_Call
     (I   : Natural;
      F   : String;
      PN1 : String;
      PN2 : String);
   procedure W_Subprogram_Call
     (I   : Natural;
      F   : String;
      PN1 : String;
      PN2 : String;
      PN3 : String);
   procedure W_Subprogram_Call
     (I   : Natural;
      F   : String;
      PN1 : String;
      PN2 : String;
      PN3 : String;
      PN4 : String);
   procedure W_Subprogram_Signature
     (I   : Natural;
      F   : String;
      PN1 : Character;
      PT1 : String;
      PN2 : Character;
      PT2 : String);
   procedure W_Subprogram_Declaration
     (I   : Natural;
      F   : String;
      PN1 : Character;
      PT1 : String;
      PN2 : Character;
      PT2 : String);
   procedure W_Subprogram_Definition
     (I   : Natural;
      F   : String;
      PN1 : Character;
      PT1 : String;
      PN2 : Character;
      PT2 : String);
   procedure W_Subprogram_Definition_End (I : Natural; F : String);
   procedure W_Table_Access (N : Character; A : String);
   procedure W_Type_Attribute (K : Node_Kind);
   procedure W_Type_Attribute (A : String; T : String);
   procedure W_With (P : String);

   procedure W_Package_Body_Python (prefix : String);
   procedure W_Attribute_Body_python (A : String);
   procedure W_Attribute_Body_python (A : Types.Node_Id);
   procedure W_Attribute_Register_python (A : String; prefix : String);
   procedure W_Attribute_Register_python (A : Types.Node_Id; prefix : String);
   procedure W_Package_Spec_Python;
   procedure W_Python_Script;
   procedure W_Attribute_Python_Script (A : String);
   procedure W_Attribute_Python_Script (A : Types.Node_Id);

   ------------------------
   -- General facilities --
   ------------------------

   procedure Usage;
   --  Usage
   --     -d     : add debugging info
   --     -O     : optimize memory print by removing procedure to dump tree
   --     -D ARG : output files in directory ARG
   --     -p     : output files on stdout

   function Copy_Str_At_End_Of_Name
     (Name : Types.Name_Id;
      Str  : String) return String;

end Parser;
