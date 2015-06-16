------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . B E _ A A D L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with GNAT.Command_Line; use GNAT.Command_Line;

with Ocarina.Namet;  use Ocarina.Namet;
with Ocarina.Output; use Ocarina.Output;
with Utils;  use Utils;

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Analyzer.AADL.Queries;
with Ocarina.ME_AADL.AADL_Tree.Debug;
with Ocarina.Instances;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.Options;  use Ocarina.Options;
with Ocarina.Backends; use Ocarina.Backends;

with Ocarina.Backends.Messages;
with Ocarina.BE_AADL.Namespaces;
with Ocarina.BE_AADL.Components;
with Ocarina.BE_AADL.Identifiers;

package body Ocarina.BE_AADL is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   use Ocarina.Backends.Messages;
   use Ocarina.ME_AADL.AADL_Tree.Debug;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.Instances;
   use Ocarina.Analyzer.AADL.Queries;
   use Ocarina.BE_AADL.Namespaces;
   use Ocarina.BE_AADL.Components;
   use Ocarina.BE_AADL.Identifiers;

   procedure Generate_AADL_Model (Node : Node_Id);
   --  Prints all the AADL source corresponding to the subtree having
   --  Node as a root. If given the absolute root of an AADL syntactic
   --  tree, prints all the parsed AADL source.

   procedure Generate_Min_AADL_Model (Node : Node_Id);
   --  Instantiates the AADL tree, then for each node of the root
   --  system, prints the minimal amount of AADL source necessary to
   --  model this node. If an output directory is set in the output
   --  options, then it creates an AADL file per node inside the
   --  specified aoutput directory. Otherwise, prints on the standard
   --  output.

   procedure Generate_AADL_Annex (Node : Node_Id);
   --  Prints all the AADL source corresponding to the subtree having
   --  Node as a root.

   Entity_Name : Name_Id := No_Name;
   --  Name of the entity to print using the AADL printer

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Register_Backend ("aadl", Generate_AADL_Model'Access, AADL);
      Register_Backend ("aadl_min", Generate_Min_AADL_Model'Access, AADL_Min);
      Register_Backend ("aadl_annex", Generate_AADL_Annex'Access, AADL_Annex);

      Initialize_Option_Scan;
      loop
         case Getopt ("* e:") is
            when ASCII.NUL =>
               exit;

            when 'e' =>
               if Full_Switch = "e" then
                  declare
                     D : constant String := Parameter;
                  begin
                     if D'Length /= 0 then
                        Entity_Name := To_Lower (Get_String_Name (D));
                     end if;
                  end;
               end if;
            when others =>
               null;
         end case;
      end loop;
   exception
      when others =>
         null;
   end Init;

   ---------------------------
   -- Print_Item_Refined_To --
   ---------------------------

   procedure Print_Item_Refined_To (Node : Node_Id) is
   begin
      Print_Identifier (Identifier (Node));
      Write_Space;

      Print_Token (T_Colon);

      if Is_Refinement (Node) then
         Write_Space;
         Print_Tokens ((T_Refined, T_To));
      end if;
   end Print_Item_Refined_To;

   -------------------------------
   -- Print_Constrained_Subtree --
   -------------------------------

   procedure Print_Constrained_Subtree
     (Node      : Node_Id;
      Criterion : Node_Id := No_Node)
   is
      pragma Assert (Present (Node));

      --  Some internal procedures

      procedure Internal_Print_AADL_Specification is
         new Print_Constrained_AADL_Specification
        (Is_Printable);
      procedure Internal_Print_Package is new Print_Constrained_Package
        (Is_Printable);
      procedure Internal_Print_Property_Set is
         new Print_Constrained_Property_Set (Is_Printable);

   begin
      case Kind (Node) is
         when K_AADL_Specification =>
            Internal_Print_AADL_Specification (Node, Criterion);

         when K_Package_Specification =>
            Internal_Print_Package (Node, Criterion);

         when K_Component_Type =>
            if Is_Printable (Node, Criterion) then
               Print_Component_Type (Node);
            end if;

         when K_Component_Implementation =>
            if Is_Printable (Node, Criterion) then
               Print_Component_Implementation (Node);
            end if;

         when K_Feature_Group_Type =>
            if Is_Printable (Node, Criterion) then
               Print_Feature_Group_Type (Node);
            end if;

         when K_Property_Set =>
            Internal_Print_Property_Set (Node, Criterion);

         when others =>
            Node_Not_Handled (Node);
            --  This case should not happen
      end case;
   end Print_Constrained_Subtree;

   -------------------------
   -- Generate_AADL_Model --
   -------------------------

   procedure Generate_AADL_Model (Node : Node_Id) is
      pragma Assert (Present (Node));

      procedure Internal_Print_Subtree is new Print_Constrained_Subtree
        (Always_Printable);

   begin
      if Output_Filename /= No_Name then
         Set_Output (Create_File (Get_Name_String (Output_Filename), Binary));
      end if;

      Internal_Print_Subtree (Node, No_Node);

      Set_Standard_Error;
   end Generate_AADL_Model;

   -------------------------
   -- Generate_AADL_Annex --
   -------------------------

   procedure Generate_AADL_Annex (Node : Node_Id) is
      pragma Assert (Present (Node));

      procedure Internal_Print_Subtree is new Print_Constrained_Subtree
        (Always_Printable);

   begin
      Internal_Print_Subtree (Node, No_Node);
   end Generate_AADL_Annex;

   --------------------------
   -- Print_None_Statement --
   --------------------------

   procedure Print_None_Statement is
   begin
      Write_Indentation;
      Print_Token (T_None);
      Print_Token (T_Semicolon);
   end Print_None_Statement;

   -----------------
   -- Print_Token --
   -----------------

   procedure Print_Token (Token : Ocarina.ME_AADL.Tokens.Token_Type) is
   begin
      Write_Str (Image (Token));
   end Print_Token;

   ------------------
   -- Print_Tokens --
   ------------------

   procedure Print_Tokens (Tokens : Ocarina.ME_AADL.Tokens.Token_List_Type) is
   begin
      for Index in Tokens'Range loop
         Print_Token (Tokens (Index));
         if Index < Tokens'Last then
            Write_Space;
         end if;
      end loop;
   end Print_Tokens;

   ----------------------
   -- Node_Not_Handled --
   ----------------------

   procedure Node_Not_Handled (Node : Node_Id) is
      pragma Assert (Node /= No_Node);
   begin
      W_Str ("*** This node is not handled by the AADL printer: ");
      W_Node_Header (Node);

      raise Program_Error;
   end Node_Not_Handled;

   ----------------------
   -- Always_Printable --
   ----------------------

   function Always_Printable
     (Node      : Node_Id;
      Criterion : Node_Id) return Boolean
   is
      pragma Unreferenced (Criterion);
   begin
      if Entity_Name = No_Name then
         return True;
      end if;

      if
        (Kind (Node) = K_Named_AADL_Entity
         or else Kind (Node) = K_AADL_Declaration
         or else Kind (Node) = K_Entity_Reference
         or else Kind (Node) = K_Package_Specification
         or else Kind (Node) = K_Import_Declaration
         or else Kind (Node) = K_Alias_Declaration
         or else Kind (Node) = K_Component_Type
         or else Kind (Node) = K_Component_Implementation
         or else Kind (Node) = K_Contained_Entity
         or else Kind (Node) = K_Subclause
         or else Kind (Node) = K_Prototype
         or else Kind (Node) = K_Binding_Prototype
         or else Kind (Node) = K_Feature
         or else Kind (Node) = K_Refinable_Feature
         or else Kind (Node) = K_Port_Spec
         or else Kind (Node) = K_Feature_Group_Spec
         or else Kind (Node) = K_Subprogram_Spec
         or else Kind (Node) = K_Parameter
         or else Kind (Node) = K_Subcomponent_Access
         or else Kind (Node) = K_Flow_Spec
         or else Kind (Node) = K_Mode
         or else Kind (Node) = K_Mode_Transition_Trigger
         or else Kind (Node) = K_Flow_Implementation
         or else Kind (Node) = K_End_To_End_Flow_Spec
         or else Kind (Node) = K_Flow_Implementation_Refinement
         or else Kind (Node) = K_End_To_End_Flow_Refinement
         or else Kind (Node) = K_Subprogram_Call
         or else Kind (Node) = K_Subprogram_Call_Sequence
         or else Kind (Node) = K_Subcomponent
         or else Kind (Node) = K_Feature_Group_Type
         or else Kind (Node) = K_Connection
         or else Kind (Node) = K_Property_Set
         or else Kind (Node) = K_Contained_Element_Path
         or else Kind (Node) = K_Property_Type_Declaration
         or else Kind (Node) = K_Constant_Property_Declaration
         or else Kind (Node) = K_Property_Definition_Declaration
         or else Kind (Node) = K_Property_Association
         or else Kind (Node) = K_Named_Element
         or else Kind (Node) = K_Property_Term
         or else Kind (Node) = K_Enumeration_Term
         or else Kind (Node) = K_Unit_Term
         or else Kind (Node) = K_Component_Classifier_Term
         or else Kind (Node) = K_Record_Term_Element
         or else Kind (Node) = K_Computed_Term
         or else Kind (Node) = K_Unit_Definition
         or else Kind (Node) = K_Classifier_Category_Ref
         or else Kind (Node) = K_Referable_Element_Category
         or else Kind (Node) = K_Reference_Category
         or else Kind (Node) = K_Record_Type_Element
         or else Kind (Node) = K_Unique_Property_Type_Identifier
         or else Kind (Node) = K_Unique_Property_Const_Identifier
         or else Kind (Node) = K_Annex_Subclause
         or else Kind (Node) = K_Annex_Library
         or else Kind (Node) = K_Annex_Path
         or else Kind (Node) = K_Array_Selection)
      then
         return Name (Identifier (Node)) = Entity_Name;
      else
         if Kind (Node) = K_Name_Visibility_Declaration then
            return False;
         else
            return True;
         end if;
      end if;
   end Always_Printable;

   -----------------------------
   -- Generate_Min_AADL_Model --
   -----------------------------

   procedure Generate_Min_AADL_Model (Node : Node_Id) is
      procedure Internal_Print_Minimal_Tree is new Print_Constrained_Subtree
        (Needed_By);
      --  This procedure prints for the AADL source corresponding to
      --  components or properties that are needed by its given
      --  criterion.

      Instance_Root : constant Node_Id := Instantiate_Model (Node);
      RS            : Node_Id;
   begin
      if No (Instance_Root) then
         Display_Error ("Cannot instantiate the AADL model", Fatal => True);
      end if;

      if Output_Filename /= No_Name then
         Set_Output (Create_File (Get_Name_String (Output_Filename), Binary));
      end if;

      --  Get the root system of the architecture instance

      RS := AIN.Root_System (Instance_Root);
      if AINU.Is_Empty (AIN.Subcomponents (RS)) then
         return;
      end if;

      Internal_Print_Minimal_Tree (Node, AIN.Corresponding_Declaration (RS));

      Set_Standard_Error;
   end Generate_Min_AADL_Model;

end Ocarina.BE_AADL;
