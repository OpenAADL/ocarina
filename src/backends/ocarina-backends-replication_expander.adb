------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.REPLICATION_EXPANDER                   --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ocarina.Namet;
with Utils;
with Ocarina.Output;
with Outfiles;
with Locations; use Locations;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ocarina.ME_AADL;
with Ocarina.BE_AADL;
with Ocarina.AADL_Values;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.Backends.Messages;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.Me_AADL.AADL_Tree.Entities.Properties;
with Ocarina.Builder.AADL.Components.Subcomponents;
with Ocarina.Builder.AADL.Components.Connections;
with Ocarina.Builder.AADL.Components.Features;
with Ocarina.Builder.AADL.Components;
with Ocarina.Builder.AADL.Components.Subprogram_Calls;
with Ocarina.Builder.AADL.Properties;

package body Ocarina.Backends.Replication_Expander is

   use Ocarina.Namet;
   use Utils;
   use Ocarina.Output;
   use Outfiles;

   use Ocarina.ME_AADL;
   use Ocarina.BE_AADL;
   use Ocarina.AADL_Values;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.Backends.Messages;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.Analyzer.AADL.Finder;
   use Ocarina.Me_AADL.AADL_Tree.Entities.Properties;
   use Ocarina.Builder.AADL.Components.Subcomponents;
   use Ocarina.Builder.AADL.Components;
   use Ocarina.Builder.AADL.Properties;
   use Ocarina.Builder.AADL.Components.Connections;
   use Ocarina.Builder.AADL.Components.Features;
   use Ocarina.Builder.AADL.Components.Subprogram_Calls;

   package ATN  renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Types.Node_Id) is

   begin
      Write_Line ("--------------------------------------------------");
      Write_Line ("------------ FT-Replication Generator ------------");
      Write_Line ("--------------------------------------------------");
      Write_Line (" ");

      Expand_With_Replicas (AADL_Root);

   end Generate;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Register_Backend ("ft_replication", Generate'Access, FT_Replication);
   end Init;

   function Is_Node_Of_Node_List (Node : Node_Id; List : Node_List)
      return boolean;
   procedure Remove_Node_From_Node_List (Node : in out Node_Id;
                                         List : in out Node_List);
   function Has_Replication_Properties (N : Node_Id) return boolean;
   function Is_Defined_Import_Declaration (N : Node_Id)
     return boolean;
   procedure Thread_Replication (T : Replication_Info);
   procedure Process_Replication (P : Replication_Info);
   procedure System_Replication (S : Replication_Info);
   procedure Device_Replication (D : Replication_Info);
   procedure Expand_Component (C : Replication_Info);
   procedure Expand_Component_Implementation (I : Replication_Info);
   procedure Extract_Properties_List (N : Node_Id);
   procedure Check_Replication_Validity (Record_Info : Replication_Info);
   function Check_Consensus_Algo_Validity
     (Replicated_Component : Node_Id; Consensus_List : Node_List)
     return boolean;
   function Add_Connection (Source           : Node_Id;
                           Destination       : Node_Id;
                           Source_Port       : Node_Id;
                           Destination_Port  : Node_Id;
                           Parent_Comp_Impl  : Node_Id;
                           Category : Ocarina.Me_AADL.Connection_Type
                                    := CT_Port_Connection)
     return Node_Id;
   --  Adding connection between source and destination
   --  situated inside the Parent_Comp_Impl component

   function Generate_Replica (Replicated_Component : Node_Id;
                             Identifiers_List      : List_Id;
                             Comp_Category         :
                             Ocarina.ME_AADL.Component_Category)
       return List_Id;
   --  Replicate the Replicated_Component component Replica_Number
   --  onces and give them the Identifiers_List list of identifiers

   function Add_Subcomponent (Loc                 : Location := No_Location;
                              Comp_Impl           : Node_Id;
                              Category : Ocarina.ME_AADL.Component_Category;
                              Is_Refinement       : Boolean;
                              In_Modes            : Node_Id := No_Node;
                              Prototypes_Bindings : List_Id := No_List;
                              Entity_Ref          : Node_Id := No_Node;
                              Identifier_Name     : Name_Id)
     return Node_Id;

   function Get_Connection (Component   : Node_Id;
                            Conn_Source : Node_Id)
     return Node_Id;
   --  return the connection of component which has as source
   --  the parameter source if exist else No_Node

   procedure Set_Property_Association_List (Pointed_Node : Node_Id;
                                           Replica_List : List_Id;
                                           Container    : Node_Id);
   --  Set the property association list to the different replica
   --  by copying the ones of the replicated component

   function Add_Component (Loc             : Location;
                           Core_Name       : Node_Id;
                           Namespace       : Node_Id;
                           Component_Type  :
                           Ocarina.ME_AADL.Component_Category;
                           Is_Private      : Boolean := False;
                           Suffix_Name     : String;
                           Implementation  : Boolean := False)
     return Node_Id;
   --  return a node_Id representing the new created component type
   --  if Implementation is false or component implementation if
   --  Implementation is true with specifiying its identifier in both cases
   --  The identifier of the new added component is the concatenation
   --  of the core_name and the suffix_name

   function Create_Call_Sequence_to_Wrapper
                  (T                : Node_Id;
                   Wrapper_Spg_Impl : Node_Id)
     return Node_Id;
   --  Create Return the subprogram call node,
   --  return no_node whenever declaration fail

   procedure Add_Wrapper_Spg (Thread_Type : Node_Id;
                              Thread_Impl : Node_Id;
                              R : Replication_Info);
   --  add the wrapper subprogram into the thread voter
   --  and then create subprograms voters calling the
   --  procedure Create_Voter_Subprograms

   function Add_Subprogram_Parameter (Spg        : Node_Id;
                                      Feature    : Node_Id;
                                      Entity_Ref : Node_Id := No_Node)
     return Node_Id;
   --  Add the Feature into the subprogram Spg

   procedure Create_Voter_Subprograms (R            : Replication_Info;
                                       Wrapper      : Node_Id;
                                       Wrapper_Impl : Node_Id);
   --  Create the voter subprograms (Map and vote) for each
   --  output of the replicated component

   procedure Generate_Consensus (Consensus_Value  : Node_Id;
                                 Consensus_Type   : Integer;
                                 Feature_Node     : Node_Id;
                                 Wrapper_Spg      : Node_Id;
                                 Wrapper_Spg_Impl : Node_Id;
                                 Replica_Number   : Integer;
                                 Call_Seq         : Node_Id);
   --  generates the mapping_Spg and Vote_Spg inside the Wrapper_Spg
   --  depending on the Consensus_Type and Consensus_Value and connect
   --  each of them to others

   procedure Add_Source_Name (Spg               : Node_Id;
                              Source_Name_Value : Node_Id);
   --  Add the property source Name to the subprogram Spg

   function Add_Spg_Call_To_Call_Seq (Call_Name : Name_Id;
                                      Spg_Comp : Node_Id;
                                      Call_Seq : Node_Id)
      return Node_Id;
   --  add a call_subprogram to a call_Sequence

   procedure Copy_Property (N1              : Node_Id;
                            N2              : Node_Id;
                            Property_Node   : Node_Id);
   --  apply the property_value attributed to the property_name
   --  to node N2 similarely to N1

   procedure Set_Actual_Processor_Binding (P1        : Node_Id;
                                           P2        : Node_Id;
                                           Container : Node_Id);
   --  Add the property Actual_Pocessor_Binding to process P2
   --  by affecting the same value of the Property_Node

   ----------------------------------
   -- Set_Actual_Processor_Binding --
   ----------------------------------

   procedure Set_Actual_Processor_Binding (P1        : Node_Id;
                                           P2        : Node_Id;
                                           Container : Node_Id)
   is
      pragma Assert (kind (P1) = kind (P2));

      Prop_Name         : constant Name_Id
                        := Get_String_Name ("Actual_Processor_Binding");
      Prop_Node, Prop_Assoc : Node_Id := No_Node;
      Properties_List   : List_Id;
      Applies_To        : List_Id;
      Applies_To_Node   : Node_Id;
      Found             : Boolean := False;
   begin
      Properties_List := ATN.Properties (Container);
      if (not ATNU.Is_Empty (Properties_List)) then
         Prop_Node := First_Node (Properties_List);
      end if;
      while (Present (Prop_Node) and then not Found) loop
         if (To_Lower (Name (Identifier (Property_Name (Prop_Node))))
             = To_Lower (Prop_Name)) and then
         (not ATNU.Is_Empty (Applies_To_Prop (Prop_Node)))
         then
            Applies_To := List_Items (First_Node
                         (Applies_To_Prop (Prop_Node)));
            Applies_To_Node := First_Node (Applies_To);
            while (Present (Applies_To_Node) and then not Found) loop
               if Corresponding_Entity (Applies_To_Node) = P1
               then
                  Prop_Assoc := Prop_Node;
                  Found := true;
               end if;
               Applies_To_Node := Next_Node (Applies_To_Node);
            end loop;
            if (Found) then
               Copy_Property (P1, P2, Prop_Assoc);
            end if;
         end if;
         Prop_Node := Next_Node (Prop_Node);
      end loop;
   end Set_Actual_Processor_Binding;

   -------------------
   -- Copy_Property --
   -------------------

   procedure Copy_Property (N1              : Node_Id;
                            N2              : Node_Id;
                            Property_Node   : Node_Id)
      is
      --  Applies_To_Node                : Node_Id;
      Id, Id2, S2, S, N                   : Node_Id;
      Applied_To, L                  : List_Id;
      --  Applies_To_List, L : List_Id;
      Prop_Name                      : Name_Id;
      Property_Value : constant Node_Id
        := Property_Association_Value (Property_Node);
      Value_Of_Association : constant Node_Id
         := New_Node (K_Property_Value, Loc (N1));
      New_Comp_Name : Name_Id;

   begin

      Prop_Name := Name (Identifier (Property_Name (Property_Node)));
      Id := ATNU.Make_Identifier
            (No_Location, Prop_Name, Prop_Name, No_Node);
      S2 := New_Node (K_Entity_Reference, No_Location);
      Set_Identifier (S2, Id);

      --  Create the property
      L := New_List (K_List_Id, No_Location);

      N := New_Node (K_Contained_Element_Path, No_Location);
      Set_List_Items (N, L);
      S := New_Node (K_Reference_Term, No_Location);
      Set_Reference_Term (S, N);
      Set_Full_Identifier (S2, Id);

      New_Comp_Name := Name (Identifier (N2));
      Applied_To := New_List (K_List_Id, No_Location);
      Append_Node_To_List
              (Make_Identifier
              (No_Location, New_Comp_Name, New_Comp_Name, No_Node),
              Applied_To);
      Id2 := New_Node (K_Contained_Element_Path, No_Location);
      Set_List_Items (Id2, Applied_To);
      Applied_To := New_List (K_List_Id, No_Location);
      Append_Node_To_List (Id2, Applied_To);

      Id := Add_New_Property_Association
            (Loc            => No_Location,
             Name           => Id,
             Property_Name  => Property_Name (Property_Node),
             Container      => Container_Component (N1),
             In_Binding     => In_Binding (Property_Node),
             In_Modes       => In_Modes (Property_Node),
             Property_Value => Single_Value (Property_Value),
             Is_Constant    => Is_Constant (Property_Node),
             Is_Access      => Is_Access (Property_Node),
             Is_Additive    => Is_Additive_Association
                               (Property_Node),
             Applies_To     => Applied_To);
      if No (Id) then
         raise Program_Error;
      end if;

      if Property_Value /= No_Node then
         if (Multi_Value (Property_Value) /= No_List) then
            --  and then kind (Multi_Value (Property_Value))
            --  = K_Property_List_Value
            Set_Single_Value (Value_Of_Association, No_Node);
            Set_Multi_Value (Value_Of_Association,
                            Multi_Value (Property_Value));
         else
            Set_Single_Value (Value_Of_Association,
                            Single_Value (Property_Value));
            Set_Multi_Value (Value_Of_Association, No_List);
         end if;
      else
         Set_Single_Value (Value_Of_Association, No_Node);
         Set_Multi_Value (Value_Of_Association, No_List);
      end if;
         Set_Property_Association_Value (Id, Value_Of_Association);

   end Copy_Property;

   --------------------------------
   -- Remove_Node_From_Node_List --
   --------------------------------

   procedure Remove_Node_From_Node_List (Node : in out Node_Id;
                                         List : in out Node_List)
     is
      pragma Assert (Present (Node));

      Done       : boolean := False;
      Current, Last : Node_Id;
   begin
      if List.First /= No_Node and then
      Is_Node_Of_Node_List (Node, List)
      then
         Current := List.First;
         if Current = Node then
            List.First := Next_Entity (Current);
            Node := No_Node;
         else
            while Present (Current) and then not (Done) loop
               Last := Current;
               Current := Next_Entity (Current);
               if Current = Node then
                  if Present (Next_Entity (Current)) then
                     List.Last := Next_Entity (Current);
                  else
                     List.Last := Last;
                  end if;
                  Node := No_Node;
                  Done := True;
               else
                  Current := Next_Entity (Current);
               end if;
            end loop;
         end if;
      end if;
   end Remove_Node_From_Node_List;

   ------------------------------
   -- Add_Spg_Call_To_Call_Seq --
   ------------------------------

   function Add_Spg_Call_To_Call_Seq (Call_Name : Name_Id;
                                      Spg_Comp : Node_Id;
                                      Call_Seq : Node_Id)
      return Node_Id
   is
      pragma Assert (kind (Spg_Comp) = K_Component_Implementation);
      pragma Assert (kind (Call_Seq) = K_Subprogram_Call_Sequence);
      Node          : Node_Id;
      Success       : Boolean := False;
      Call_Spg_Name : Node_Id;
      Path_Ref   : constant List_Id := New_List (K_List_Id, No_Location);
      C, Ref : Node_Id;
   begin
      Node := New_Node (K_Subprogram_Call, No_Location);
      Call_Spg_Name := Make_Identifier (No_Location,
                                        Call_Name,
                                        Call_Name,
                                        Node);
      Set_Identifier (Node, Call_Spg_Name);
      Ref := New_Node (K_Entity_Reference, No_Location);
      C := New_Node (K_Node_Container, No_Location);
      Set_Item (C, ATNU.Make_Identifier
                (No_Location, Name (Identifier (Spg_Comp)),
                 Display_Name (Identifier (Spg_Comp)), No_Node));
      ATNU.Append_Node_To_List (C, Path_Ref);
      Set_Path (Ref, Path_Ref);
      Set_Identifier (Ref, ATNU.Make_Identifier
                      (No_Location, Name (Identifier (Spg_Comp)),
                       Display_Name (Identifier (Spg_Comp)), No_Node));
      Set_Entity_Ref (Node, Ref);
      Success := Add_Subprogram_Call (Call_Seq, Node);
      if (not Success) then
         raise Program_Error;
      end if;
      return Node;
   end Add_Spg_Call_To_Call_Seq;

   ---------------------
   -- Add_Source_Name --
   ---------------------

   procedure Add_Source_Name (Spg               : Node_Id;
                              Source_Name_Value : Node_Id)
      is

         pragma Assert (Kind (Spg) = K_Component_Type or else
                        Kind (Spg) = K_Component_Implementation);

         Source_Name_Str : constant String := "Source_Name";
         Source_Name     : constant Name_Id
                         := Get_String_Name (Source_Name_Str);
         S, S2, N, Id    : Node_Id;
         L               : constant List_Id
                           := New_List (K_List_Id, No_Location);
   begin
      Id := ATNU.Make_Identifier
         (No_Location, Source_Name, Source_Name, No_Node);
      S2 := New_Node (K_Entity_Reference, No_Location);
      Set_Identifier (S2, Id);

      --  Create the property

      N := New_Node (K_Contained_Element_Path, No_Location);
      Set_List_Items (N, L);
      S := New_Node (K_Reference_Term, No_Location);
      Set_Reference_Term (S, N);
      Set_Full_Identifier (S2, Id);
      Id := Add_New_Property_Association
            (Loc            => No_Location,
             Name           => Id,
             Property_Name  => S2,
             Container      => Spg,
             In_Binding     => No_Node,
             In_Modes       => No_Node,
             Property_Value => Source_Name_Value,
             Is_Constant    => False,
             Is_Access      => False,
             Is_Additive    => False,
             Applies_To     => No_List);
      if No (Id) then
         raise Program_Error;
      end if;
   end Add_Source_Name;

   ------------------------
   -- Generate_Consensus --
   ------------------------

   procedure Generate_Consensus (Consensus_Value  : Node_Id;
                                 Consensus_Type   : Integer;
                                 Feature_Node     : Node_Id;
                                 Wrapper_Spg      : Node_Id;
                                 Wrapper_Spg_Impl : Node_Id;
                                 Replica_Number   : Integer;
                                 Call_seq         : Node_Id)
   is
      Map_Spg, Map_Spg_Impl   : Node_Id := No_Node;
      Vote_Spg, Vote_Spg_Impl : Node_Id := No_Node;
      Data, Data_Impl         : Node_Id := No_Node;
      Param, Conn             : Node_Id;
      Map_Call, Vote_call     : Node_Id;
      Generated_Name          : Name_Id;
      Generated_feat          : Integer;
      Feature_Name            : constant Name_Id
                              := Name (Identifier (Feature_Node));
      Voter_Param_Name        : constant Name_Id
                              := Get_String_Name ("Voter_Array");
      Voter_Param, Map_Param  : Node_Id;
      Src                     : Name_Id;
      Data_Source             : Node_Id;
      Entity                  : Node_Id;
   begin

      Data := Add_Component
              (Loc (Wrapper_Spg),
               Entity_Ref (Feature_Node),
               Namespace (Wrapper_Spg),
               CC_Data,
               False, --  Is_Private
               "_Array",
               False); --  Comp Impl

      Data_Impl := Add_Component
                   (Loc (Wrapper_Spg),
                    Data,
                    Namespace (Wrapper_Spg),
                    CC_Data,
                    False, --  Is_Private
                    ".Impl",
                    True); --  Comp Impl
      --  1. Create the component subprogram declaration Map_Spg
      Map_Spg := Add_Component
                     (Loc (Wrapper_Spg),
                      No_Node,  --  core_name
                      Namespace (Wrapper_Spg),
                      CC_Subprogram,
                      False, --  Is_Private
                      "Mapping_Spg_" & Get_Name_String (Feature_Name),
                      False); --  Comp Impl

      if No (Map_Spg) then
         raise Program_Error;
      end if;

      --  2. Create the component subprogram Impl Wrapper_Spg_Impl
      Map_Spg_Impl := Add_Component
                          (Loc (Wrapper_Spg),
                          Map_Spg,  --  core_name
                          Namespace (Wrapper_Spg),
                          CC_Subprogram,
                          False, --  Is_Private
                          ".Impl", --  suffix_name
                          True); --  Comp Impl
      if No (Map_Spg_Impl) then
         raise Program_Error;
      end if;
      --  3. create the mapping subprogram parameters
      case Consensus_Type is
         when 0 => --  Source_Text
            Vote_Spg := Add_Component
                        (Loc (Wrapper_Spg),
                        No_Node,  --  core_name
                        Namespace (Wrapper_Spg),
                        CC_Subprogram,
                        False, --  Is_Private
                        "Vote_Spg_" & Get_Name_String (Feature_Name),
                        False); --  Comp Impl

            if No (Vote_Spg) then
               raise Program_Error;
            end if;

            --  2.2 Create the component subprogram Impl Wrapper_Spg_Impl
            Vote_Spg_Impl := Add_Component
                            (Loc (Wrapper_Spg),
                            Vote_Spg,  --  core_name
                            Namespace (Wrapper_Spg),
                            CC_Subprogram,
                            False, --  Is_Private
                            ".Impl", --  suffix_name
                            True); --  Comp Impl
            if No (Vote_Spg_Impl) then
               raise Program_Error;
            end if;
            --  3. add the property source text to the vote subprogram
            --  to specify algorithm
            --  create the property
            Add_Source_Name (Vote_Spg, Single_Value (Consensus_Value));

            --  4. Create call subprogram to vote and map spgs

            ----------------------------------------------------------
            Vote_Call := Add_Spg_Call_To_Call_Seq (Add_Prefix_To_Name
                         ("Call_", Display_Name (Identifier (Vote_Spg))),
                         Vote_Spg_Impl,
                         Call_Seq);
            Map_Call := Add_Spg_Call_To_Call_Seq (Add_Prefix_To_Name
                         ("Call_", Display_Name (Identifier (Map_Spg))),
                         Map_Spg_Impl,
                         Call_Seq);
            ----------------------------------------------------------
            Generated_Feat := 0;
            while (Generated_Feat < Replica_Number) loop
               Generated_Feat := Generated_Feat + 1;
               Generated_Name := Add_Suffix_To_Name
                                 ("_" & Image (Generated_Feat),
                                 Feature_Name);
               Param := Add_New_Parameter
                        (Loc => Loc (Map_Spg),
                         Name => ATNU.Make_Identifier (Loc (Map_Spg),
                                 Generated_Name, Generated_Name, No_Node),
                         Container => Map_Spg,
                         Is_In => True,
                         Is_Out => False,
                         Is_Refinement => False);
               Conn := Add_Connection
                       (Wrapper_Spg,
                        Map_Call,
                        Param,
                        Param,
                        Wrapper_Spg_Impl,
                        CT_Parameter);
               if No (Conn) then
                  raise Program_error;
               end if;
            end loop;
            ----------------------------------------------------------
            --  add out param (or out event port) to the vote_spg and
            --  connect it to the wrapper_spg
            if (kind (Feature_Node) = K_Port_Spec) then
               if (Is_Event (Feature_Node)) then
                  Param := Add_New_Port_Spec
                           (Loc (Map_Spg),
                           ATNU.Make_Identifier (Loc (Map_Spg),
                           Feature_Name, Feature_Name, No_Node),
                           Vote_Spg,
                           False,
                           True,
                           Is_Data (Feature_Node),
                           Is_Event (Feature_Node),
                           Is_Feature (Feature_Node),
                           Is_Refinement (Feature_Node),
                           No_Node);
                  --  connect voter_Spg to wrapper_spg
                  Conn := Add_Connection
                          (Vote_Call,
                           Wrapper_spg,
                           Param,
                           Param,
                           Wrapper_Spg_Impl,
                           CT_Port_Connection);
               else
                  Param := Add_New_Parameter
                           (Loc => Loc (Map_Spg),
                           Name => ATNU.Make_Identifier (Loc (Map_Spg),
                           Feature_Name, Feature_Name, No_Node),
                           Container => Vote_Spg,
                           Is_In => False,
                           Is_Out => True,
                           Is_Refinement => False);
                  --  connect voter_Spg to wrapper_spg
                  Conn := Add_Connection
                          (Vote_Call,
                           Wrapper_spg,
                           Param,
                           Param,
                           Wrapper_Spg_Impl,
                           CT_Parameter);
                  if No (conn) then
                     raise Program_Error;
                  end if;
               end if;
               --  add in param to the vote_spg and
               --  connect it to the Map_spg
               --  iiii
            elsif (kind (Feature_Node) = K_Subcomponent_Access) then
               --  to treat data_access features
               null;
            else
               null;
            end if;
            --  Add the property_source name to the data created component
            Src := Get_String_Name (Get_Name_String (Name
                   (Identifier (Entity_Ref (Feature_Node)))) & "_Array");
            Data_Source := New_Node (K_Literal, No_Location);
            Set_Value (Data_Source, New_String_Value (Src));
            Add_Source_Name (Data_Impl, Data_Source);

            --  Add a feature of this type to the voter spg as well as
            --  to the mapping spg

            Voter_Param := Add_New_Parameter
                     (Loc => Loc (Wrapper_Spg),
                      Name => ATNU.Make_Identifier (Loc (Map_Spg),
                      Add_Suffix_To_Name ("_In", Voter_Param_Name),
                      Add_Suffix_To_Name ("_In", Voter_Param_Name),
                      No_Node),
                      Container => Vote_Spg,
                      Is_In => True,
                      Is_Out => False,
                      Is_Refinement => False);
            Entity := New_Node (K_Entity_Reference, No_Location);
            Set_Identifier (Entity, ATNU.Make_Identifier (No_Location,
                            Name (Identifier (Data)),
                            Name (Identifier (Data)),
                            No_Node));
            Set_Entity_Ref (Voter_Param, Entity);
            Map_Param := Add_New_Parameter
                     (Loc => Loc (Wrapper_Spg),
                      Name => ATNU.Make_Identifier (Loc (Map_Spg),
                      Add_Suffix_To_Name ("_Out", Voter_Param_Name),
                      Add_Suffix_To_Name ("_Out", Voter_Param_Name),
                      No_Node),
                      Container => Map_Spg,
                      Is_In => False,
                      Is_Out => True,
                      Is_Refinement => False);
            Set_Entity_Ref (Map_Param, Entity);
            --  connect voter_Spg to map_spg
            Conn := Add_Connection
              (Map_Call,
               Vote_Call,
               Map_Param,
               Voter_Param,
               Wrapper_Spg_Impl,
               CT_Parameter);
            if No (conn) then
               raise Program_Error;
            end if;
         when 1 => --  Classifier
            --  Voter_Spg :=
            null;
         when 2 =>
            null; --  to treat the reference
         when others => raise Program_Error;
      end case;
      --  generate data type correspondant to the type of data of
      --  the target feature (Feature_Node)

   end Generate_Consensus;

   ------------------------------
   -- Create_Voter_Subprograms --
   ------------------------------

   procedure Create_Voter_Subprograms (R            : Replication_Info;
                                       Wrapper      : Node_Id;
                                       Wrapper_impl : Node_Id) is
      Consensus_List     : Node_List;
      Property_Node      : Node_Id;
      Applies_To_List    : List_Id;
      Entity_Of_Property : Node_Id;
      Pointed_Node       : Node_Id := No_Node;
      Consensus_Type     : Integer;
      Consensus_Value    : Node_Id;
      Call_Seq           : Node_Id;
   begin

      --  add call_seq to the wrapper_spg
      Call_Seq := Add_New_Subprogram_Call_Sequence
                  (No_Location,
                  Make_Identifier
                  (No_Location,
                  Get_String_Name ("CS"),
                  Get_String_Name ("cs"),
                  No_Node),
                  Wrapper_Impl);

      Consensus_List := R.Consensus_Algo;
      if Consensus_List.First /= No_Node then
         while Present (Consensus_List.First) loop
            Property_Node := Consensus_List.First;
            Applies_To_List := List_Items (First_Node
                               (Applies_To_Prop (Property_Node)));
            if not (Remove_Suffix_From_Name ("_source_text",
            To_Lower (Display_Name (Identifier (Property_Node))))
            = To_Lower (Display_Name (Identifier (Property_Node))))
            then
               Consensus_Type := 0;
            elsif not (Remove_Suffix_From_Name ("_class", To_Lower
            (Display_Name (Identifier (Property_Node))))
            = To_Lower (Display_Name (Identifier (Property_Node))))
            then
               Consensus_Type := 1;
            elsif not (Remove_Suffix_From_Name ("_ref", To_Lower
            (Display_Name (Identifier (Property_Node)))) =
            To_Lower (Display_Name (Identifier (Property_Node))))
            then
               Consensus_Type := 2;
            else
               null;
            end if;
            if not ATNU.Is_Empty (Applies_To_List) then
               Pointed_Node := First_Node (Applies_To_List);
               while (Present (Pointed_Node)) loop
                  if Kind (Pointed_Node) = K_Array_Selection then
                     Entity_Of_Property := Corresponding_Entity
                                    (Identifier (Pointed_Node));
                  else
                     Entity_Of_Property := Corresponding_Entity
                                           (Pointed_Node);
                  end if;
                  Pointed_Node := Next_Node (Pointed_Node);
               end loop;
            end if;

            Consensus_Value := Property_Association_Value (Property_Node);
            Generate_consensus (Consensus_Value,
                                Consensus_Type,
                                Entity_Of_Property,
                                Wrapper,
                                Wrapper_Impl,
                                Integer (R.Number),
                                Call_Seq);
            Remove_Node_From_Node_List (Property_Node, Consensus_List);
         end loop;
      end if;
   end Create_Voter_Subprograms;

   ----------------------
   --  Add_Wrapper_Spg --
   ----------------------

   procedure Add_Wrapper_Spg (Thread_Type : Node_Id;
                              Thread_Impl : Node_Id;
                              R : Replication_Info)
   is

      pragma Assert (Kind (Thread_Type) = K_Component_Type);
      pragma Assert (Kind (Thread_Impl) = K_Component_Implementation);
      Wrapper_Spg      : Node_Id;
      Wrapper_Spg_Impl : Node_Id;
      Current_Feature  : Node_Id;
      Feature_List     : List_Id;
      Src, Dest, Conn  : Node_Id;
      Category         : Ocarina.Me_AADL.Connection_Type;
      New_Param        : Node_Id;
      Call_To_Wrapper  : Node_Id;

   begin

      --  1. create the wrapper
      --  1.1. Create the component subprogram declaration Wrapper_Spg
      Wrapper_Spg := Add_Component
                     (Loc (Thread_Type),
                      No_Node,  --  core_name
                      Namespace (Thread_Type),
                      CC_Subprogram,
                      False, --  Is_Private
                      "Wrapper_Spg", --  suffix_name
                      False); --  Comp Impl

      if No (Wrapper_Spg) then
         raise Program_Error;
      end if;

      --  1.2 Create the component subprogram Impl Wrapper_Spg_Impl
      Wrapper_Spg_Impl := Add_Component
                          (Loc (Thread_Type),
                          No_Node,  --  core_name
                          Namespace (Thread_Type),
                          CC_Subprogram,
                          False, --  Is_Private
                          "Wrapper_Spg.Impl", --  suffix_name
                          True); --  Comp Impl
      if No (Wrapper_Spg_Impl) then
         raise Program_Error;
      end if;

      --  2. Add Parameters to the wrapper
      Feature_List := ATN.Features (Thread_Type);
      Current_Feature := First_Node (Feature_List);
      while Present (Current_Feature) loop
         New_Param := Add_Subprogram_Parameter (Wrapper_Spg, Current_Feature);
         if No (New_Param) then
            raise Program_Error;
         end if;
         Current_Feature := Next_Node (Current_Feature);
      end loop;

      --  3. Add call Sequence to the wrapper
      Call_To_Wrapper := Create_Call_Sequence_to_Wrapper
                         (Wrapper_Spg_Impl, Thread_Impl);

      --  4. connect voter_th to wrapper_Spg
      Current_Feature := First_Node (Feature_List);
      while Present (Current_Feature) loop
         if (kind (Current_Feature) = K_Port_Spec) then
            if (Is_In (Current_Feature)) then
               Src := Thread_Impl;
               Dest := Call_To_Wrapper;
               Category := CT_Parameter;
            elsif Is_Out (Current_Feature) then
               Dest := Thread_Impl;
               Src := Call_To_Wrapper;
               Category := CT_Port_Connection;
            end if;
            Conn := Add_Connection
                    (Src,
                    Dest,
                    Current_Feature,
                    Current_Feature,
                    --  because the voter thread (voter_th) and
                    --  the wrapper have the same feature'identifier
                    Thread_Impl,
                    Category);
            if No (Conn) then
               raise Program_Error;
            end if;
         else
            --  it remainds the treatment of other types of features
            --  parameters, data access and subprogram accessess
            null;
         end if;
         Current_Feature := Next_Node (Current_Feature);
      end loop;

      --  4. create voters subprograms (map and vote)
      Create_Voter_Subprograms (R, Wrapper_Spg, Wrapper_Spg_Impl);
   end Add_Wrapper_Spg;

   -------------------------------
   --  Add_Subprogram_Parameter --
   -------------------------------

   function Add_Subprogram_Parameter (Spg        : Node_Id;
                                      Feature    : Node_Id;
                                      Entity_Ref : Node_Id := No_Node)
   return Node_Id is

      pragma Assert (Kind (Spg) = K_Component_Type);
      Added_Param   : Node_Id := No_Node;
      --  Added_Feature : Boolean;
   begin
      if (kind (Feature) = K_Port_Spec) then
         if (Is_In (Feature)) then
            Added_Param := Add_New_Parameter
                           (Loc => Loc (Spg),
                           Name => Identifier (Feature),
                           Container => Spg,
                           Is_In => Is_In (Feature),
                           Is_Out => Is_Out (Feature),
                           Is_Refinement => False);
         elsif Is_Event (Feature) then
            Added_Param := Add_New_Port_Spec
                     (Loc (Feature),
                      Identifier (Feature),
                      Spg,
                      Is_In (Feature),
                      Is_Out (Feature),
                      Is_Data (Feature),
                      Is_Event (Feature),
                      Is_Feature (Feature),
                      Is_Refinement (Feature),
                      No_Node);

         elsif Is_Data (Feature) then
            Added_Param := Add_New_Parameter
                           (Loc => Loc (Spg),
                           Name => Identifier (Feature),
                           Container => Spg,
                           Is_In => Is_In (Feature),
                           Is_Out => Is_Out (Feature),
                           Is_Refinement => False);
         else
            null;
         end if;
      elsif kind (Feature) = K_Parameter then
         Added_Param := Add_New_Parameter
                       (Loc => Loc (Spg),
                        Name => Identifier (Feature),
                        Container => Spg,
                        Is_In => Is_In (Feature),
                        Is_Out => Is_Out (Feature),
                        Is_Refinement => False);
      else
         null;
      end if;
      if (Entity_Ref /= No_Node) then
         Set_Entity_Ref (Added_Param, Entity_Ref);
      end if;
      return Added_Param;
   end Add_Subprogram_Parameter;

   --------------------------------------
   --  Create_Call_Sequence_to_Wrapper --
   --------------------------------------

   function Create_Call_Sequence_to_Wrapper
                  (T                : Node_Id;
                   Wrapper_Spg_Impl : Node_Id)
     return Node_Id
     is
      Wrapper_CS_Name  : constant Name_Id := Get_String_Name ("cs");
      Wrapper_Call_Seq : Node_Id := No_Node;
      Node             : Node_Id;
      Path_Ref   : constant List_Id := New_List (K_List_Id, No_Location);
      C, Ref : Node_Id;

   begin

      Node := New_Node (K_Subprogram_Call, No_Location);
      Set_Identifier
           (Node, Make_Identifier
            (No_Location, Get_String_Name ("Wrapper_Call"),
             Get_String_Name ("Wrapper_Call"), Node));
      Ref := New_Node (K_Entity_Reference, No_Location);
      C := New_Node (K_Node_Container, No_Location);
      Set_Item (C, Make_Identifier
                (No_Location, Name (Identifier (T)),
                 Display_Name (Identifier (T)), No_Node));
      ATNU.Append_Node_To_List (C, Path_Ref);
      Set_Path (Ref, Path_Ref);
      Set_Identifier (Ref, Make_Identifier
                      (No_Location, Name (Identifier (T)),
                       Display_Name (Identifier (T)), No_Node));
      Set_Entity_Ref (Node, Ref);

      Wrapper_Call_Seq := Add_New_Subprogram_Call_Sequence
              (No_Location,
               Make_Identifier
               (No_Location,
                Wrapper_CS_Name,
                Wrapper_CS_Name,
                No_Node),
                Wrapper_Spg_Impl);

      if not Add_Subprogram_Call (Wrapper_Call_Seq, Node) then
         raise Program_Error;
      end if;
      return Node;
   end Create_Call_Sequence_to_Wrapper;

   --------------------
   --  Add_Component --
   --------------------

   function Add_Component (Loc             : Location;
                           Core_Name       : Node_Id;
                           Namespace       : Node_Id;
                           Component_Type  :
                           Ocarina.ME_AADL.Component_Category;
                           Is_Private      : Boolean := False;
                           Suffix_Name     : String;
                           Implementation  : Boolean := False)
     return Node_Id is
      Identifier_Name : Node_Id;
      New_Comp        : Node_Id := No_Node;

   begin

      Identifier_Name := New_Node (K_Identifier, Loc);
      if (not Implementation) then
         New_Comp := Add_New_Component_Type
                  (Loc,
                  Identifier_Name,
                  Namespace,
                  Component_Type,
                  Is_Private);
      else
         New_Comp := Add_New_Component_Implementation
                  (Loc,
                  Identifier_Name,
                  Namespace,
                  Component_Type,
                  Is_Private);
      end if;

      if (Present (Core_Name)) then
         Set_Identifier (New_Comp, Identifier_Name);
         Set_Name (Identifier_Name, To_Lower (
             Add_Suffix_To_Name (Suffix_Name,
             Display_Name (Identifier (Core_Name)))));
         Set_Display_Name (Identifier_Name,
             Add_Suffix_To_Name (Suffix_Name,
             Display_Name (Identifier (Core_Name))));
      elsif not (Suffix_Name = "") then
         Set_Identifier (New_Comp, Identifier_Name);
         Set_Name (Identifier (New_Comp), To_Lower (Get_String_Name
               (Suffix_Name)));
         Set_Display_Name (Identifier (New_Comp),
               Get_String_Name (Suffix_Name));
      else
         raise Program_Error;
      end if;

      return New_Comp;

   end Add_Component;

   ------------------------------------
   --  Set_Property_Association_List --
   ------------------------------------

   procedure Set_Property_Association_List (Pointed_Node : Node_Id;
                                           Replica_List  : List_Id;
                                           Container     : Node_Id)
   is
      Properties_List        : List_Id;
      Current_Property       : Node_Id;
      Applied_To, Applies_To : List_Id;
      Id, Applies_To_Node    : Node_Id;
      Replica          : Node_Id;
      Prop_Index       : Integer;
      Prop_List_Length : constant Integer
                       := ATNU.Length (ATN.Properties (Container));
      Found : Boolean := False;
   begin

      Applied_To := New_List (K_List_Id, No_Location);
      ATNU.Append_Node_To_List (Make_Identifier (No_Location,
                           Name (Identifier (Pointed_Node)),
                           Name (Identifier (Pointed_Node)),
                           No_Node), Applied_To);
      Id := New_Node (K_Contained_Element_Path, No_Location);
      Set_List_Items (Id, Applied_To);
      ATNU.Append_Node_To_List (Pointed_Node, Applied_To);

      -----------------------------------------------
      Properties_List := ATN.Properties (Container);
      Current_Property := First_Node (Properties_List);
      Prop_Index := 1;
      ----------------------------------------------

      while (Prop_Index < Prop_List_Length) and then
      Present (Current_Property) loop
         if not (ATNU.Is_Empty (Applies_To_Prop (Current_Property)))
         then
            Applies_To := List_Items (First_Node
                         (Applies_To_Prop (Current_Property)));
            Applies_To_Node := First_Node (Applies_To);
            while (Present (Applies_To_Node) and then not Found) loop
               if Corresponding_Entity (Applies_To_Node) = Pointed_Node
               then
                  Replica := First_Node (Replica_List);
                  while (Present (Replica)) loop
                     Copy_Property (Pointed_Node, Replica, Current_Property);
                     Replica := Next_Node (Replica);
                  end loop;
                  Remove_Node_From_List (Current_Property, Properties_List);
                  Found := true;
               end if;
               Applies_To_Node := Next_Node (Applies_To_Node);
            end loop;
         end if;
         Current_Property := Next_Node (Current_Property);
         Prop_Index := Prop_Index + 1;
      end loop;
   end Set_Property_Association_List;

   ---------------------
   --  Get_Connection --
   ---------------------

   function Get_Connection (Component   : Node_Id;
                            Conn_Source : Node_Id)
     return Node_Id is
      Connection_List : List_Id;
      Current_Conn    : Node_Id;
      Target_Conn     : Node_Id;
      Found           : boolean := false;
   begin

      Connection_List := ATN.Connections (Component);
      Current_Conn := First_Node (Connection_List);
      while (Present (Current_Conn) and then not Found) loop
         if (Entity (ATN.Source (Current_Conn)) = Conn_Source) then
            Found := true;
            Target_Conn := Current_Conn;
         elsif (Entity (ATN.Destination (Current_Conn)) = Conn_Source)
         then
            Found := true;
            Target_Conn := Current_Conn;
         end if;
         Current_Conn := Next_Node (Current_Conn);
      end loop;
      if not Found then
         return No_Node;
      else
         return Target_Conn;
      end if;
   end Get_Connection;

   -----------------------
   --  Generate_Replica --
   -----------------------

   function Generate_Replica (Replicated_Component : Node_Id;
                             Identifiers_List      : List_Id;
                             Comp_Category         :
                             Ocarina.ME_AADL.Component_Category)
     return List_Id is
      Generated_Replica_Nbr : Integer;
      Replica_List          : List_Id;
      Current_Identifier    : Node_Id;
      New_Replica           : Node_Id;
      Parent_Component      : Node_Id;
   begin

      Replica_List := New_List (K_List_Id, No_Location);
      Parent_Component := Container_Component (Replicated_Component);

      Generated_Replica_Nbr := 0;
      Current_Identifier := First_Node (Identifiers_List);
      while Generated_Replica_Nbr < ATNU.Length (Identifiers_List) and then
         Present (Current_Identifier) loop
         New_Replica := Add_Subcomponent
                 (Loc                => No_Location,
                 Comp_Impl           => Parent_Component,
                 Category            => Comp_Category,
                 Is_Refinement       => False,
                 In_Modes            => In_Modes (Replicated_Component),
                 Prototypes_Bindings => Prototype_Bindings
                                        (Replicated_Component),
                 Entity_Ref          => Entity_Ref (Replicated_Component),
                 Identifier_Name     => Get_String_Name
                                        (Get_String_Of_Property_Value
                                        (Current_Identifier)));
         --  save the diffrent created replicas in a table to use them
         --  when connecting the replicas with voters
         ATNU.Append_Node_To_List (New_Replica,
                              Replica_List);
         Generated_Replica_Nbr := Generated_Replica_Nbr + 1;
         Current_Identifier := Next_Node (Current_Identifier);
      end loop;
      return Replica_List;
   end Generate_Replica;

   ----------------------
   --  Is_Node_In_List --
   ----------------------

   function Add_Connection (Source           : Node_Id;
                           Destination       : Node_Id;
                           Source_Port       : Node_Id;
                           Destination_Port  : Node_Id;
                           Parent_Comp_Impl  : Node_Id;
                           Category : Ocarina.Me_AADL.Connection_Type
                                    := CT_Port_Connection)
     return Node_Id is

      pragma Assert (Present (Source));
      pragma Assert (Present (Destination));

      Source_Entity_Ref  : Node_Id;
      Dest_Entity_Ref    : Node_Id;
      Connect_Node       : Node_Id;
   begin

      --  create entity_reference of both source and destination
      Source_Entity_Ref := New_Node (K_Entity_Reference, No_Location);

      --  concatinate the identifier of the source component with
      --  source feature
      Set_Identifier (Source_Entity_Ref,
                     Make_Identifier (No_Location,
                     Add_Prefix_To_Name (Get_Name_String (Name (Identifier
                     (Source))), Add_Prefix_To_Name (".", Name
                     (Identifier (Source_Port)))),

                     Add_Prefix_To_Name (Get_Name_String (Display_Name
                     (Identifier (Source))), Add_Prefix_To_Name
                     (".", Display_Name (Identifier (Source_Port)))),
                     No_Node));

      Set_Entity (Source_Entity_Ref, Source);

      Dest_Entity_Ref := New_Node (K_Entity_Reference,
                                               No_Location);
      --  concatinate the identifier of the destination
      --  component with destination feature
      Set_Identifier (Dest_Entity_Ref,
                     Make_Identifier (No_Location,

                     Add_Prefix_To_Name (Get_Name_String (Name (Identifier
                     (Destination))), Add_Prefix_To_Name (".", Name
                     (Identifier (Destination_Port)))),

                     Add_Prefix_To_Name (Get_Name_String (Display_Name
                     (Identifier (Destination))), Add_Prefix_To_Name
                     (".", Display_Name (Identifier (Destination_Port)))),
                     No_Node));
      Set_Entity (Dest_Entity_Ref, Destination);

      --  Add new connection
      Connect_Node := Add_New_Connection
                      (Loc (Source),
                      No_Node,  --  Identifier
                      Parent_Comp_Impl,
                      Category,
                      False,  --  Is_Refinement
                      False,  --  Is_Bidirect
                      Source_Entity_Ref,
                      Dest_Entity_Ref,
                      No_Node);  -- InModes
      return Connect_Node;
   end Add_Connection;

   ----------------------
   --  Add_Subcomponent --
   ----------------------

   function Add_Subcomponent (Loc                 : Location := No_Location;
                              Comp_Impl           : Node_Id;
                              Category : Ocarina.ME_AADL.Component_Category;
                              Is_Refinement       : Boolean;
                              In_Modes            : Node_Id := No_Node;
                              Prototypes_Bindings : List_Id := No_List;
                              Entity_Ref          : Node_Id := No_Node;
                              Identifier_Name     : Name_Id)

     return Node_Id is

      Identifier_Node     : Node_Id;
      Entity_Reference    : Node_Id;
      New_Subcomp         : Node_Id;

   begin
      Identifier_Node := New_Node (K_Identifier, Loc);
      Entity_Reference := New_Node (K_Entity_Reference, No_Location);
      Set_Identifier (Entity_Reference,
                      Make_Identifier (No_Location,
                      Name (Identifier (Entity_Ref)),
                      Name (Identifier (Entity_Ref)),
                      No_Node));
      Set_Entity (Entity_Reference, Entity_Ref);
      New_Subcomp := Add_New_Subcomponent
                     (Loc,
                      Identifier_Node,
                      Comp_Impl,
                      Category,
                      Is_Refinement,
                      In_Modes,
                      Prototypes_Bindings,
                      Entity_Reference);
      Set_Identifier (New_Subcomp, Identifier_Node);

      Set_Name (Identifier (New_Subcomp), To_Lower (Identifier_Name));
      Set_Display_Name (Identifier (New_Subcomp), Identifier_Name);
      return New_Subcomp;
   end Add_Subcomponent;

   ----------------------
   --  Is_Node_In_List --
   ----------------------

   function Is_Node_Of_Node_List (Node : Node_Id; List : Node_List)
     return boolean is

      pragma Assert (Present (Node));

      Is_Node : boolean := False;
      Current : Node_Id;
   begin
      if List.First /= No_Node then
         Current := List.First;
         while Present (Current) loop
            if Current = Node then
               Is_Node := true;
               exit;
            else
               Current := Next_Entity (Current);
            end if;
         end loop;
      end if;
      return Is_Node;
   end Is_Node_Of_Node_List;

   --------------------------------
   -- Has_Replication_Properties --
   --------------------------------

   function Has_Replication_Properties (N : Node_Id) return boolean is

      pragma Assert (Kind (N) = K_Component_Type
                     or else Kind (N) = K_Component_Implementation);

      Properties_List     : List_Id := No_List;
      Property_Node       : Node_Id := No_Node;
      Is_Replicated       : boolean := false;
      Property_Name       : Name_Id;
      Replication_Prefix  : constant Name_Id :=
                            Get_String_Name ("Replication_Properties");
   begin
      if not ATNU.Is_Empty (ATN.Properties (N)) then
         Properties_List := ATN.Properties (N);
         Property_Node := First_Node (Properties_List);
         while Present (Property_Node) and then not Is_Replicated loop
            Property_Name := Get_Name_Of_Entity (Property_Node, true);
            if Is_Prefix (Replication_Prefix, Property_Name) then
               Is_Replicated := true;
            end if;
            Property_Node := Next_Node (Property_Node);
         end loop;
      end if;
      return Is_Replicated;
   end Has_Replication_Properties;

   -----------------------------------
   -- Is_Defined_Import_Declaration --
   -----------------------------------

   function Is_Defined_Import_Declaration (N : Node_Id)
     return boolean is
      pragma Assert (Kind (N) = K_Package_Specification);

      Imports_List           : List_Id := No_List;
      With_Clause_Node       : Node_Id := No_Node;
      Package_Spec_List_Node : Node_Id := No_Node;
      Import_Node            : Node_Id := No_Node;
      Is_Defined             : boolean := false;
      Replication_Prefix     : constant Name_Id :=
                            Get_String_Name ("replication_properties");
   begin

      if Declarations (N) /= No_List then
         Package_Spec_List_Node := First_Node (Declarations (N));
         while Present (Package_Spec_List_Node) and then not Is_Defined loop
            case Kind (Package_Spec_List_Node) is
            when K_Name_Visibility_Declaration =>
               if not ATNU.Is_Empty (List_Items (Package_Spec_List_Node)) then
                  Imports_List := List_Items (Package_Spec_List_Node);
                  With_Clause_Node := First_Node (Imports_List);
                  while Present (With_Clause_Node) and then not Is_Defined loop
                     case kind (With_Clause_Node) is
                     when K_Import_Declaration =>
                        if not ATNU.Is_Empty (List_Items
                              (With_Clause_Node))
                        then
                           Import_Node := First_Node (List_Items
                                          (With_Clause_Node));
                        end if;
                        while Present (Import_Node) and then
                              not Is_Defined loop
                           if Kind (Import_Node) = K_Identifier and then
                                   Is_Prefix (Replication_Prefix,
                                              (Name (Import_Node)))
                           then
                              Is_Defined := true;
                           end if;
                           Import_Node := Next_Node (Import_Node);
                        end loop;
                     when others =>
                        null;
                     end case;
                     With_Clause_Node := Next_Node (With_Clause_Node);
                  end loop;
               end if;
            when others =>
               null;
            end case;
            Package_Spec_List_Node := Next_Node (Package_Spec_List_Node);
         end loop;
      end if;
      return Is_Defined;
   end Is_Defined_Import_Declaration;

   -------------------------
   -- Expand_With_Replica --
   -------------------------

   procedure Expand_With_Replicas (Root : Node_Id) is

      pragma Assert (Kind (Root) = K_AADL_Specification);

      Entity_Node                   : Node_Id;
      List_Node                     : Node_Id;
      Is_Defined_With_Clause        : boolean := false;
      Is_Replicated                 : boolean := false;
      Source_File_Name : Name_Id := No_Name;
      Output_File_Name : Name_Id;
      Output_File      : GNAT.OS_Lib.File_Descriptor
                          := GNAT.OS_Lib.Invalid_FD;
      Package_Name     : Name_Id := No_name;
      Identifier_Node  : Node_Id;
   begin
      if not ATNU.Is_Empty (Declarations (Root)) then
         List_Node := First_Node (Declarations (Root));
         while Present (List_Node) and then not Is_Defined_With_Clause loop
            case Kind (List_Node) is
            when K_Package_Specification  =>
               if Is_Defined_Import_Declaration (List_Node) then
                  Is_Defined_With_Clause := true;
               end if;
            when others =>
                  null;
            end case;
            List_Node := Next_Node (List_Node);
         end loop;

         if Is_Defined_With_Clause then
            List_Node := First_Node (Declarations (Root));
            while Present (List_Node) loop
               case Kind (List_Node) is
               when K_Package_Specification  =>
                  Package_Name := Add_Prefix_To_Name ("Generated_",
                                  Display_Name (Identifier (List_Node)));
                  Identifier_Node := New_Node (K_Identifier,
                                  Loc (List_Node));
                  Set_Identifier (List_Node, Identifier_Node);
                  Set_Display_Name (Identifier (List_Node), Package_Name);
                  Set_Name (Identifier (List_Node),
                                    To_Lower (Package_Name));
                  if Declarations (List_Node) /= No_List then
                     Entity_Node :=
                       First_Node (Declarations (List_Node));
                     while Entity_Node /= No_Node loop
                        case Kind (Entity_Node) is
                           when K_Component_Type
                                | K_Component_Implementation =>
                              Is_Replicated := Has_Replication_Properties
                                                    (Entity_Node);
                              if Is_Replicated then
                                 Extract_Properties_List (Entity_Node);
                                 Source_File_Name := Loc (Entity_Node).
                                                     Base_Name;
                              end if;
                           when others =>
                              null;
                        end case;
                        Entity_Node := Next_Node (Entity_Node);
                     end loop;
                  end if;
               when others =>
                  null;
               end case;
               List_Node := Next_Node (List_Node);
            end loop;

            --  Generation of a new aadl model containing the desired
            --  replication expansion

            --  1. Create the generated output file

            if (Source_File_Name /= No_Name) then
               Output_File_Name := Add_Prefix_To_Name ("Generated_",
                                   Source_File_Name);
               if Output_File_Name /= No_Name then
                  Output_File := Create_File (Get_Name_String
                                 (Output_File_Name), Binary);
                  Set_Output (Output_File);

                  --  2. Generate the new AADL tree

                  Generate_AADL_Model (Root, False);
                  Set_Standard_Error;
                  Release_Output (Output_File);
               else
                  Display_Error ("Cannot create file",
                              Fatal => True, Warning => False);
               end if;
            end if;
         else
            Display_Error ("Cannot expand AADL models: No defined replication"
                   & " properties", Fatal => False, Warning => False);
         end if;
      end if;
   end Expand_With_Replicas;

   ------------------------
   -- Thread_Replication --
   ------------------------

   procedure Thread_Replication (T : Replication_Info) is
   begin
      Put_Line ("Replicate a" & Get_Category_Of_Component
      (Get_Corresponding_Component (T.Replicated_Component))'Img);
   end Thread_Replication;

   -------------------------
   -- Process_Replication --
   -------------------------

   procedure Process_Replication (P : Replication_Info) is
      Replicated_Component        : Node_Id;
      Parent_Component            : Node_Id;
      Replica_Number              : Integer := 0;
      Replica_Location            : Location := No_Location;
      Voter_Pro, Voter_Pro_Impl   : Node_Id := No_Node;
      Voter_Th, Voter_Th_Impl     : Node_Id := No_Node;
      Voter_Th_Subcomponent       : Node_Id := No_Node;
      Voter_Pro_Subcomponent      : Node_Id := No_Node;
      Adding_Feature              : boolean;
      Generated_Feature_Nbr       : Integer := 0;
      Generated_Feature           : Node_Id;
      Feature_Name                : Name_Id := No_Name;
      Feature_Identifier          : Node_Id := No_Node;
      Features_List, Replica_List : List_Id;
      Current_Feature             : Node_Id;
      New_Replica                 : Node_Id;
      Generated_Replica_Nbr       : Integer;
      Corresponding_component     : Node_Id;
      Connect_Node                : Node_Id;
      Source, Destination         : Node_Id;
      Target_Connection           : Node_Id;

   begin
      Put_Line ("**************** Replicate a process ****************");
      Replicated_Component := P.Replicated_Component;
      Corresponding_component := Get_Corresponding_Component
                                 (Replicated_Component);
      Parent_Component := Container_Component (Replicated_Component);
      Replica_Location := Loc (P.Replicated_Component);
      Replica_Number := Integer (P.Number);

      --  1. Create the voter process containing itself the voter thread
      --  1.1 Create a new component type corresponding to the voter Process
      Voter_Pro := Add_Component
                   (Replica_Location,
                    Replicated_Component,  --  core_name
                    Namespace (Parent_Component),
                    CC_Process,
                    False, --  Is_Private
                    "_Voter_Process", --  suffix_name
                    False); --  Compo Impl

      --  1.2 Create a new component implementation corresponding to
      --  the voter Process
      Voter_Pro_Impl := Add_Component
                       (Replica_Location,
                       Voter_Pro,  --  core_name
                       Namespace (Parent_Component),
                       Get_Category_Of_Component (Voter_Pro),
                       False, --  Is_Private
                       ".impl", --  suffix_name
                       True); --  Comp Impl

      --  1.3 Add the voter process as a subcomponent of the parent system
      Voter_Pro_Subcomponent := Add_Subcomponent
                               (Replica_Location,
                                Parent_Component,
                                Get_Category_Of_Component (Voter_Pro_Impl),
                                False,
                                No_Node,
                                No_List,
                                Voter_Pro_Impl,
                                Name (Identifier (Voter_Pro)));

      --  1.4 Create a new component type corresponding to the voter Thread
      Voter_Th := Add_Component
                  (Replica_Location,
                  Replicated_Component,  --  core_name
                  Namespace (Parent_Component),
                  CC_Thread,
                  False, --  Is_Private
                  "_Voter_Th", --  suffix_name
                  False); --  Comp Impl

      --  1.5 Create a new component implementation corresponding
      --  to the voter Thread
      Voter_Th_Impl := Add_Component
                      (Replica_Location,
                      Voter_Th,  --  core_name
                      Namespace (Parent_Component),
                      CC_Thread,
                      False, --  Is_Private
                      ".impl", --  suffix_name
                      True); --  Compo Impl

      --  1.6 Set the voter thread as a subcomponent of the voter process
      Voter_Th_Subcomponent := Add_Subcomponent
                               (Replica_Location,
                                Voter_Pro_Impl,
                                Get_Category_Of_Component (Voter_Th_Impl),
                                False,
                                No_Node,
                                No_List,
                                Voter_Th_Impl,
                                Name (Identifier (Voter_Th)));
      -------------------------------------------------------------------
      --  3. Generate the different replicas
      Replica_List := Generate_Replica (Replicated_Component,
                                        P.Identifiers, CC_Process);

      --  **************************************************
      --  3 Add the features to the created process, thread
      --  and wrapper_Spg and connect them progressively

      Features_List := ATN.Features (Corresponding_Entity (
            Component_Type_Identifier (Corresponding_component)));
      Current_Feature := First_Node (Features_List);
      Generated_Replica_Nbr := 1;
      while Present (Current_Feature) loop

         if kind (Current_Feature) /= K_Parameter
           and then kind (Current_Feature) = K_Port_Spec
           and then (Is_Data (Current_Feature)
                    or else Is_Event (Current_Feature))
           and then Is_Out (Current_Feature)
         then
            --  3.1 Collect the list of features and duplicate
            Generated_Feature_Nbr := 0;
            --  generate the in features of the voter process
            --  and the out features of the voter thread
            Generated_Feature := Add_New_Port_Spec
                                   (Loc (Current_Feature),
                                    Identifier (Current_Feature),
                                    Parent_Component,
                                    Is_In (Current_Feature),
                                    Is_Out (Current_Feature),
                                    Is_Data (Current_Feature),
                                    Is_Event (Current_Feature),
                                    Is_Feature (Current_Feature),
                                    False, --  Is_Refinement (Current_Feature),
                                    No_Node);
            Adding_Feature := Add_Feature (Voter_Pro,
                              Generated_Feature);

            if not Adding_Feature then
                  Display_Error ("Cannot expand AADL models: Can not generate"
                  & " features of the voter Process",
                  Fatal => False, Warning => False);
            else
               Adding_Feature := Add_Feature (Voter_Th,
                             Generated_Feature);
               if not Adding_Feature then
                  Display_Error ("Cannot expand AADL models: Can not generate"
                  & " features of the voter Thread",
                  Fatal => False, Warning => False);
               else

                  --  connection between voter_process and The target component
                  --  nous devons suivre la connection afin d'arriver la dest
                  --  ---------------------------------------------------------
                  --  3.2 connetion between voter thread and voter process
                  --  Adding progressively the connexion between process
                  --  and thread

                  --  the direction of the connection
                  Destination := Voter_Pro_Subcomponent;
                  Source := Voter_Th_Subcomponent;
                  --  add the connection between source and destination
                  Connect_Node := Add_Connection
                                  (Source,
                                   Destination,
                                   Generated_Feature,
                                   Generated_Feature,
                                   --  because the voters (Pro and th)
                                   --  have the same identifier of the feature
                                   Voter_Pro_Impl);
                  if No (Connect_Node) then
                     raise Program_Error;
                  end if;
               end if;
            end if;

            --  generate the in features of the voter
            New_Replica := First_Node (Replica_List);
            while not (Generated_Feature_Nbr = Replica_Number) loop
               Generated_Feature_Nbr := Generated_Feature_Nbr + 1;
               --  to count the generated number of features
               Feature_Identifier := New_Node (K_Identifier, Loc (Voter_Pro));
               Generated_Feature := Add_New_Port_Spec
                                   (Loc (Current_Feature),
                                    Feature_Identifier,
                                    Parent_Component,
                                    True,
                                    Is_In (Current_Feature),
                                    Is_Data (Current_Feature),
                                    Is_Event (Current_Feature),
                                    Is_Feature (Current_Feature),
                                    False, --  Is_Refinement (Current_Feature),
                                    No_Node);
               --  To create the different features of the voter process
               --  and thread
               Feature_Name := Add_Suffix_To_Name ("_"
                               & Image (Generated_Feature_Nbr),
                               Display_Name (Identifier (Current_Feature)));
               Set_Identifier (Generated_Feature, Feature_Identifier);
               Set_Name (Feature_Identifier, To_Lower (Feature_Name));
               Set_Display_Name (Feature_Identifier, Feature_Name);
               Adding_Feature := Add_Feature (Voter_Pro,
                              Generated_Feature);

               if not Adding_Feature then
                  Display_Error ("Cannot expand AADL models: Can not generate"
                  & " features of the voter Process",
                  Fatal => False, Warning => False);
               else
                  --  Adding progressively the connexion from voter process
                  --   (out) to original components (in)
                  Source := Voter_Pro_Subcomponent;
                  Target_Connection := Get_Connection (Parent_Component,
                                                       Current_Feature);
                  if (Present (Target_Connection)) then
                     Destination := ATN.Destination (Target_Connection);

                     Connect_Node := Add_Connection
                                     (Source => Source,
                                     Destination => Corresponding_Entity (Item
                                     (First_Node (Path (Destination)))),
                                     Source_Port => Current_Feature,
                                     Destination_Port => Entity (Destination),
                                     Parent_Comp_Impl => Parent_Component);
                     if No (Connect_Node) then
                        raise Program_Error;
                     end if;
                     ATNU.Remove_Node_From_List (Target_Connection,
                                     ATN.Connections (Parent_Component));
                  end if;
               end if;
               Adding_Feature := Add_Feature (Voter_Th,
                                 Generated_Feature);
               if not Adding_Feature then
                  Display_Error ("Cannot expand AADL models: Can not generate"
                  & " features of the voter thread",
                  Fatal => False, Warning => False);
               else

                  --  Adding progressively the connexion between process
                  --  and thread

                  --  the direction of the connection
                  Source := Voter_Pro_Subcomponent;
                  Destination := Voter_Th_Subcomponent;

                  --  create entity_reference of both source and destination
                  Connect_Node := Add_Connection
                                  (Source,
                                  Destination,
                                  Generated_Feature,
                                  Generated_Feature,
                                  Voter_Pro_Impl);
                  if No (Connect_Node) then
                     raise Program_Error;
                  end if;
                  --  Creating the connection from replicas to voter process
                  --  the direction of the connection
                  Source := New_Replica;
                  Destination := Voter_Pro_Subcomponent;
                  --  connect the source to the destination
                  Connect_Node := Add_Connection
                                  (Source,
                                  Destination,
                                  Current_Feature,
                                  Generated_Feature,
                                  Parent_Component);
                  if No (Connect_Node) then
                     raise Program_Error;
                  end if;
               end if;
               New_Replica := Next_Node (New_Replica);
            end loop;

            Generated_Replica_Nbr := Generated_Replica_Nbr + 1;
         elsif kind (Current_Feature) = K_Port_Spec and then
            (Is_Data (Current_Feature) or else Is_Event (Current_Feature))
           and then Is_In (Current_Feature)
         then

         --  4. connect original component to Replicas
         --  from original component model -> to replicas
            Destination := Replicated_Component;
            Target_Connection := Get_Connection (Parent_Component,
                                                Current_Feature);

            if (Present (Target_Connection)) then
               Source := ATN.Source (Target_Connection);
               New_Replica := First_Node (Replica_List);
               while (Present (New_Replica)) loop
                  Connect_Node := Add_Connection
                                  (Source => Corresponding_Entity (Item
                                  (First_Node (Path (Source)))),
                                  Destination => New_Replica,
                                  Source_Port => Entity (Source),
                                  Destination_Port => Current_Feature,
                                  Parent_Comp_Impl => Parent_Component);
                  if No (Connect_Node) then
                     raise Program_Error;
                  end if;
                  New_Replica := Next_Node (New_Replica);
               end loop;
               ATNU.Remove_Node_From_List (Target_Connection,
                      ATN.Connections (Parent_Component));
            end if;

         elsif kind (Current_Feature) = K_Subcomponent_Access then

            --  Subcomponent_Category (Current_Feature) = CC_Data then
            Put_Line ("data_access " & Is_provided (Current_Feature)'Img);
            --  it remains the implementation of the connection between
            --  replicas and voters if it consist of non port_spec feature
         else
            null;
         end if;
         Current_Feature := Next_Node (Current_Feature);
      end loop;

      --  6.Create Wrapper Spg and subprograms to execute vote
      Add_Wrapper_Spg (Voter_Th, Voter_Th_Impl, P);

      --  7. set the property actual_processor_binding
      Set_Actual_Processor_Binding (Replicated_Component,
                                    Voter_Pro_Subcomponent,
                                    Parent_Component);

      --  8. Copy the same properties of the replicated component to replicas
      --  and remove those related to original replica

      Set_Property_Association_List (Replicated_Component,
                                     Replica_List,
                                     Parent_Component);
      --  9. Remove the original component from the list of subcomponent
      Remove_Node_From_List (Replicated_Component, ATN.Subcomponents
                            (Parent_Component));

   end Process_Replication;

   ------------------------
   -- System_Replication --
   ------------------------

   procedure System_Replication (S : Replication_Info) is
   begin
      Put_Line ("Replicate a " & Get_Category_Of_Component
      (Get_Corresponding_Component (S.Replicated_Component))'Img);
   end System_Replication;

   ------------------------
   -- Device_Replication --
   ------------------------

   procedure Device_Replication (D : Replication_Info) is

      Replicated_Component    : Node_Id;
      Corresponding_component : Node_Id;
      Parent_Component        : Node_Id;

   begin
      Put_Line ("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
      Replicated_Component := D.Replicated_Component;
      Corresponding_component := Get_Corresponding_Component
                                 (Replicated_Component);
      Put_Line (kind (Replicated_Component)'Img);
      Put_Line ("Replicate a " & Get_Category_Of_Component
               (Corresponding_component)'Img);
      Put_Line (kind (Corresponding_component)'Img);
      --  Parent_Component := Parent (Replicated_Component);
      Parent_Component := Namespace (Corresponding_component);
      Put_Line (Parent_Component'Img);
      Put_Line (kind (Parent_Component)'Img);
      Put_Line (Get_Name_Of_Entity (Parent_Component));

   end Device_Replication;

   ----------------------
   -- Expand_Component --
   ----------------------

   procedure Expand_Component (C : Replication_Info) is

      pragma Assert (Kind (Get_Corresponding_Component
              (C.Replicated_Component)) = K_Component_Type);

      Has_Category                  : Component_Category;
   begin
      Has_Category := Get_Category_Of_Component
      (Get_Corresponding_Component (C.Replicated_Component));
      case Has_Category is
      when CC_Thread =>
         Thread_Replication (C);
      when CC_Process =>
         Process_Replication (C);
      when CC_Device =>
         Device_Replication (C);
      when CC_System =>
         System_Replication (C);
      when others =>
         null;
      end case;
   end Expand_Component;

   -------------------------------------
   -- Expand_Component_Implementation --
   -------------------------------------

   procedure Expand_Component_Implementation (I : Replication_Info) is
      --  because the kind of the replicated component is K_Subcomponent
      pragma Assert (Kind (Get_Corresponding_Component
                    (I.Replicated_Component)) = K_Component_Implementation);

      Has_Category                  : Component_Category;
   begin
      Has_Category := Get_Category_Of_Component
      (Get_Corresponding_Component (I.Replicated_Component));
      case Has_Category is
      when CC_Thread =>
         Thread_Replication (I);
      when CC_Process =>
         Process_Replication (I);
      when CC_Device =>
         Device_Replication (I);
      when CC_System =>
         System_Replication (I);
      when others =>
         null;
      end case;
   end Expand_Component_Implementation;

   -----------------------------
   -- Extract_Properties_List --
   -----------------------------

   procedure Extract_Properties_List (N : Node_Id) is

      pragma Assert (Kind (N) = K_Component_Implementation
             or else Kind (N) = K_Component_Type);

      Prefix_Replication : constant Name_Id :=
          Get_String_Name ("Replication_Properties");
      Prefix_Description    : constant Name_Id :=
          Get_String_Name ("Replication_Properties::Description");
      Prefix_Number         : constant Name_Id :=
          Get_String_Name ("Replication_Properties::Replica_Number");
      Prefix_Identifiers    : constant Name_Id :=
          Get_String_Name ("Replication_Properties::Replica_Identifiers");
      Prefix_Type           : constant Name_Id :=
        Get_String_Name ("Replication_Properties::Replica_Type");
      Prefix_Consensus_Algorithm : constant Name_Id := Get_String_Name
      ("Replication_Properties::Consensus_Algorithm");

      Property_Node                     : Node_Id;
      Properties_List                   : List_Id := No_List;
      Property_Name                     : Name_Id;
      Applies_To_List                   : List_Id;
      Pointed_Node                      : Node_Id;
      Entity_Of_Property                : Node_Id;
      Consensus_List                    : Node_List;
      Current_Entity_Of_Property        : Node_Id;
      Next_Entity_Of_Property           : Node_Id := No_Node;
      Replicated_Component_List         : Node_List;
      Replicated_Component_Info         : Replication_Info;
   begin
      if not ATNU.Is_Empty (ATN.Properties (N)) then
         Properties_List := ATN.Properties (N);
         Property_Node := First_Node (Properties_List);
         Applies_To_List := List_Items (First_Node
                         (Applies_To_Prop (Property_Node)));
         if not ATNU.Is_Empty (Applies_To_List) then
            Pointed_Node := First_Node (Applies_To_List);
            if Kind (Pointed_Node) = K_Array_Selection then
               Current_Entity_Of_Property := Corresponding_Entity
                                  (Identifier (Pointed_Node));
            else
               Current_Entity_Of_Property := Corresponding_Entity
                                  (Pointed_Node);
            end if;
         end if;

         --  loop to traverse all replicated component
         while Present (Current_Entity_Of_Property) and then
               not Is_Node_Of_Node_List (Current_Entity_Of_Property,
               Replicated_Component_List) loop

            Remove_Nodes_From_List (Consensus_List);
            Replicated_Component_Info.Replicated_Component :=
                                  Current_Entity_Of_Property;
            --  loop to traverse replication_properties related to
            --  the current replicated component
            while Present (Property_Node) loop
               Applies_To_List := List_Items (First_Node
                               (Applies_To_Prop (Property_Node)));
               --  extract the node to each the property is applied
               if not ATNU.Is_Empty (Applies_To_List) then
                  Pointed_Node := First_Node (Applies_To_List);
                  if Kind (Pointed_Node) = K_Array_Selection then
                     Entity_Of_Property := Corresponding_Entity
                         (Identifier (Pointed_Node));
                  else
                     Entity_Of_Property := Corresponding_Entity
                                          (Pointed_Node);
                  end if;
               end if;
               --  if it consists on the current component we extract the list
               --  of properties, else save the next replicated component
               if (Entity_Of_Property = Current_Entity_Of_Property)
                  or else (kind (Current_Entity_Of_Property) = K_Subcomponent
                  and then Find_Feature (Get_Corresponding_Component
                     (Current_Entity_Of_Property), Identifier
                     (Entity_Of_Property)) /= No_Node)
               then
                  Property_Name := Get_Name_Of_Entity (Property_Node, true);
                  if Is_Prefix (Prefix_Description, Property_Name) then
                     Replicated_Component_Info.Description :=
                       Get_String_Name (Get_Replication_Description
                       (Property_Node, Current_Entity_Of_Property));
                     ATNU.Remove_Node_From_List (Property_Node,
                                                 Properties_List);
                  elsif Is_Prefix (Prefix_Number, Property_Name) then
                     Replicated_Component_Info.Number :=
                        Get_Replication_Replica_Number
                        (Property_Node, Current_Entity_Of_Property);
                     ATNU.Remove_Node_From_List (Property_Node,
                                                 Properties_List);
                  elsif Is_Prefix (Prefix_Identifiers, Property_Name) then
                     Replicated_Component_Info.Identifiers :=
                            Get_Replication_Replica_Identifiers
                            (Property_Node, Current_Entity_Of_Property);
                     ATNU.Remove_Node_From_List (Property_Node,
                                                 Properties_List);
                  elsif Is_Prefix (Prefix_Type, Property_Name) then
                     Replicated_Component_Info.Replication_Type :=
                      Get_Replication_Replica_Type
                      (Property_Node, Current_Entity_Of_Property);
                     ATNU.Remove_Node_From_List (Property_Node,
                                                 Properties_List);
                  elsif Is_Prefix (Prefix_Consensus_Algorithm,
                                   Property_Name)
                  then
                     Get_Replication_Consensus_Algorithm
                     (Property_Node, Consensus_List);
                     ATNU.Remove_Node_From_List (Property_Node,
                                                 Properties_List);
                  end if;
               else
                  if Is_Prefix (Prefix_Replication, Get_Name_Of_Entity
                     (Property_Node, true)) and then
                     not Is_Node_Of_Node_List (Entity_Of_Property,
                         Replicated_Component_List) and then
                    kind (Entity_Of_Property) = k_Subcomponent
                  then
                     Next_Entity_Of_Property := Entity_Of_Property;
                  end if;
               end if;
               Property_Node := Next_Node (Property_Node);
            end loop;

            --  To check the validity of all the replication information
            Replicated_Component_Info.Consensus_Algo := Consensus_List;
            Check_Replication_Validity (Replicated_Component_Info);
            --  update the list of the replicated component
            ATNU.Append_Node_To_Node_List (Current_Entity_Of_Property,
            Replicated_Component_List, True);

            --  go to the next one to extract their properties
            if (Next_Entity_Of_Property /= No_Node) then
               Current_Entity_Of_Property := Next_Entity_Of_Property;
               Property_Node := First_Node (Properties_List);
            end if;

         end loop;
      end if;
   end Extract_Properties_List;

   ------------------------------------
   --  Check_Consensus_Algo_Validity --
   ------------------------------------

   function Check_Consensus_Algo_Validity
      (Replicated_Component : Node_Id;
      Consensus_List       : Node_List)
      return boolean is
      Property_Node  : Node_Id := No_Node;
      Features : Node_List;
      Applies_To_List      : List_Id;
      Entity_Of_Property   : Node_Id;
      Pointed_Node         : Node_Id := No_Node;
      Is_Valid             : boolean;
      Current_Feature      : Node_Id;
      Number_Of_Feature    : Integer := 0;
      Exist                : boolean;
   begin
      if Consensus_List.First /= No_Node then
         Property_Node := Consensus_List.First;
      end if;
      if Present (Replicated_Component) then
         Features := Find_All_Features
                  (Get_Corresponding_Component (Replicated_Component));
         if (Features.First /= No_Node) then
            Current_Feature := Features.First;
            --  check if there is a consensus algo related to each feature
            --  having the out or inout mode or is a data access
            while Present (Current_Feature) loop
               if ((Kind (Current_Feature) = K_Port_Spec and then
                  Is_Out (Current_Feature)) or else
                  ((Kind (Current_Feature) = K_Subcomponent_Access) and then
                     Get_Category_Of_Component
                       (Entity (Entity_Ref
                                  (Current_Feature))) = CC_DATA))
               then
                  Number_Of_Feature := Number_Of_Feature + 1;
                  Exist := false;
                  Property_Node := Consensus_List.First;
                  while Present (Property_Node) loop
                     Applies_To_List := List_Items (First_Node
                        (Applies_To_Prop (Property_Node)));
                     if not ATNU.Is_Empty (Applies_To_List) then
                        Pointed_Node := First_Node (Applies_To_List);
                        if Kind (Pointed_Node) = K_Array_Selection then
                           Entity_Of_Property := Corresponding_Entity
                                        (Identifier (Pointed_Node));
                        else
                           Entity_Of_Property := Corresponding_Entity
                                       (Pointed_Node);
                        end if;
                     end if;
                     if (Entity_Of_Property = Current_Feature) then
                        Exist := true;
                        exit;
                     else
                        Property_Node := Next_Entity (Property_Node);
                     end if;
                  end loop;
                  if not Exist then
                     Display_Error ("No Specified Consensus Algorithm for the "
                     & "feature " & Get_Name_Of_Entity (Current_Feature)
                     & " when replicating the component "
                     & Get_Name_Of_Entity (Replicated_Component),
                     Fatal => False, Warning => False);
                     exit;
                  end if;
               else
                  null; --  other types of features are not treated
               end if;
               Current_Feature := Next_Entity (Current_Feature);
            end loop;
         end if;
      end if;
      --  To guarantee that there is no redundant consensus algorithm applied
      --  for the same feature
      if Number_Of_Feature = ATNU.Length (Consensus_List) then
         Is_Valid := true;
      else
         Is_Valid := false;
      end if;
      return Is_Valid;
   end Check_Consensus_Algo_Validity;

   ---------------------------------
   --  Check_Replication_Validity --
   ---------------------------------

   procedure Check_Replication_Validity (Record_Info : Replication_Info) is
      Replicated_Component : Node_Id;
      Description          : Name_Id;
      Number               : Unsigned_Long_long;
      Replication_Type     : Replication_Types := Replication_None;
      Identifiers_List     : List_Id;
      Consensus_List       : Node_List;
      Is_Valid             : boolean := False;
   begin
      Replicated_Component := Record_Info.Replicated_Component;
      Description := Record_Info.Description;
      Number := Record_Info.Number;
      Identifiers_List := Record_Info.Identifiers;
      Replication_Type := Record_Info.Replication_Type;
      Consensus_List := Record_Info.Consensus_Algo;

      if Record_Info.Description = No_Name or else
        Get_Name_String (Description) = ""
      then
         Display_Error ("Cannot expand AADL models: No Replication "
         & "Description Specified for the component "
         & Get_Name_Of_Entity (Record_Info.Replicated_Component),
         Fatal => False, Warning => False);
      else
         if Number < 2 then
            Display_Error ("Cannot expand AADL models: " & Image (Number)
            & " : Invalid number of replicas", Fatal => False,
            Warning => False);
         else
            if Replication_Type = Replication_None then
               Display_Error ("Cannot expand AADL models: No Replication"
               & "Type Specified for the component "
               & Get_Name_Of_Entity (Record_Info.Replicated_Component),
               Fatal => False, Warning => False);
            elsif Replication_Type = Replication_Passive then
               Display_Error ("Cannot expand AADL models: We do not yet"
               & "deal with passive replication", Fatal => False,
               Warning => False);
            else
               if Unsigned_Long_Long (ATNU.Length (Identifiers_List)) = 0
               then
                  Display_Error ("Cannot expand AADL models: The number of "
                  & "identifiers is not specified for the component "
                  & Get_Name_Of_Entity (Record_Info.Replicated_Component),
                  Fatal => False, Warning => False);
               elsif Unsigned_Long_Long (ATNU.Length (Identifiers_List))
                 /= Number
               then
                  Display_Error ("Cannot expand AADL models: The number of "
                  & "identifiers (" & Image (ATNU.Length (Identifiers_List))
                  & ") does not match with the number of replicas ("
                  & Image (Number) & ")", Fatal => False, Warning => False);
               else
                  if not Check_Consensus_Algo_Validity (Replicated_Component,
                                                        Consensus_List)
                  then
                     Display_Error ("Cannot expand AADL models: Invalid List"
                     & " of consensus Algorithm", Fatal => False
                     , Warning => False);
                  else
                     Is_Valid := true;
                  end if;
               end if;
            end if;
         end if;
      end if;
      if (Is_Valid) then
         if Kind (Get_Corresponding_Component (Replicated_Component))
           = K_Component_Type
         then
            Expand_Component (Record_Info);
         elsif Kind (Get_Corresponding_Component
                       (Replicated_Component)) = K_Component_Implementation
         then
            Expand_Component_Implementation (Record_Info);
         else
            Display_Error ("Invalid_Type", Fatal => False, Warning => False);
         end if;
      end if;
   end Check_Replication_Validity;

end Ocarina.Backends.Replication_Expander;
