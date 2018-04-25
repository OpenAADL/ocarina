------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . P O _ H I _ A D A . T Y P E S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Values;
with Ocarina.Backends.PO_HI_Ada.Mapping;
with Ocarina.Backends.PO_HI_Ada.Runtime;

package body Ocarina.Backends.PO_HI_Ada.Types is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.Ada_Values;
   use Ocarina.Backends.PO_HI_Ada.Mapping;
   use Ocarina.Backends.PO_HI_Ada.Runtime;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;

   function Length_Spec (E : Node_Id) return Node_Id;
   --  Length of an array type to be exported to C code

   -----------------
   -- Length_Spec --
   -----------------

   function Length_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      pragma Assert (Get_Data_Representation (E) = Data_Array);

      N :=
        Make_Subprogram_Specification
          (Defining_Identifier => Make_Defining_Identifier (SN (S_Length)),
           Parameter_Profile   =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier => Make_Defining_Identifier (PN (P_A)),
                   Subtype_Mark        => Map_Ada_Defining_Identifier (E))),
           Return_Type => RE (RE_Integer));

      return N;
   end Length_Spec;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

      function Feature_Spg_Spec (E : Node_Id) return Node_Id;
      --  Builds a spec for a protected object procedure from an AADL
      --  subprogram spec E.

      function Pragma_Export_Length (E : Node_Id) return Node_Id;
      --  Length of an array type to be exported to C code

      ----------------------
      -- Feature_Spg_Spec --
      ----------------------

      function Feature_Spg_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
         N       : Node_Id;
         Mode    : Mode_Id;
         P       : Node_Id;
         Spg     : Node_Id;
      begin
         pragma Assert
           (Kind (E) = K_Subprogram_Spec_Instance
            or else Kind (E) = K_Subcomponent_Access_Instance);

         Spg := Corresponding_Instance (E);

         pragma Assert (AAU.Is_Subprogram (Spg));

         if not AAU.Is_Empty (Features (Spg)) then
            P := First_Node (Features (Spg));

            while Present (P) loop
               if Kind (P) = K_Parameter_Instance then

                  --  Create a parameter specification

                  if Is_In (P) and then Is_Out (P) then
                     Mode := Mode_Inout;
                  elsif Is_Out (P) then
                     Mode := Mode_Out;
                  else
                     Mode := Mode_In;
                  end if;

                  if No (Backend_Node (Identifier (E))) then
                     Visit_Component_Instance (Corresponding_Instance (P));
                  end if;

                  N :=
                    Make_Parameter_Specification
                      (Defining_Identifier => Map_Ada_Defining_Identifier (P),
                       Subtype_Mark        =>
                         Map_Ada_Data_Type_Designator
                           (Corresponding_Instance (P)),
                       Parameter_Mode => Mode);
                  Append_Node_To_List (N, Profile);
               end if;

               P := Next_Node (P);
            end loop;
         end if;

         N :=
           Make_Subprogram_Specification
             (Defining_Identifier => Map_Ada_Defining_Identifier (E),
              Parameter_Profile   => Profile);

         return N;
      end Feature_Spg_Spec;

      --------------------------
      -- Pragma_Export_Length --
      --------------------------

      function Pragma_Export_Length (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         pragma Assert (Get_Data_Representation (E) = Data_Array);

         N :=
           Make_Pragma_Statement
             (Pragma_Export,
              Make_List_Id
                (Make_Defining_Identifier (PN (P_C)),
                 Make_Defining_Identifier (SN (S_Length)),
                 Make_Literal
                   (New_String_Value (Map_Exported_Length_Symbol (E)))));

         return N;
      end Pragma_Export_Length;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Visit_Architecture_Instance (E);

            when K_Component_Instance =>
               Visit_Component_Instance (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Architecture_Instance --
      ---------------------------------

      procedure Visit_Architecture_Instance (E : Node_Id) is
      begin
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Data =>
               Visit_Data_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         Data_Representation   : Supported_Data_Representation;
         Language_Type         : Supported_Source_Language;
         N                     : Node_Id;
         S                     : Node_Id;
         Name                  : Name_Id;
         Actual_Data_Size      : Unsigned_Long_Long;
         Data_Size             : Size_Type;
         Number_Representation : constant Supported_Number_Representation :=
           Get_Number_Representation (E);
         Is_Signed : constant Boolean := Number_Representation = Signed;
      begin
         --  Do not generate Ada type more than once

         if No (Get_Handling (E, By_Name, H_Ada_Type_Spec)) then

            --  Add a fake handling for now, to avoid infinite recursion

            Set_Handling (E, By_Name, H_Ada_Type_Spec, No_Node + 1);

            Language_Type := Get_Source_Language (E);

            if Language_Type = Language_ASN1 then
               --  If the type is defined through an ASN.1
               --  specification, then we assume the type is generated
               --  in the ASN1_Types package. We simply bind this type
               --  to the specification.

               Add_With_Package
                 (E            => RU (RU_ASN1_Types),
                  Used         => True,
                  Warnings_Off => True,
                  Elaborated   => True);

               Name := Get_Type_Source_Name (E);
               if Name = No_Name then
                  Display_Located_Error
                    (Loc (E),
                     "ASN1 types require use of the Type_Source_Name property",
                     Fatal => True);
               end if;

               N :=
                 Make_Designator
                   (Name,
                    Fully_Qualified_Name (RU (RU_ASN1_Types)));

            elsif Language_Type = Language_Ada_95 then
               --  If the type is defined through as an Ada type, then
               --  we simply drag a dependency onto the Ada package
               --  that hosts this type.

               Name := Get_Type_Source_Name (E);

               if Name = No_Name then
                  Display_Located_Error
                    (Loc (E),
                     "Ada opaque types require the definition of the " &
                     "'Type_Source_Name' property",
                     Fatal => True);
               end if;

               declare
                  U : constant Name_Id := Unit_Name (Name);
                  L : constant Name_Id := Local_Name (Name);
                  P : Node_Id;
               begin
                  if U /= No_Name then
                     --  The user provided a fully qualified name that
                     --  is not prefixed by Standard, add this fully
                     --  qualified name in the package

                     P := Make_Designator (U);
                     ADN.Set_Corresponding_Node
                       (ADN.Defining_Identifier (P),
                        New_Node (ADN.K_Package_Specification));
                     Add_With_Package (P);

                     N := Make_Designator (L);
                     Set_Homogeneous_Parent_Unit_Name (N, P);
                  else
                     --  Otherwise, simply refer to Standard package

                     N := Make_Designator (L);
                     Set_Homogeneous_Parent_Unit_Name (N, RU (RU_Standard));
                  end if;

                  N :=
                    Make_Full_Type_Declaration
                      (Defining_Identifier => Map_Ada_Defining_Identifier (E),
                       Type_Definition     =>
                         Make_Derived_Type_Definition (N, Is_Subtype => True),
                       Is_Subtype => True);
               end;
            else
               --  Otherwise, we extract from the Data_Model specific
               --  properties the exact nature of the type and
               --  generate its definition.

               Data_Representation := Get_Data_Representation (E);
               Data_Size           := Get_Data_Size (E);
               Actual_Data_Size    := To_Bytes (Data_Size);

               case Data_Representation is
                  when Data_Array =>
                     declare
                        Dimension : constant ULL_Array := Get_Dimension (E);
                        RC : constant List_Id   := New_List (ADN.K_List_Id);
                     begin
                        Visit
                          (ATN.Entity (ATN.First_Node (Get_Base_Type (E))));

                        for Index in Dimension'Range loop
                           N :=
                             Make_Range_Constraint
                               (Make_Literal (New_Integer_Value (1, 1, 10)),
                                Make_Literal
                                  (New_Integer_Value
                                     (Dimension (Index),
                                      1,
                                      10)),
                                RE (RE_Positive));
                           Append_Node_To_List (N, RC);
                        end loop;

                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Array_Type_Definition
                                 (Range_Constraints    => RC,
                                  Component_Definition =>
                                    Map_Ada_Data_Type_Designator
                                      (ATN.Entity
                                         (ATN.First_Node
                                            (Get_Base_Type (E))))));
                     end;

                  when Data_Boolean =>
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier =>
                            Map_Ada_Defining_Identifier (E),
                          Type_Definition =>
                            Make_Derived_Type_Definition (RE (RE_Boolean)));

                  when Data_Bounded_Array =>
                     raise Program_Error; --  XXX

                  when Data_Character =>
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier =>
                            Map_Ada_Defining_Identifier (E),
                          Type_Definition =>
                            Make_Derived_Type_Definition (RE (RE_Character)));

                  when Data_Wide_Character =>
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier =>
                            Map_Ada_Defining_Identifier (E),
                          Type_Definition =>
                            Make_Derived_Type_Definition
                              (RE (RE_Wide_Character)));

                  when Data_Enum =>
                     declare
                        Enumerators : constant Name_Array :=
                          Get_Enumerators (E);
                        Enumeration_List : constant List_Id :=
                          New_List (ADN.K_Enumeration_Literals);
                     begin
                        for J in Enumerators'Range loop
                           N := Make_Defining_Identifier (Enumerators (J));
                           Append_Node_To_List (N, Enumeration_List);
                        end loop;

                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Enumeration_Type_Definition
                                 (Enumeration_List));
                     end;

                  when Data_Float =>
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier =>
                            Map_Ada_Defining_Identifier (E),
                          Type_Definition =>
                            Make_Derived_Type_Definition (RE (RE_Long_Float)));

                  when Data_Fixed =>
                     declare
                        Data_Digits : constant Unsigned_Long_Long :=
                          Get_Data_Digits (E);
                        Data_Scale : constant Unsigned_Long_Long :=
                          Get_Data_Scale (E);
                     begin
                        if Data_Digits /= 0 and then Data_Scale /= 0 then
                           N :=
                             Make_Full_Type_Declaration
                               (Defining_Identifier =>
                                  Map_Ada_Defining_Identifier (E),
                                Type_Definition =>
                                  Make_Decimal_Type_Definition
                                    (Data_Digits,
                                     Data_Scale));

                        else
                           if Data_Digits = 0 then
                              Display_Located_Error
                                (Loc (E),
                                 "Missing digit number of fixed point type",
                                 Fatal => True);
                           end if;

                           if Data_Scale = 0 then
                              Display_Located_Error
                                (Loc (E),
                                 "Missing the scale of fixed point type!",
                                 Fatal => True);
                           end if;
                        end if;
                     end;

                  when Data_Integer =>
                     if Data_Size.S = 0 then
                        --  If no size info is given, we default to a
                        --  standard integer

                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Derived_Type_Definition (RE (RE_Integer)));

                     elsif Actual_Data_Size = 1 and then Is_Signed then
                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (RE (RE_Integer_8)));

                     elsif Actual_Data_Size = 1 and then not Is_Signed then
                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (RE (RE_Unsigned_8)));

                     elsif Actual_Data_Size = 2 and then Is_Signed then
                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (RE (RE_Integer_16)));

                     elsif Actual_Data_Size = 2 and then not Is_Signed then
                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (RE (RE_Unsigned_16)));

                     elsif Actual_Data_Size = 4 and then Is_Signed then
                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (RE (RE_Integer_32)));

                     elsif Actual_Data_Size = 4 and then not Is_Signed then
                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (RE (RE_Unsigned_32)));

                     elsif Actual_Data_Size = 8 and then Is_Signed then
                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (RE (RE_Integer_64)));

                     elsif Actual_Data_Size = 8 and then not Is_Signed then
                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (RE (RE_Unsigned_64)));

                     else
                        Display_Located_Error
                          (Loc (E),
                           "Unsupported data size" & Actual_Data_Size'Img,
                           Fatal => True);
                     end if;

                  when Data_String =>
                     declare
                        Dimension : constant ULL_Array := Get_Dimension (E);
                     begin
                        N :=
                          Make_Package_Instantiation
                            (Defining_Identifier =>
                               Map_Ada_Package_Identifier (E),
                             Generic_Package =>
                               RU
                               (RU_Ada_Strings_Bounded_Generic_Bounded_Length),
                             Parameter_List =>
                               Make_List_Id
                                 (Make_Literal
                                    (New_Integer_Value
                                       (Dimension (Dimension'First),
                                        1,
                                        10))));
                        Append_Node_To_List
                          (N,
                           ADN.Visible_Part (Current_Package));

                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Is_Subtype      => True,
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (Make_Selected_Component
                                    (Map_Ada_Package_Identifier (E),
                                     Make_Defining_Identifier
                                       (TN (T_Bounded_String))),
                                  Is_Subtype => True));
                     end;

                  when Data_Wide_String =>
                     declare
                        Dimension : constant ULL_Array := Get_Dimension (E);
                        RU_Wi_Str : constant RU_Id     :=
                          RU_Ada_Strings_Wide_Bounded_Generic_Bounded_Length;
                     begin
                        N :=
                          Make_Package_Instantiation
                            (Defining_Identifier =>
                               Map_Ada_Package_Identifier (E),
                             Generic_Package => RU (RU_Wi_Str),
                             Parameter_List  =>
                               Make_List_Id
                                 (Make_Literal
                                    (New_Integer_Value
                                       (Dimension (Dimension'First),
                                        1,
                                        10))));
                        Append_Node_To_List
                          (N,
                           ADN.Visible_Part (Current_Package));

                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_Ada_Defining_Identifier (E),
                             Is_Subtype      => True,
                             Type_Definition =>
                               Make_Derived_Type_Definition
                                 (Make_Selected_Component
                                    (Map_Ada_Package_Identifier (E),
                                     Make_Defining_Identifier
                                       (TN (T_Bounded_Wide_String))),
                                  Is_Subtype => True));
                     end;

                  when Data_Struct | Data_With_Accessors =>
                     declare
                        Components : constant List_Id :=
                          New_List (ADN.K_Component_List);
                        C         : Node_Id := First_Node (Subcomponents (E));
                        Visible_P : List_Id;
                        Private_P : List_Id;
                        I         : Unsigned_Long_Long;
                        O         : Node_Id;
                     begin
                        --  Build the component list

                        while Present (C) loop
                           --  Generate the Ada type corresponding to the
                           --  subcomponent.

                           Visit (Corresponding_Instance (C));

                           --  Make the record or private type component

                           if AAU.Is_Data (Corresponding_Instance (C)) then
                              N :=
                                Make_Component_Declaration
                                  (Defining_Identifier =>
                                     Map_Ada_Defining_Identifier (C),
                                   Subtype_Indication =>
                                     Map_Ada_Data_Type_Designator
                                       (Corresponding_Instance (C)));
                              Append_Node_To_List (N, Components);
                           end if;

                           C := Next_Node (C);
                        end loop;

                        if Data_Representation = Data_Struct then
                           --  Record type

                           N :=
                             Make_Full_Type_Declaration
                               (Defining_Identifier =>
                                  Map_Ada_Defining_Identifier (E),
                                Type_Definition =>
                                  Make_Record_Type_Definition
                                    (Make_Record_Definition (Components)));
                        else
                           --  Protected type

                           declare
                              CCP : Supported_Concurrency_Control_Protocol;
                           begin
                              --  Per the Ravenscar profile, all
                              --  protected objects are supposed to be
                              --  PCP, ensure this is true also for
                              --  the AADL model.

                              CCP := Get_Concurrency_Protocol (E);
                              if CCP /= Priority_Ceiling then
                                 Display_Located_Error
                                   (Loc (E),
                                    "Incompatible concurrency protocol, " &
                                    "PolyORB-HI/Ada requires " &
                                    "Priority_Ceiling",
                                    True);
                              end if;
                           end;

                           Visible_P := New_List (ADN.K_Declaration_List);
                           Private_P := Components;

                           S := First_Node (Features (E));

                           while Present (S) loop
                              --  We are sure that S is of kind
                              --  K_Subprogram_Spec_Instance. Otherwise,
                              --  an error whould be raised when trying
                              --  to find the data type.

                              --  Build a subprogram spec and append it
                              --  to the visible part of the protected
                              --  type.

                              N := Feature_Spg_Spec (S);
                              Bind_AADL_To_Feature_Subprogram
                                (Identifier (S),
                                 N);
                              Append_Node_To_List (N, Visible_P);

                              S := Next_Node (S);
                           end loop;

                           --  Build the private type spec

                           N :=
                             Make_Protected_Object_Spec
                               (Defining_Identifier =>
                                  Map_Ada_Defining_Identifier (E),
                                Visible_Part => Visible_P,
                                Private_Part => Private_P,
                                Is_Type      => True);

                           I := Get_Priority_Celing_Of_Data_Access (E);

                           if I /= 0 then
                              --  The data component defines a priority,
                              --  use it for the ceiling priority of the
                              --  protected object.

                              O :=
                                Make_Pragma_Statement
                                  (Pragma_Priority,
                                   Make_List_Id (Map_Ada_Priority (I)));
                              Append_Node_To_List (O, ADN.Private_Part (N));

                           else
                              --  The data component defines no priority,
                              --  use System.Priority'Last for the
                              --  ceiling priority of the protected
                              --  object.

                              Display_Located_Error
                                (Loc (E),
                                 "No  priority defined, will use" &
                                 " default value: System.Priority'Last",
                                 False,
                                 True);

                              O :=
                                Make_Pragma_Statement
                                  (Pragma_Priority,
                                   Make_List_Id
                                     (Make_Attribute_Designator
                                        (RE (RE_Priority),
                                         A_Last)));
                              Append_Node_To_List (O, ADN.Private_Part (N));
                           end if;
                        end if;
                     end;

                  when Data_Union =>
                     Display_Located_Error
                       (Loc (E),
                        "unsupported data type (" &
                        Supported_Data_Representation'Image
                          (Data_Representation) &
                        ")",
                        Fatal => True);

                  when Data_None =>
                     Display_Located_Error
                       (Loc (E),
                        "unspecified data representation",
                        Fatal => True);
               end case;
            end if;

            --  Mark the data type as being handled and append it to
            --  the handled list.

            Set_Handling (E, By_Name, H_Ada_Type_Spec, N);

            --  In the case of a data type with accessor, visit the
            --  parameters of its features subprograms. It is
            --  important to do this *after* marking the type as
            --  handled, to avoid endless loops and *before* adding
            --  the type declaration to the package statements because
            --  the declaration order of type is important in Ada.

            if Data_Representation = Data_With_Accessors then
               S := First_Node (Features (E));

               while Present (S) loop
                  Visit (Corresponding_Instance (S));

                  S := Next_Node (S);
               end loop;
            end if;

            --  Append the type declaration to the package spec

            if Language_Type /= Language_ASN1 then
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            end if;

            --  If the user specified a data size in the AADL model,
            --  generate a corresponding Ada clause.

            if Get_Data_Size (E) /= Null_Size then
               N :=
                 Make_Attribute_Definition_Clause
                   (Map_Ada_Defining_Identifier (E),
                    A_Size,
                    Make_Literal
                      (New_Integer_Value
                         (To_Bits (Get_Data_Size (E)),
                          1,
                          10)));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            end if;

            if Get_Data_Representation (E) /= Data_With_Accessors then
               declare
                  Data_Size : Unsigned_Long_Long;
               begin
                  if Get_Data_Size (E) /= Null_Size then
                     Data_Size := To_Bits (Get_Data_Size (E));
                  else
                     Data_Size := Estimate_Data_Size (E);
                  end if;

                  --  A documentary comment for the data size

                  N :=
                    Message_Comment
                      (Get_Name_String
                         (To_Ada_Name (AIN.Name (Identifier (E)))) &
                       "'Object_Size ~=" &
                       Unsigned_Long_Long'Image (Data_Size) &
                       " bits");
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
               end;
            end if;

            if Get_Data_Representation (E) = Data_Struct then
               N :=
                 Make_Attribute_Definition_Clause
                   (Map_Ada_Defining_Identifier (E),
                    Attribute_Designator => A_Alignment,
                    Expression => Make_Literal (New_Integer_Value (8, 1, 10)));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            end if;

            --  Array types have also a subprogram 'Length' which is
            --  generated for use in other languages in which arrays
            --  are not aware of their lengths (such as C).

            if Get_Data_Representation (E) = Data_Array then
               N := Length_Spec (E);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Add a pragma Export for 'Length' in order for it to
               --  be seen by C code.

               N := Pragma_Export_Length (E);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            end if;
         end if;

         --  Bind the type to its mapping

         Bind_AADL_To_Type_Definition
           (Identifier (E),
            Get_Handling (E, By_Name, H_Ada_Type_Spec));

         --  Declare the "default value" of a type.

         --  Note: If a default value can be computed, then this value
         --  is declared as a 'constant' so that the user cannot
         --  modify it.

         if No (Get_Handling (E, By_Name, H_Ada_Type_Default_Value)) then
               Data_Representation := Get_Data_Representation (E);

               --  We generate default values for all types except
               --  protected types.

               if Data_Representation /= Data_With_Accessors then
                  declare
                     Default_Value : constant Node_Id :=
                       Get_Ada_Default_Value (E);
                  begin
                     N :=
                       Make_Object_Declaration
                       (Defining_Identifier =>
                          Map_Ada_Default_Value_Identifier (E),
                        Constant_Present  => Present (Default_Value),
                        Object_Definition => Map_Ada_Defining_Identifier (E),
                        Expression        => Default_Value);

                     Set_Handling (E, By_Name, H_Ada_Type_Default_Value, N);
                     Append_Node_To_List
                       (N, ADN.Visible_Part (Current_Package));
                  end;
               else
                  N := No_Node;
               end if;
         end if;

         --  Bind the type to its default value if a default value has
         --  been generated.

         if Present (N) then
            Bind_AADL_To_Default_Value
              (Identifier (E),
               Get_Handling (E, By_Name, H_Ada_Type_Default_Value));
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Types_Spec;

         --  Start recording the handling since they have to be reset
         --  for each node.

         Start_Recording_Handlings;

         --  Visit all the subcomponents of the process

         Visit_Subcomponents_Of (E);

         --  Unmark all the marked types

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         F        : Node_Id;
      begin
         --  Declare all necessary data types

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Present (Corresponding_Instance (F)) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Visit all the call sequences of the subprogram

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
      begin
         Push_Entity (Ada_Root);

         --  Visit all the subcomponents of the system

         Visit_Subcomponents_Of (E);

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         F        : Node_Id;
      begin
         --  Declare all necessary data types. We cannot rely only on
         --  subprogram calls to generate necessary data type becaus
         --  threads may not contain subprogram calls.

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then AIN.Is_Data (F) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Visit all the call sequences of the thread

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Thread_Instance;

   end Package_Spec;

   ------------------
   -- Package_Body --
   ------------------

   package body Package_Body is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

      function Feature_Spg_Body (E : Node_Id; Data : Node_Id) return Node_Id;
      --  Builds a body for a protected object procedure from an AADL
      --  subprogram spec E.

      function Length_Body (E : Node_Id) return Node_Id;
      --  Body of the 'Length' function associated to array type E

      ----------------------
      -- Feature_Spg_Body --
      ----------------------

      function Feature_Spg_Body (E : Node_Id; Data : Node_Id) return Node_Id is
         N            : Node_Id;
         Call_Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
         Param        : Node_Id;
         C_Access     : Node_Id;
         D            : Node_Id;
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Spg          : Node_Id;
      begin
         pragma Assert
           (Kind (E) = K_Subprogram_Spec_Instance
            or else Kind (E) = K_Subcomponent_Access_Instance);

         Spg := Corresponding_Instance (E);

         pragma Assert (AAU.Is_Subprogram (Spg));

         --  The body of a subprogram contained in a protected type
         --  contains simply a call to the user implementation of the
         --  corresponding AADL subprogram.

         --  Since the subprogram has Data Access to its containing
         --  data component. It gives all the protected fields of the
         --  protected type as IN, OUT or IN OUT (depending on the
         --  data access property) parameters to the subprogram
         --  implementation call. Therefore the parameters for the
         --  subprogram implementation call are:

         --  1 - The list of the AADL subprogram parameters:

         if not AAU.Is_Empty (Features (Spg)) then
            Param := First_Node (Features (Spg));

            while Present (Param) loop
               if Kind (Param) = K_Parameter_Instance then
                  --  Create a parameter association

                  N :=
                    Make_Parameter_Association
                      (Selector_Name    => Map_Ada_Defining_Identifier (Param),
                       Actual_Parameter =>
                         Map_Ada_Defining_Identifier (Param));
                  Append_Node_To_List (N, Call_Profile);
               end if;

               Param := Next_Node (Param);
            end loop;
         end if;

         --  2 - The list of all record fields given

         --  FIXME: Respect the mapping rules by setting the correct
         --  parameter orientation. For now all parameter are
         --  considered IN OUT. Provide all necessary routines
         --  (passing through intermediate variables, to prevent the
         --  user from cheating).

         if not AAU.Is_Empty (Features (Spg))
           and then not Is_Priority_Shifter (Data)
         then

            C_Access := First_Node (Features (Spg));

            while Present (C_Access) loop
               if Kind (C_Access) = K_Subcomponent_Access_Instance then
                  D := Corresponding_Instance (C_Access);

                  if not AAU.Is_Empty (Subcomponents (D)) then
                     Param := First_Node (Subcomponents (D));

                     while Present (Param) loop
                        --  Create a parameter association

                        if AAU.Is_Data (Corresponding_Instance (Param)) then
                           N :=
                             Make_Parameter_Association
                               (Selector_Name =>
                                  Map_Ada_Protected_Aggregate_Identifier
                                    (C_Access,
                                     Param),
                                Actual_Parameter =>
                                  Map_Ada_Defining_Identifier (Param));

                           Append_Node_To_List (N, Call_Profile);
                        end if;

                        Param := Next_Node (Param);
                     end loop;
                  end if;
               end if;

               C_Access := Next_Node (C_Access);
            end loop;
         end if;

         --  Call the implementation subprogram with the built profile

         N :=
           Make_Subprogram_Call
             (Extract_Designator
                (ADN.Subprogram_Node
                   (Backend_Node (Identifier (Corresponding_Instance (E))))),
              Call_Profile);
         Append_Node_To_List (N, Statements);

         --  Build the subprogram implementation

         N :=
           Make_Subprogram_Implementation
             (ADN.Feature_Subprogram_Node (Backend_Node (Identifier (E))),
              No_List,
              Statements);

         return N;
      end Feature_Spg_Body;

      -----------------
      -- Length_Body --
      -----------------

      function Length_Body (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         N :=
           Make_Subprogram_Implementation
             (Length_Spec (E),
              No_List,
              Make_List_Id
                (Make_Return_Statement
                   (Make_Attribute_Designator
                      (Make_Defining_Identifier (PN (P_A)),
                       A_Length))));

         return N;
      end Length_Body;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Visit_Architecture_Instance (E);

            when K_Component_Instance =>
               Visit_Component_Instance (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Architecture_Instance --
      ---------------------------------

      procedure Visit_Architecture_Instance (E : Node_Id) is
      begin
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Data =>
               Visit_Data_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         Data_Representation : Supported_Data_Representation;
         N                   : Node_Id;
      begin
         --  Do not generate Ada type more than once

         if No (Get_Handling (E, By_Name, H_Ada_Type_Body)) then

            --  Add a fake handling for now, to avoid infinite recursion
            Set_Handling (E, By_Name, H_Ada_Type_Body, No_Node + 1);

            Data_Representation := Get_Data_Representation (E);

            case Data_Representation is
               when Data_Array =>
                  N := Length_Body (E);

                  --  Append the type declaration to the package body

                  Append_Node_To_List (N, ADN.Statements (Current_Package));

                  --  Mark the data type as being handled

                  Set_Handling (E, By_Name, H_Ada_Type_Body, N);

               when Data_With_Accessors =>
                  declare
                     Statements : constant List_Id :=
                       New_List (ADN.K_Statement_List);
                     C : Node_Id := First_Node (Subcomponents (E));
                     S : Node_Id;
                  begin
                     --  Visit the subcomponents

                     while Present (C) loop
                        Visit (Corresponding_Instance (C));
                        C := Next_Node (C);
                     end loop;

                     --  Protected type

                     S := First_Node (Features (E));

                     while Present (S) loop
                        --  Build a subprogram spec and append it to
                        --  the visible part of the protected type.

                        N := Feature_Spg_Body (S, E);
                        Append_Node_To_List (N, Statements);

                        S := Next_Node (S);
                     end loop;

                     --  Build the private type body

                     N :=
                       Make_Protected_Object_Body
                         (Defining_Identifier =>
                            Map_Ada_Defining_Identifier (E),
                          Statements => Statements);

                     --  Append the type declaration to the package body

                     Append_Node_To_List (N, ADN.Statements (Current_Package));

                     --  Mark the data type as being handled

                     Set_Handling (E, By_Name, H_Ada_Type_Body, N);
                  end;

               when Data_Struct =>
                  declare
                     C : Node_Id := First_Node (Subcomponents (E));
                  begin
                     --  Visit the subcomponents

                     while Present (C) loop
                        Visit (Corresponding_Instance (C));
                        C := Next_Node (C);
                     end loop;
                  end;

               when others =>
                  null;
            end case;
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Types_Body;

         --  Start recording the handling since they have to be reset
         --  for each node.

         Start_Recording_Handlings;

         --  Visit all the subcomponents of the process

         Visit_Subcomponents_Of (E);

         --  Unmark all the marked types

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         F        : Node_Id;
      begin
         --  Declare all necessary data types

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Present (Corresponding_Instance (F)) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Visit all the call sequences of the subprogram

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
      begin
         Push_Entity (Ada_Root);

         --  Visit all the subcomponents of the system

         Visit_Subcomponents_Of (E);

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         F        : Node_Id;
      begin
         --  Declare all necessary data types.

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then AIN.Is_Data (F) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Visit all the call sequences of the thread

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Thread_Instance;

   end Package_Body;

end Ocarina.Backends.PO_HI_Ada.Types;
