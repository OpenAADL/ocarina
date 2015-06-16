------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . A A D L _ X M L . M A I N        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2011-2015 ESA & ISAE.                    --
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

with Ocarina.Namet;                     use Ocarina.Namet;
with Ocarina.Instances.Queries; use Ocarina.Instances.Queries;
with Ocarina.AADL_Values;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.AADL_XML.Mapping;

package body Ocarina.Backends.AADL_XML.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.AADL_XML.Mapping;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   use type ATN.Node_Kind;

   procedure Visit_Component (E : Node_Id);
   procedure Visit_Subcomponents_Of (E : Node_Id);

   function Map_Component (E : Node_Id) return Node_Id;

   -------------------
   -- Map_Component --
   -------------------

   function Map_Component (E : Node_Id) return Node_Id is
      Category_Name_String : constant array
      (Component_Category'Range) of Name_Id :=
        (CC_Abstract          => Get_String_Name ("abstract"),
         CC_Bus               => Get_String_Name ("bus"),
         CC_Data              => Get_String_Name ("data"),
         CC_Device            => Get_String_Name ("device"),
         CC_Memory            => Get_String_Name ("memory"),
         CC_Process           => Get_String_Name ("process"),
         CC_Processor         => Get_String_Name ("processor"),
         CC_Subprogram        => Get_String_Name ("subprogram"),
         CC_Subprogram_Group  => Get_String_Name ("subprogram group"),
         CC_System            => Get_String_Name ("system"),
         CC_Thread            => Get_String_Name ("thread"),
         CC_Thread_Group      => Get_String_Name ("thread group"),
         CC_Unknown           => No_Name,
         CC_Virtual_Bus       => Get_String_Name ("virtual bus"),
         CC_Virtual_Processor => Get_String_Name ("virtual processor"));

      N               : Node_Id;
      Classifier_Node : Node_Id;

   begin
      N := Make_XML_Node ("component");

      --  Category

      Append_Node_To_List
        (Make_Assignement
           (Make_Defining_Identifier (Get_String_Name ("category")),
            Make_Defining_Identifier
              (Category_Name_String (Get_Category_Of_Component (E)))),
         XTN.Items (N));

      --  Identifier

      if Present (Parent_Subcomponent (E)) then
         Append_Node_To_List
           (Make_Assignement
              (Make_Defining_Identifier (Get_String_Name ("identifier")),
               Make_Defining_Identifier
                 (Display_Name (Identifier (Parent_Subcomponent (E))))),
            XTN.Items (N));
      else
         Append_Node_To_List
           (Make_Assignement
              (Make_Defining_Identifier (Get_String_Name ("identifier")),
               Make_Defining_Identifier (Display_Name (Identifier (E)))),
            XTN.Items (N));
      end if;

      --  Classifier

      Classifier_Node := Make_XML_Node ("classifier");
      Append_Node_To_List
        (Make_Defining_Identifier (Display_Name (Identifier (E))),
         XTN.Subitems (Classifier_Node));
      Append_Node_To_List (Classifier_Node, XTN.Subitems (N));

      return N;
   end Map_Component;

   ----------------------------
   -- Visit_Subcomponents_Of --
   ----------------------------

   procedure Visit_Subcomponents_Of (E : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Subcomponents_Of;

   Root_System_Node : Node_Id := No_Node;
   AADL_XML_Node    : Node_Id := No_Node;
   Current_XML_Node : Node_Id := No_Node;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Root_System_Node := Root_System (E);
            Visit (Root_System_Node);

         when K_Component_Instance =>
            Visit_Component (E);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------
   -- Visit_Component --
   ---------------------

   procedure Visit_Component (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
      N        : Node_Id;

      Old_XML_Node        : Node_Id;
      Subcomponents_Node  : Node_Id;
      Features_Node       : Node_Id;
      Properties_Node     : Node_Id;
      Feature_Node        : Node_Id;
      F                   : Node_Id;
      P                   : Node_Id;
      U                   : Node_Id;
      Property_Node       : Node_Id;
      Components_Node     : Node_Id;
      Property_Value_Node : Node_Id;
      AADL_Property_Value : Node_Id;

   begin
      if Category = CC_System then
         P := Map_HI_Node (E);
         Push_Entity (P);

         U := Map_HI_Unit (E);
         Push_Entity (U);

         if AADL_XML_Node = No_Node then
            AADL_XML_Node := Make_XML_Node ("aadl_xml");

            Append_Node_To_List
              (Make_Assignement
                 (Make_Defining_Identifier (Get_String_Name ("root_system")),
                  Make_Defining_Identifier
                    (Display_Name (Identifier (Root_System_Node)))),
               XTN.Items (AADL_XML_Node));

            Append_Node_To_List
              (AADL_XML_Node,
               XTN.Subitems (XTN.Root_Node (XTN.XML_File (U))));
            Components_Node := Make_XML_Node ("components");
            Append_Node_To_List
              (Components_Node,
               XTN.Subitems (AADL_XML_Node));
         end if;
         Current_XML_Node := Components_Node;
      end if;

      --  First bits of the component node

      N := Map_Component (E);
      Append_Node_To_List (N, XTN.Subitems (Current_XML_Node));

      --  Features

      Features_Node := Make_XML_Node ("features");
      Append_Node_To_List (Features_Node, XTN.Subitems (N));

      if Present (Features (E)) then
         F := First_Node (Features (E));
         while Present (F) loop
            Feature_Node := Make_XML_Node ("feature");

            --  Identifier

            Append_Node_To_List
              (Make_Assignement
                 (Make_Defining_Identifier (Get_String_Name ("identifier")),
                  Make_Defining_Identifier (Display_Name (Identifier (F)))),
               XTN.Items (Feature_Node));

            --  Direction: in/out/inout

            declare
               Direction_Node : Node_Id;
               Direction_Kind : Name_Id;
            begin
               Direction_Node := Make_XML_Node ("direction");
               if Kind (F) = K_Port_Spec_Instance then
                  if Is_In (F) and then not Is_Out (F) then
                     Direction_Kind := Get_String_Name ("in");
                  elsif (not Is_In (F)) and then Is_Out (F) then
                     Direction_Kind := Get_String_Name ("out");
                  elsif Is_In (F) and then Is_Out (F) then
                     Direction_Kind := Get_String_Name ("inout");
                  end if;
               else
                  Direction_Kind := Get_String_Name ("none");
               end if;

               Append_Node_To_List
                 (Make_Assignement
                    (Make_Defining_Identifier (Get_String_Name ("kind")),
                     Make_Defining_Identifier (Direction_Kind)),
                  XTN.Items (Direction_Node));

               Append_Node_To_List
                 (Direction_Node,
                  XTN.Subitems (Feature_Node));
            end;

            --  Type: event/data/event data

            declare
               Type_Node : Node_Id;
               Type_Kind : Name_Id;
            begin
               Type_Node := Make_XML_Node ("type");
               if Kind (F) = K_Port_Spec_Instance then
                  if Is_Event (F) and then not Is_Data (F) then
                     Type_Kind := Get_String_Name ("event");
                  elsif not (Is_Event (F)) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("data");
                  elsif Is_Event (F) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("event_data");
                  end if;
               elsif Kind (F) = K_Subcomponent_Access_Instance then
                  Type_Kind := Get_String_Name ("access");
               else
                  Type_Kind := Get_String_Name ("feature");
               end if;

               Append_Node_To_List
                 (Make_Assignement
                    (Make_Defining_Identifier (Get_String_Name ("kind")),
                     Make_Defining_Identifier (Type_Kind)),
                  XTN.Items (Type_Node));

               Append_Node_To_List (Type_Node, XTN.Subitems (Feature_Node));
            end;

            --  Classifier

            declare
               Classifier_Node : Node_Id;
            begin
               Classifier_Node := Make_XML_Node ("classifier");
               Append_Node_To_List
                 (Make_Defining_Identifier
                    (Display_Name (Identifier (Corresponding_Instance (F)))),
                  XTN.Subitems (Classifier_Node));
               Append_Node_To_List
                 (Classifier_Node,
                  XTN.Subitems (Feature_Node));
            end;

            Append_Node_To_List (Feature_Node, XTN.Subitems (Features_Node));
            F := Next_Node (F);
         end loop;
      end if;

      --  Subcomponents

      Subcomponents_Node := Make_XML_Node ("subcomponents");
      Append_Node_To_List (Subcomponents_Node, XTN.Subitems (N));

      Old_XML_Node     := Current_XML_Node;
      Current_XML_Node := Subcomponents_Node;
      Visit_Subcomponents_Of (E);
      Current_XML_Node := Old_XML_Node;

      --  Properties

      Properties_Node := Make_XML_Node ("properties");
      Append_Node_To_List (Properties_Node, XTN.Subitems (N));

      if Present (Properties (E)) then
         F := First_Node (Properties (E));
         while Present (F) loop
            --  XXX Warning, if there are multiple values for a
            --  property (e.g. because of inheritance), then all
            --  values are dumped.

            Property_Node := Make_XML_Node ("property");

            Append_Node_To_List
              (Make_Assignement
                 (Make_Defining_Identifier (Get_String_Name ("name")),
                  Make_Defining_Identifier (Display_Name (Identifier (F)))),
               XTN.Items (Property_Node));

            Property_Value_Node := Make_XML_Node ("property_value");
            Append_Node_To_List
              (Property_Value_Node,
               XTN.Subitems (Property_Node));

            AADL_Property_Value :=
              Get_Value_Of_Property_Association (E, Name (Identifier (F)));

            if Present (AADL_Property_Value)
              and then ATN.Kind (AADL_Property_Value) = ATN.K_Signed_AADLNumber
              and then Present (ATN.Unit_Identifier (AADL_Property_Value))
            then
               --  This property value denotes a property with a unit
               --  identifier.

               declare
                  Unit_Node : Node_Id;
               begin
                  Unit_Node := Make_XML_Node ("unit");
                  Append_Node_To_List
                    (Unit_Node,
                     XTN.Subitems (Property_Value_Node));

                  Append_Node_To_List
                    (Make_Assignement
                       (Make_Defining_Identifier (Get_String_Name ("value")),
                        Make_Defining_Identifier
                          (Get_String_Name
                             (Ocarina.AADL_Values.Image
                                (ATN.Value
                                   (ATN.Number_Value
                                      (AADL_Property_Value)))))),
                     XTN.Items (Unit_Node));

                  Append_Node_To_List
                    (Make_Assignement
                       (Make_Defining_Identifier (Get_String_Name ("unit")),
                        Make_Defining_Identifier
                          (ATN.Display_Name
                             (ATN.Unit_Identifier (AADL_Property_Value)))),
                     XTN.Items (Unit_Node));
               end;

            elsif Present (AADL_Property_Value)
              and then ATN.Kind (AADL_Property_Value) = ATN.K_Signed_AADLNumber
              and then
              (not Present (ATN.Unit_Identifier (AADL_Property_Value)))
            then
               --  This property value denotes a property without unit

               declare
                  Unit_Node : Node_Id;
               begin
                  Unit_Node := Make_XML_Node ("value");
                  Append_Node_To_List
                    (Unit_Node,
                     XTN.Subitems (Property_Value_Node));

                  Append_Node_To_List
                    (Make_Assignement
                       (Make_Defining_Identifier (Get_String_Name ("value")),
                        Make_Defining_Identifier
                          (Get_String_Name
                             (Ocarina.AADL_Values.Image
                                (ATN.Value
                                   (ATN.Number_Value
                                      (AADL_Property_Value)))))),
                     XTN.Items (Unit_Node));
               end;

            elsif Present (AADL_Property_Value)
              and then ATN.Kind (AADL_Property_Value) = ATN.K_Literal
            then
               --  This property value denotes a literal

               declare
                  Unit_Node : Node_Id;
               begin
                  Unit_Node := Make_XML_Node ("value");
                  Append_Node_To_List
                    (Unit_Node,
                     XTN.Subitems (Property_Value_Node));

                  Append_Node_To_List
                    (Make_Assignement
                       (Make_Defining_Identifier (Get_String_Name ("value")),
                        Make_Defining_Identifier
                          (Get_String_Name
                             (Ocarina.AADL_Values.Image
                                (ATN.Value (AADL_Property_Value),
                                 Quoted => False)))),
                     XTN.Items (Unit_Node));
               end;

            elsif Present (AADL_Property_Value)
              and then ATN.Kind (AADL_Property_Value) = ATN.K_Reference_Term
            then
               --  This property value denotes a reference term

               declare
                  Unit_Node : Node_Id;
               begin
                  Unit_Node := Make_XML_Node ("reference");
                  Append_Node_To_List
                    (Unit_Node,
                     XTN.Subitems (Property_Value_Node));

                  Append_Node_To_List
                    (Make_Assignement
                       (Make_Defining_Identifier (Get_String_Name ("value")),
                        Make_Defining_Identifier
                          (ATN.Display_Name
                             (ATN.First_Node --  XXX must iterate
                                (ATN.List_Items
                                   (ATN.Reference_Term
                                      (AADL_Property_Value)))))),
                     XTN.Items (Unit_Node));
               end;

            elsif Present (AADL_Property_Value)
              and then ATN.Kind (AADL_Property_Value) = ATN.K_Enumeration_Term
            then
               --  This property value denotes an enumeration term

               declare
                  Unit_Node : Node_Id;
               begin
                  Unit_Node := Make_XML_Node ("value");
                  Append_Node_To_List
                    (Unit_Node,
                     XTN.Subitems (Property_Value_Node));

                  Append_Node_To_List
                    (Make_Assignement
                       (Make_Defining_Identifier (Get_String_Name ("value")),
                        Make_Defining_Identifier
                          (ATN.Display_Name
                             (ATN.Identifier (AADL_Property_Value)))),
                     XTN.Items (Unit_Node));
               end;

            elsif Present (AADL_Property_Value)
              and then ATN.Kind (AADL_Property_Value) = ATN.K_Number_Range_Term
            then
               --  This property value denotes a number range term

               declare
                  Unit_Node : Node_Id;
               begin
                  Unit_Node := Make_XML_Node ("range");
                  Append_Node_To_List
                    (Unit_Node,
                     XTN.Subitems (Property_Value_Node));

                  Append_Node_To_List
                    (Make_Assignement
                       (Make_Defining_Identifier
                          (Get_String_Name ("value_low")),
                        Make_Defining_Identifier
                          (Get_String_Name
                             (Ocarina.AADL_Values.Image
                                (ATN.Value
                                   (ATN.Number_Value
                                      (ATN.Lower_Bound
                                         (AADL_Property_Value))))))),
                     XTN.Items (Unit_Node));

                  Append_Node_To_List
                    (Make_Assignement
                       (Make_Defining_Identifier
                          (Get_String_Name ("value_high")),
                        Make_Defining_Identifier
                          (Get_String_Name
                             (Ocarina.AADL_Values.Image
                                (ATN.Value
                                   (ATN.Number_Value
                                      (ATN.Upper_Bound
                                         (AADL_Property_Value))))))),
                     XTN.Items (Unit_Node));

                  if Present
                      (ATN.Unit_Identifier
                         (ATN.Lower_Bound (AADL_Property_Value)))
                  then
                     Append_Node_To_List
                       (Make_Assignement
                          (Make_Defining_Identifier (Get_String_Name ("unit")),
                           Make_Defining_Identifier
                             (ATN.Display_Name
                                (ATN.Unit_Identifier
                                   (ATN.Lower_Bound (AADL_Property_Value))))),
                        XTN.Items (Unit_Node));
                  end if;
               end;

            elsif Present (AADL_Property_Value) then
               --  XXX ICE

               declare
                  Unit_Node : Node_Id;
               begin
                  Unit_Node :=
                    Make_XML_Node (ATN.Kind (AADL_Property_Value)'Img);
                  Append_Node_To_List
                    (Unit_Node,
                     XTN.Subitems (Property_Value_Node));
               end;
            end if;

            Append_Node_To_List
              (Property_Node,
               XTN.Subitems (Properties_Node));
            F := Next_Node (F);
         end loop;
      end if;

      if Category = CC_System then
         Pop_Entity;
         Pop_Entity; --  A
      end if;
   end Visit_Component;

end Ocarina.Backends.AADL_XML.Main;
