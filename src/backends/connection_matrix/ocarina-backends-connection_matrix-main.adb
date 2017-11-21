------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.CONNECTION_MATRIX.MAIN                  --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Instances.Queries;

with Ocarina.ME_AADL.AADL_Tree.Nodes;

with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Backends.Connection_Matrix.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.XML_Tree.Nutils;

   use Ocarina.Instances.Queries;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);

   Current_Parent_Node : Node_Id;
   My_Root             : Node_Id;

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
      N     : constant Node_Id := New_Node (XTN.K_HI_Node);
      D     : constant Node_Id := New_Node (XTN.K_HI_Distributed_Application);
      U     : Node_Id;
      T     : Node_Id;
      P     : Node_Id;
      Q     : Node_Id;
      Tmp   : Node_Id;
      H1    : Node_Id;
      Title : Node_Id;
   begin
      My_Root := Root_System (E);

      XML_Root := D;

      Get_Name_String (To_XML_Name (AIN.Name (AIN.Identifier (My_Root))));

      XTN.Set_Name (D, Name_Find);
      XTN.Set_Units (D, New_List (XTN.K_List_Id));
      XTN.Set_HI_Nodes (D, New_List (XTN.K_List_Id));

      Push_Entity (D);

      Set_Str_To_Name_Buffer ("general");
      XTN.Set_Name (N, Name_Find);

      XTN.Set_Units (N, New_List (XTN.K_List_Id));

      --  Append the partition N to the node list

      Append_Node_To_List (N, XTN.HI_Nodes (Current_Entity));
      XTN.Set_Distributed_Application (N, Current_Entity);

      Push_Entity (N);

      U := New_Node (XTN.K_HI_Unit, AIN.Identifier (My_Root));
      Get_Name_String (To_XML_Name (Display_Name (Identifier (My_Root))));
      Add_Str_To_Name_Buffer ("_connection_matrix");
      T := Make_Defining_Identifier (Name_Find);
      P := Make_XML_File (T);

      --  This is a special XML file, we specify .html extension
      XTN.Set_Is_HTML (P, True);
      XTN.Set_Distributed_Application_Unit (P, U);
      XTN.Set_XML_File (U, P);

      --   Make the main <html> node in the XML file.
      XTN.Set_Root_Node (P, Make_XML_Node ("html"));

      Append_Node_To_List (U, XTN.Units (Current_Entity));
      XTN.Set_Entity (U, Current_Entity);

      Push_Entity (U);

      Current_Parent_Node := XTN.Root_Node (XTN.XML_File (U));

      --  Make the <head> node of the HTML file.
      Tmp := Make_XML_Node ("head");

      Append_Node_To_List (Tmp, XTN.Subitems (Current_Parent_Node));

      --  Add a title in the <head> section node
      Title := Make_XML_Node ("title");

      Set_Str_To_Name_Buffer ("Connectivity Matrix for System ");
      Get_Name_String_And_Append (Display_Name (Identifier (My_Root)));

      XTN.Set_Node_Value (Title, Make_Defining_Identifier (Name_Find));

      Append_Node_To_List (Title, XTN.Subitems (Tmp));

      --  Make the <body>...</body> node of the HTML file.
      Tmp := Make_XML_Node ("body");
      Append_Node_To_List (Tmp, XTN.Subitems (Current_Parent_Node));
      Current_Parent_Node := Tmp;

      --  Title of the document, using a <h1>...</h1> node
      Tmp := Make_XML_Node ("h1");
      Append_Node_To_List (Tmp, XTN.Subitems (Current_Parent_Node));

      --  Style of the <h1> node
      Set_Str_To_Name_Buffer
        ("font-family: Arial;" &
         "text-align: center; font-weight: bold; font-size: 1.2em");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("style");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (Tmp));

      --  Title of the document
      Set_Str_To_Name_Buffer ("Connectivity Matrix for System ");
      Get_Name_String_And_Append (Display_Name (Identifier (My_Root)));
      H1 := Make_Defining_Identifier (Name_Find);

      XTN.Set_Node_Value (Tmp, H1);

      Visit (My_Root);

      Pop_Entity;
      Pop_Entity;
      Pop_Entity;
   end Visit_Architecture_Instance;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------

   procedure Visit_Component_Instance (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S                : Node_Id;
      N                : Node_Id;
      Conn             : Node_Id;
      T                : Node_Id;
      TR               : Node_Id;
      TD               : Node_Id;
      UL               : Node_Id;
      LI               : Node_Id;
      H2               : Node_Id;
      Table            : Node_Id;
      P                : Node_Id;
      Q                : Node_Id;
      Connected        : Boolean;
      Source_Component : Node_Id;
      Dest_Component   : Node_Id;
      Bandwidth        : Unsigned_Long_Long;
      Bandwidth_Unit   : Name_Id;
      Latency          : Unsigned_Long_Long;
      Latency_Unit     : Name_Id;
      Associated_Bus   : Node_Id;
      Has_Bus          : Boolean := False;
      Bus_Instance     : Node_Id;
   begin
      --  Declare the table node that will contain the connectivity matrix.
      Table := Make_XML_Node ("table");

      --  Some CSS style to apply on the table.
      Set_Str_To_Name_Buffer ("border-style: solid; border-width: 1px;");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("style");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (Table));

      if not AINU.Is_Empty (Subcomponents (E)) then

         --  Add a <tr> node that represent a line.
         TR := Make_XML_Node ("tr");

         --  Add a <td> node that represent a colon in the line.
         TD := Make_XML_Node ("td");

         Append_Node_To_List (TD, XTN.Subitems (TR));

         S := First_Node (Subcomponents (E));

         --  In the following loop, we build a complete line
         --  that contains the name of all system subcomponents.
         while Present (S) loop
            if Get_Category_Of_Component (S) = CC_Bus then
               Has_Bus := True;
            else

               TD := Make_XML_Node ("td");

               Set_Str_To_Name_Buffer
                 ("font-family: Arial; background-color: #0a97ac;" &
                  "text-align: center; font-weight: bold; font-size: 0.8em");
               P := Make_Defining_Identifier (Name_Find);
               Set_Str_To_Name_Buffer ("style");
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (TD));

               Get_Name_String (Display_Name (Identifier (S)));

               N := Make_Defining_Identifier (Name_Find);
               XTN.Set_Node_Value (TD, N);

               Append_Node_To_List (TD, XTN.Subitems (TR));
            end if;
            S := Next_Node (S);
         end loop;

         Append_Node_To_List (TR, XTN.Subitems (Table));
      end if;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));

         --  In the following loop, we iterate on each subcomponents
         --  and analyzes which one is connected.
         while Present (S) loop
            if Get_Category_Of_Component (S) /= CC_Bus then
               --  Create a new line (<tr> node).
               TR := Make_XML_Node ("tr");

               --  Create a new colon that contain the name of the
               --  sub-component being analyzed.
               TD := Make_XML_Node ("td");

               Set_Str_To_Name_Buffer
                 ("font-family: Arial; background-color: #0a97ac;" &
                  "text-align: center; font-weight: bold; font-size: 0.8em");
               P := Make_Defining_Identifier (Name_Find);
               Set_Str_To_Name_Buffer ("style");
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (TD));

               Get_Name_String (Display_Name (Identifier (S)));

               N := Make_Defining_Identifier (Name_Find);
               XTN.Set_Node_Value (TD, N);

               Append_Node_To_List (TD, XTN.Subitems (TR));

               T := First_Node (Subcomponents (E));

               --  Here, we iterate again on all system sub-components
               --  and try to see which one is connected to the component
               --  actually analyzed (S).

               while Present (T) loop

                  if Get_Category_Of_Component (T) /= CC_Bus then
                     TD := Make_XML_Node ("td");

                     --  Default initialization.
                     Connected      := False;
                     Bandwidth      := 0;
                     Associated_Bus := No_Node;

                     if not AINU.Is_Empty (AIN.Connections (E)) then
                        Conn := First_Node (AIN.Connections (E));
                        while Present (Conn) loop
                           if Kind (Conn) = K_Connection_Instance then
                              if Get_Category_Of_Connection (Conn) =
                                CT_Port_Connection
                              then
                                 Source_Component :=
                                   Item
                                     (AIN.First_Node (Path (Source (Conn))));

                                 Dest_Component :=
                                   Item
                                     (AIN.First_Node
                                        (Path (Destination (Conn))));
                                 if Dest_Component = T
                                   and then Source_Component = S
                                 then

                                    Associated_Bus :=
                                      Get_Bound_Bus (Conn, False);

                                    if Is_Defined_Property
                                        (Conn,
                                         "bus_properties::required_bandwidth")
                                    then
                                       Bandwidth :=
                                         Get_Integer_Property
                                           (Conn,
                                            "bus_properties:" &
                                            ":required_bandwidth");

                                       Bandwidth_Unit :=
                                         ATN.Name
                                           (ATN.Unit_Identifier
                                            (Get_Value_Of_Property_Association
                                               (Conn,
                                                Get_String_Name
                                                  ("bus_properties:" &
                                                     ":required_bandwidth"))));
                                       Connected := True;
                                    end if;
                                 end if;
                              end if;
                           end if;
                           Conn := Next_Node (Conn);
                        end loop;
                     end if;

                     N := No_Node;

                     --  If S = T, then, the component analyzed and the
                     --  component on which we iterate are the same. So,
                     --  we just put the cell in grey.

                     if S = T then
                        Set_Str_To_Name_Buffer ("background-color: grey;");
                        P := Make_Defining_Identifier (Name_Find);
                        Set_Str_To_Name_Buffer ("style");
                        Q := Make_Defining_Identifier (Name_Find);
                        Append_Node_To_List
                          (Make_Assignement (Q, P),
                           XTN.Items (TD));

                     --  If Connected is true, the component being analyzed
                     --  and the component on which we iterate are not the
                     --  same AND are connected. In that case, we put the cell
                     --  in green and print the required bandwidth and (if
                     --  specified) the bus associated to the connection.

                     elsif Connected then
                        if Bandwidth /= 0 then
                           Set_Str_To_Name_Buffer ("<strong>");
                           Add_Str_To_Name_Buffer
                             (Unsigned_Long_Long'Image (Bandwidth));
                           Get_Name_String_And_Append (Bandwidth_Unit);
                           Add_Str_To_Name_Buffer ("</strong>");

                           --  Put information about the associated
                           --  bus if specified.

                           if Associated_Bus /= No_Node then
                              Add_Str_To_Name_Buffer ("<br/><em>(");
                              Get_Name_String_And_Append
                                (Display_Name (Identifier (Associated_Bus)));
                              Add_Str_To_Name_Buffer (")</em>");
                           else
                              Add_Str_To_Name_Buffer
                                ("<br/>(<em>unknwon bus</em>)");
                           end if;
                           N := Make_Defining_Identifier (Name_Find);

                           Set_Str_To_Name_Buffer
                             ("font-family: Arial; " &
                              "background-color: #91ff94;" &
                              "text-align: center; font-size: 0.8em");
                           P := Make_Defining_Identifier (Name_Find);
                           Set_Str_To_Name_Buffer ("style");
                           Q := Make_Defining_Identifier (Name_Find);
                           Append_Node_To_List
                             (Make_Assignement (Q, P),
                              XTN.Items (TD));
                        else

                           --  We put N/A in the cell as a text when
                           --  the required bandwidth size is not specified.

                           Set_Str_To_Name_Buffer ("N/A");
                           N := Make_Defining_Identifier (Name_Find);
                        end if;
                     else
                        --  Components are not connected, we put
                        --  the cell to red.
                        Set_Str_To_Name_Buffer
                          ("font-family: Arial; " &
                           "background-color: #b83f3f;" &
                           "text-align: center; font-size: 0.8em");
                        P := Make_Defining_Identifier (Name_Find);
                        Set_Str_To_Name_Buffer ("style");
                        Q := Make_Defining_Identifier (Name_Find);
                        Append_Node_To_List
                          (Make_Assignement (Q, P),
                           XTN.Items (TD));
                     end if;

                     if N /= No_Node then
                        XTN.Set_Node_Value (TD, N);
                     end if;

                     Append_Node_To_List (TD, XTN.Subitems (TR));
                  end if;
                  T := Next_Node (T);
               end loop;

               Append_Node_To_List (TR, XTN.Subitems (Table));
            end if;
            S := Next_Node (S);
         end loop;
      end if;

      --  Add the table to the main HTML node (<body/>).
      Append_Node_To_List (Table, XTN.Subitems (Current_Parent_Node));

      --  Now, we are enumerating all buses that are used
      --  in the model.

      if Has_Bus then

         --  Make a subtitle for the list of buses.
         H2 := Make_XML_Node ("h2");

         Set_Str_To_Name_Buffer
           ("font-family: Arial;" & "font-weight: bold; font-size: 1.2em");
         P := Make_Defining_Identifier (Name_Find);
         Set_Str_To_Name_Buffer ("style");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (H2));

         Set_Str_To_Name_Buffer ("Buses used");
         N := Make_Defining_Identifier (Name_Find);
         XTN.Set_Node_Value (H2, N);

         Append_Node_To_List (H2, XTN.Subitems (Current_Parent_Node));

         --  Add a <ul> node that represent a line.
         UL := Make_XML_Node ("ul");

         S := First_Node (Subcomponents (E));

         --  In the following loop, we build a complete line
         --  that contains the name of all system subcomponents.
         while Present (S) loop
            if Get_Category_Of_Component (S) = CC_Bus then
               Bus_Instance := Corresponding_Instance (S);
               LI           := Make_XML_Node ("li");

               Set_Str_To_Name_Buffer
                 ("font-family: Arial;" & "font-size: 0.8em");
               P := Make_Defining_Identifier (Name_Find);
               Set_Str_To_Name_Buffer ("style");
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (LI));

               Bandwidth := 0;
               Latency   := 0;

               --  Try to find the bus properties: bandwidth and latency.
               if Is_Defined_Property
                   (Bus_Instance,
                    "bus_properties::bandwidth")
               then

                  Bandwidth :=
                    Get_Integer_Property
                      (Bus_Instance,
                       "bus_properties::bandwidth");

                  Bandwidth_Unit :=
                    ATN.Name
                      (ATN.Unit_Identifier
                         (Get_Value_Of_Property_Association
                            (Bus_Instance,
                             Get_String_Name ("bus_properties::bandwidth"))));
               end if;

               if Is_Defined_Property
                   (Bus_Instance,
                    "bus_properties::max_latency")
               then

                  Latency :=
                    Get_Integer_Property
                      (Bus_Instance,
                       "bus_properties::max_latency");

                  Latency_Unit :=
                    ATN.Name
                      (ATN.Unit_Identifier
                         (Get_Value_Of_Property_Association
                            (Bus_Instance,
                             Get_String_Name
                               ("bus_properties::max_latency"))));
               end if;

               --  First, display the name of the bus in bold
               --  using <strong/> tag.

               Set_Str_To_Name_Buffer ("<strong>");
               Get_Name_String_And_Append
                 (Display_Name (Identifier (Bus_Instance)));
               Add_Str_To_Name_Buffer ("</strong> (<em>");

               --  Then, put its properties between parenthesis.
               if Bandwidth = 0 and then Latency = 0 then
                  Add_Str_To_Name_Buffer ("bus properties are not declared");
               end if;

               if Bandwidth > 0 then
                  Add_Str_To_Name_Buffer ("bandwidth: ");
                  Add_Str_To_Name_Buffer
                    (Unsigned_Long_Long'Image (Bandwidth));
                  Get_Name_String_And_Append (Bandwidth_Unit);
               end if;

               if Latency > 0 then
                  Add_Str_To_Name_Buffer (" latency: ");
                  Add_Str_To_Name_Buffer (Unsigned_Long_Long'Image (Latency));
                  Get_Name_String_And_Append (Latency_Unit);
               end if;

               Add_Str_To_Name_Buffer ("</em>)");

               N := Make_Defining_Identifier (Name_Find);
               XTN.Set_Node_Value (LI, N);

               Append_Node_To_List (LI, XTN.Subitems (UL));
            end if;
            S := Next_Node (S);
         end loop;

         Append_Node_To_List (UL, XTN.Subitems (Current_Parent_Node));
      end if;

   end Visit_System_Instance;
end Ocarina.Backends.Connection_Matrix.Main;
