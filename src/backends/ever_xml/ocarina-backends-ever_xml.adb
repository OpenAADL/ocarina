------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . E V E R _ X M L          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2014-2016 ESA & ISAE.                    --
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

with Charset;           use Charset;
with Ocarina.Backends.Utils;
with Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.Instances; use Ocarina.Instances;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.BE_AADL.Components;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.Analyzer.AADL.Finder;

with Ocarina.Instances.Queries;
with Ocarina.AADL_Values;

with Ocarina.Backends.Utils;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Ocarina.Options;

package body Ocarina.Backends.Ever_XML is
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package AIE renames Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Namet;
   use Ada.Text_IO;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use Ocarina.ME_AADL;
   use Ocarina.Backends.Utils;
   use AIN;
   use Ocarina.BE_AADL.Components;
   use ATE;
   use AIE;
   use type ATN.Node_Kind;
   use Ocarina.Analyzer.AADL.Finder;
   use Ocarina.Options;
   use Ocarina.Instances.Queries;
   use Ocarina.AADL_Values;

   FD_System : File_Type;

   procedure GenerateForSystem (AADL_Root : Node_Id; Name_System : String);

   procedure Visit (E : Node_Id; Depth : Integer);
   procedure Visit_Component_Instance (E : Node_Id; Depth : Integer);
   procedure Visit_Properties (E : Node_Id; Depth : Integer);

   --  XML Helper

   procedure Put_Tag (FD : File_Type; Tag, Value : String; Indent : Integer);
   procedure Open_Tag (FD : File_Type; Tag : String; Indent : Integer);
   procedure Close_Tag (FD : File_Type; Tag : String; Indent : Integer);
   function Get_Tag_String (Tag : Tag_XML) return String;

   procedure Generate_Tag_JSON_File;

   function Get_Category_String (Cat : Component_Category) return String;

   --  Debug
   procedure Print_Title (Title : String);
   procedure Print_Subtitle (Title : String);
   procedure Print_Header (CharIn : String; Title : String);
   procedure Print_Func_Output (Func : String; Output : String);

   ----------
   -- Init --
   ----------
   procedure Init is
   begin
      --  Registration of the generator
      Register_Backend ("ever_xml", Generate'Access, Ever_XML_Backend);
   end Init;

   --------------
   -- Generate --
   --------------
   --  La funzione Generate avvia la procedura di visita dei vari system.
   --  Se nessun system è speficicato con l'opzione -r quando si lancia
   --  il backend, allora per ciascun system presente verrà generato
   --  il relativo file XML
   procedure Generate (AADL_Root : Node_Id) is
      Root_Systems  : Node_List;
      List_Node     : Node_Id;
   begin
      Generate_Tag_JSON_File;
      if Root_System_Name /= No_Name then
         GenerateForSystem (AADL_Root, Get_Name_String (Root_System_Name));
      else
         --  Genero un file per ogni system
         Root_Systems := Find_All_Root_Systems (AADL_Root);

         List_Node := Root_Systems.First;

         while Present (List_Node) loop

            Put_Line (ATE.Get_Name_Of_Entity (List_Node, True, True));

            --  Varibale globale che va impostata per far instanziare un
            --  sistema con questo particolare nome
            Root_System_Name :=
              ATE.Get_Name_Of_Entity (List_Node, False, True);

            GenerateForSystem (AADL_Root,
                               ATE.Get_Name_Of_Entity (List_Node, True, True));
            List_Node := ATN.Next_Entity (List_Node);
         end loop;

      end if;
   end Generate;

   -----------------------
   -- GenerateForSystem --
   -----------------------
   --  Istanzia e lancia la procedura di visita per il il system che viene
   --  passato come parametro. Il file di output che viene generato ha
   --  la seguente struttura per il nome:
   --  {nome_del_system}_ever_xml.xml
   procedure GenerateForSystem (AADL_Root : Node_Id; Name_System : String) is
      Instance_Root : Node_Id;
   begin

      Instance_Root := Instantiate_Model (AADL_Root);

      if No (Instance_Root) then
         raise Program_Error;
      end if;

      Print_Title ("Inizio Ever XML");

      Create (File => FD_System, Name => Name_System & "_ever_xml.xml");

      Visit (Root_System (Instance_Root), 0);

      Close (FD_System);

      Print_Title ("Fine Ever XML");
   end GenerateForSystem;

   -----------
   -- Visit --
   -----------
   procedure Visit (E : Node_Id; Depth : Integer) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Visit (Root_System (E), Depth);

         when K_Component_Instance =>
            Visit_Component_Instance (E, Depth);

         when others =>
            null;
      end case;
   end Visit;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------
   procedure Visit_Component_Instance (E : Node_Id; Depth : Integer) is
      Category    : constant Component_Category :=
        AIE.Get_Category_Of_Component (E);
      Comp_Name   : constant Name_Id := Display_Name (Identifier (E));
      F           : Node_Id;
   begin

      --  Se sono un system apro il tag system, altrimenti apro il tag
      --  component quando trovo un subcomponent
      --  OPEN SYSTEM
      if Category = CC_System then
         Open_Tag (FD_System,
                   Get_Tag_String (Tag_System),
                   Depth);
      end if;

      --  TYPE
      Put_Tag (FD_System,
               Get_Tag_String (Tag_Type),
               Get_Name_String (Comp_Name),
               Depth + 1);

      --  CATEGORY
      Put_Tag (FD_System,
               Get_Tag_String (Tag_Category),
               Get_Category_String (Category),
               Depth + 1);

      --  NAMESPACE
      Put_Tag (FD_System,
               Get_Tag_String (Tag_Namespace),
               Get_Name_String (Display_Name (Identifier (Namespace (E)))),
               Depth + 1);

      --  OPEN FEATURES
      Open_Tag (FD_System,
                Get_Tag_String (Tag_Features),
                Depth + 1);

      --------------
      -- FEATURES --
      --------------
      if Present (Features (E)) then

         F := First_Node (Features (E));

         --  Faccio un loop e fintanto che possiedo una feature F su cui
         --  indagare vado avanti. Si passa alla Feature successiva con
         --  la chiamata: F := Next_Node (F);

         while Present (F) loop
            --  OPEN FEATURE
            Open_Tag (FD_System,
                      Get_Tag_String (Tag_Feature),
                      Depth + 2);

            --  FEATURE NAME
            Put_Tag (FD_System,
                     Get_Tag_String (Tag_Feature_Name),
                     Get_Name_String (Display_Name (Identifier (F))),
                     Depth + 3);

            --  ###############################
            --  ### Direction: in/out/inout ###
            --  ###############################
            declare
               Direction_Kind : Name_Id;
            begin
               --  Verifico se la feature controllata (che è un nodo
               --  dell'albero) è di tipo K_Port_Spec_Instance. I tipi sono
               --  definiti nel file ocarina-me_aadl-aadl_instances-nodes.ads
               --  come una enum chiamata NodeKind
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

               --  FEATURE DIRECTION
               if Direction_Kind /= Get_String_Name ("none") then
                  Put_Tag (FD_System,
                           Get_Tag_String (Tag_Feature_Direction),
                           Get_Name_String (Direction_Kind),
                           Depth + 3);
               end if;
            end;

            --  ###################################
            --  ### Type: event/data/event data ###
            --  ###################################
            declare
               Type_Kind : Name_Id;
               Name_F : Name_Id;
               Namespace_F : Name_Id;
            begin
               --  Controllo la tipologia di porta. Per le porte NON event
               --  posso anche chiedere il nome del tipo associato, mentre
               --  Ocarina va in crash se lo si chiede per quelle di tipo data
               --  (ed infatti in AADL non lo si può neanche specificare).
               Name_F := Get_String_Name ("none");
               Namespace_F := Get_String_Name ("none");
               if Kind (F) = K_Port_Spec_Instance then
                  if Is_Event (F) and then not Is_Data (F) then
                     Type_Kind := Get_String_Name ("event");
                  elsif not Is_Event (F) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("data");
                     Name_F := Display_Name (Identifier
                                             (Corresponding_Instance (F)));
                     Namespace_F := Display_Name
                       (Identifier
                          (Namespace
                               (Corresponding_Instance (F))));
                  elsif Is_Event (F) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("event_data");
                     Name_F := Display_Name (Identifier
                                             (Corresponding_Instance (F)));
                     Namespace_F := Display_Name
                       (Identifier
                          (Namespace
                               (Corresponding_Instance (F))));
                  end if;
               elsif Kind (F) = K_Subcomponent_Access_Instance then
                  Type_Kind := Get_String_Name ("access");
                  Name_F := Display_Name (Identifier
                                          (Corresponding_Instance (F)));
                  Namespace_F := Display_Name
                       (Identifier
                          (Namespace
                               (Corresponding_Instance (F))));
               else
                  Type_Kind := Get_String_Name ("feature");
                  Name_F := Display_Name (Identifier
                                          (Corresponding_Instance (F)));
                  Namespace_F := Display_Name
                       (Identifier
                          (Namespace
                               (Corresponding_Instance (F))));
               end if;
               --  FEATURE PORT TYPE
               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Feature_Port_Type),
                        Get_Name_String (Type_Kind),
                        Depth + 3);

               --  FEATURE PORT DATA TYPE
               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Feature_Port_Data_Type),
                        Get_Name_String (Name_F),
                        Depth + 3);

               --  FEATURE PORT DATA TYPE NAMESPACE
               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Feature_Port_Data_Type_Namespace),
                        Get_Name_String (Namespace_F),
                        Depth + 3);

               --  FEATURE CATEGORY
               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Feature_Category),
                        Node_Kind'Image (Kind (F)),
                        Depth + 3);

            end;

            --  PROPERTIES
            Visit_Properties (F, Depth + 2);

            --  CLOSE FEATURE
            Close_Tag (FD_System,
                       Get_Tag_String (Tag_Feature),
                       Depth + 2);

            --  Passo alla Feature successiva
            F := Next_Node (F);
         end loop;

      end if;

      --  CLOSE FEATURES
      Close_Tag (FD_System,
                 Get_Tag_String (Tag_Features),
                 Depth + 1);

      ----------------
      -- PROPERTIES --
      ----------------
      Visit_Properties (E, Depth);

      -----------------
      -- CONNECTIONS --
      -----------------

      Open_Tag (FD_System,
                Get_Tag_String (Tag_Connections),
                Depth + 1);

      if Present (Connections (E)) then

         F := First_Node (Connections (E));
         while Present (F) loop

            --  CONNECTION
            Open_Tag (FD_System,
                      Get_Tag_String (Tag_Connection),
                      Depth + 2);

            --  CONNECTION NAME
            Put_Tag (FD_System,
                     Get_Tag_String (Tag_Connection_Name),
                     Get_Name_String (Display_Name (Identifier (F))),
                     Depth + 3);

            --  CONNECTION KIND
            Put_Tag (FD_System,
                     Get_Tag_String (Tag_Connection_Kind),
                     Node_Kind'Image (Kind (F)),
                     Depth + 3);

            --  CONNECTION CATEGORY
            Put_Tag (FD_System,
                     Get_Tag_String (Tag_Connection_Category),
                     Port_Connection_Type'Image
                       (AIE.Get_Category_Of_Connection (F)),
                     Depth + 3);

            --  PROPERTIES
            Visit_Properties (F, Depth + 2);

            --  INFORMAZIONI SULLA PORTA
            if AIE.Get_Category_Of_Connection (F) = CT_Port_Connection or
            else AIE.Get_Category_Of_Connection (F) = CT_Access_Subprogram then

               --  OPEN CONNECTION PORT INFO
               Open_Tag (FD_System,
                         Get_Tag_String (Tag_Connection_Port_Info),
                         Depth + 3);

               --  CONNECTION PORT INFO SOURCE
               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Connection_Port_Info_Source),
                        Get_Name_String
                          (Display_Name
                             (Identifier
                                  (AIE.Get_Referenced_Entity
                                       (Source (F))))),
                        Depth + 4);

               --  CONNECTION PORT INFO DESTINATION
               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Connection_Port_Info_Dest),
                        Get_Name_String
                          (Display_Name
                             (Identifier
                                  (AIE.Get_Referenced_Entity
                                       (Destination (F))))),
                        Depth + 4);

               --  CONNECTION PORT INFO PARENT SOURCE
               Put_Tag (FD_System,
                        Get_Tag_String
                          (Tag_Connection_Port_Info_Parent_Source),
                        Get_Name_String
                          (Display_Name
                             (Identifier
                                  (Parent_Component
                                       ((AIE.Get_Referenced_Entity
                                        (Source (F))))))),
                        Depth + 4);

               --  CONNECTION PORT INFO PARENT SOURCE NAME
               Put_Tag (FD_System,
                        Get_Tag_String
                          (Tag_Connection_Port_Info_Parent_Source_Name),
                        Get_Name_String
                          (Display_Name
                             (Identifier
                                  (Item
                                       (AIN.First_Node
                                          (Path (Source (F))))))),
                        Depth + 4);

               --  CONNECTION PORT INFO PARENT DESTINATION
               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Connection_Port_Info_Parent_Dest),
                        Get_Name_String
                          (Display_Name
                             (Identifier
                                  (Parent_Component
                                       ((AIE.Get_Referenced_Entity
                                        (Destination (F))))))),
                        Depth + 4);

               --  CONNECTION PORT INFO PARENT DESTINATION NAME
               Put_Tag (FD_System,
                        Get_Tag_String
                          (Tag_Connection_Port_Info_Parent_Dest_Name),
                        Get_Name_String
                          (Display_Name
                             (Identifier
                                  (Item
                                       (AIN.First_Node
                                          (Path (Destination (F))))))),
                        Depth + 4);

               --  CLOSE CONNECTION PORT INFO
               Close_Tag (FD_System,
                          Get_Tag_String (Tag_Connection_Port_Info),
                          Depth + 3);

            end if;

            Close_Tag (FD_System,
                       Get_Tag_String (Tag_Connection),
                       Depth + 2);

            F := Next_Node (F);
         end loop;
      end if;

      Close_Tag (FD_System,
                 Get_Tag_String (Tag_Connections),
                 Depth + 1);

      -------------------
      -- SUBCOMPONENTS --
      -------------------
      Open_Tag (FD_System,
                Get_Tag_String (Tag_Subcomponents),
                Depth + 1);

      if Present (Subcomponents (E)) then
         F := First_Node (Subcomponents (E));
         while Present (F) loop

            Open_Tag (FD_System,
                      Get_Tag_String (Tag_Subcomponent),
                      Depth + 2);

            Put_Tag (FD_System,
                     Get_Tag_String (Tag_Name),
                     Get_Name_String (Display_Name (Identifier (F))),
                     Depth + 3);

            Visit (Corresponding_Instance (F), Depth + 2);

            Close_Tag (FD_System,
                       Get_Tag_String (Tag_Subcomponent),
                       Depth + 2);

            F := Next_Node (F);
         end loop;
      end if;

      Close_Tag (FD_System,
                 Get_Tag_String (Tag_Subcomponents),
                 Depth + 1);

      --  Stessa cosa del tag di apertura. Un System chiude il tag system,
      --  mentre per tutto il resto il tag component viene chiudo dopo la sua
      --  visita in subcomponents
      --  CLOSE SYSTEM
      if Category = CC_System then
         Close_Tag (FD_System,
                    Get_Tag_String (Tag_System),
                    Depth);
      end if;

   end Visit_Component_Instance;

   procedure Visit_Properties (E : Node_Id; Depth : Integer) is
      F                   : Node_Id;
      AADL_Property_Value : Node_Id;
   begin
      ----------------
      -- PROPERTIES --
      ----------------
      Open_Tag (FD_System,
                Get_Tag_String (Tag_Properties),
                Depth + 1);
      if Present (Properties (E)) then

         F := First_Node (Properties (E));

         while Present (F) loop
            AADL_Property_Value :=
              Get_Value_Of_Property_Association (E, Name (Identifier (F)));

            Open_Tag (FD_System,
                      Get_Tag_String (Tag_Property),
                      Depth + 2);

            --  Name and Namespace
            declare
               Namespace_String    : Unbounded_String;
               Name_String         : Unbounded_String;
               Full_Name_String    : String := Get_Name_String
                 (Display_Name (Identifier (F)));
               Pos                 : Integer := Index (Full_Name_String, "::");
            begin

               if Pos > 0 then
                  Name_String := To_Unbounded_String
                    (Full_Name_String (Pos + 2 .. Full_Name_String'Last));

                  Namespace_String := To_Unbounded_String
                    (Full_Name_String (Full_Name_String'First .. Pos - 1));

                  Put_Tag (FD_System,
                           Get_Tag_String (Tag_Property_Namespace),
                           To_String (Namespace_String),
                           Depth + 3);

                  Put_Tag (FD_System,
                           Get_Tag_String (Tag_Property_Name),
                           To_String (Name_String),
                           Depth + 3);
               else
                  Put_Tag (FD_System,
                           Get_Tag_String (Tag_Property_Name),
                           Full_Name_String,
                           Depth + 3);
               end if;

            end;

            --  Case of Period
            if Present (AADL_Property_Value)
              and then ATN.Kind (AADL_Property_Value) = ATN.K_Signed_AADLNumber
              and then Present (ATN.Unit_Identifier (AADL_Property_Value))
            then
               --  Aggiungo la value con il numero effettivo, mentre per
               --  l'unità di misura devo fare una lettura separata
               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Property_Value),
                        Ocarina.AADL_Values.Image
                                (ATN.Value
                                   (ATN.Number_Value
                                      (AADL_Property_Value))),
                        Depth + 3);

               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Property_Unit),
                        Get_Name_String (ATN.Display_Name
                             (ATN.Unit_Identifier (AADL_Property_Value))),
                        Depth + 3);

            --  Case of Source_Text
            elsif Present (AADL_Property_Value)
              and then ATN.Kind (AADL_Property_Value) = ATN.K_Literal
            then

               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Property_Value),
                        Ocarina.AADL_Values.Image
                          (ATN.Value (AADL_Property_Value), Quoted => False),
                        Depth + 3);

            --  Case of Queue_Size
            elsif Present (AADL_Property_Value)
              and then ATN.Kind (AADL_Property_Value) = ATN.K_Signed_AADLNumber
              and then
              (not Present (ATN.Unit_Identifier (AADL_Property_Value)))
            then

               Put_Tag (FD_System,
                        Get_Tag_String (Tag_Property_Value),
                        Ocarina.AADL_Values.Image
                                (ATN.Value
                                   (ATN.Number_Value
                                      (AADL_Property_Value))),
                        Depth + 3);

            end if;

            Close_Tag (FD_System,
                       Get_Tag_String (Tag_Property),
                       Depth + 2);

            F := Next_Node (F);
         end loop;
      end if;
      Close_Tag (FD_System,
                 Get_Tag_String (Tag_Properties),
                 Depth + 1);
   end Visit_Properties;

   --------------------
   -- Get_Tag_String --
   --------------------
   function Get_Tag_String (Tag : Tag_XML) return String is
   begin

      case Tag is
         when Tag_System =>
            return "system";
         when Tag_Name =>
            return "name";
         when Tag_Type =>
            return "type";
         when Tag_Category =>
            return "category";
         when Tag_Namespace =>
            return "namespace";
         when Tag_Features =>
            return "features";
         when Tag_Feature =>
            return "feature";
         when Tag_Feature_Name =>
            return "name";
         when Tag_Feature_Direction =>
            return "direction";
         when Tag_Feature_Port_Data_Type =>
            return "datatype";
         when Tag_Feature_Port_Data_Type_Namespace =>
            return "datatype_namespace";
         when Tag_Feature_Port_Type =>
            return "type";
         when Tag_Feature_Category =>
            return "category";
         when Tag_Properties =>
            return "properties";
         when Tag_Property =>
            return "property";
         when Tag_Property_Name =>
            return "name";
         when Tag_Property_Namespace =>
            return "namespace";
         when Tag_Property_Value =>
            return "value";
         when Tag_Property_Unit =>
            return "unit";
         when Tag_Subcomponents =>
            return "subcomponents";
         when Tag_Subcomponent =>
            return "component";
         when Tag_Connections =>
            return "connections";
         when Tag_Connection =>
            return "connection";
         when Tag_Connection_Name =>
            return "name";
         when Tag_Connection_Kind =>
            return "kind";
         when Tag_Connection_Category =>
            return "category";
         when Tag_Connection_Port_Info =>
            return "port_info";
         when Tag_Connection_Port_Info_Source =>
            return "source";
         when Tag_Connection_Port_Info_Parent_Source =>
            return "parent_source";
         when Tag_Connection_Port_Info_Parent_Source_Name =>
            return "parent_source_name";
         when Tag_Connection_Port_Info_Dest =>
            return "dest";
         when Tag_Connection_Port_Info_Parent_Dest =>
            return "parent_dest";
         when Tag_Connection_Port_Info_Parent_Dest_Name =>
            return "parent_dest_name";
         when others =>
            return "unknown";
      end case;

   end Get_Tag_String;

   ----------------------------
   -- Generate_Tag_JSON_File --
   ----------------------------
   procedure Generate_Tag_JSON_File is
      File_Content : Unbounded_String;
      FD_JSON      : File_Type;
   begin

      Append (File_Content, "{");
      for I in Tag_XML loop
         Append (File_Content, ASCII.Quotation &
                   Tag_XML'Image (I) &
                   ASCII.Quotation);
         Append (File_Content, ": ");
         Append (File_Content, ASCII.Quotation &
                   Get_Tag_String (I) &
                   ASCII.Quotation);

         if Tag_XML'Last /= I then
            Append (File_Content, ",");
            Append (File_Content, ASCII.LF);
         end if;
      end loop;
      Append (File_Content, "}");
      Create (File => FD_JSON, Name => "tag_ever_xml.json");
      Put_Line (FD_JSON, To_String (File_Content));
      Close (FD_JSON);

   end Generate_Tag_JSON_File;

   -------------
   -- Put_Tag --
   -------------
   procedure Put_Tag (FD : File_Type; Tag, Value : String; Indent : Integer) is
      Temp_Tag : Unbounded_String;
   begin
      for I in 1 .. Indent loop
         Append (Temp_Tag, ASCII.HT);
      end loop;

      Append (Temp_Tag, "<" & Tag & ">");
      Append (Temp_Tag, Value);
      Append (Temp_Tag, "</" & Tag & ">");
      Put_Line (FD, To_String (Temp_Tag));
   end Put_Tag;

   --------------
   -- Open_Tag --
   --------------
   procedure Open_Tag (FD : File_Type; Tag : String; Indent : Integer) is
      Temp_Tag : Unbounded_String;
   begin
      for I in 1 .. Indent loop
         Append (Temp_Tag, ASCII.HT);
      end loop;

      Append (Temp_Tag, "<" & Tag & ">");
      Put_Line (FD, To_String (Temp_Tag));
   end Open_Tag;

   ---------------
   -- Close_Tag --
   ---------------
   procedure Close_Tag (FD : File_Type; Tag : String; Indent : Integer) is
      Temp_Tag : Unbounded_String;
   begin
      for I in 1 .. Indent loop
         Append (Temp_Tag, ASCII.HT);
      end loop;

      Append (Temp_Tag, "</" & Tag & ">");
      Put_Line (FD, To_String (Temp_Tag));
   end Close_Tag;

   -------------------------
   -- Get_Category_String --
   -------------------------
   function Get_Category_String (Cat : Component_Category) return String is
   begin

      case Cat is
         when CC_System =>
            return "system";
         when CC_Process =>
            return "process";
         when CC_Thread =>
            return "thread";
         when CC_Data =>
            return "data";
         when CC_Subprogram =>
            return "subprogram";
         when others =>
            return "unknown";
      end case;

   end Get_Category_String;

   -----------
   -- DEBUG --
   -----------
   procedure Print_Func_Output (Func : String; Output : String) is
      Temp_String : Unbounded_String;
   begin
      Append (Temp_String, "- ");
      Append (Temp_String, Func);
      Append (Temp_String, ": ");
      Append (Temp_String, Output);
      Put_Line (To_String (Temp_String));
   end Print_Func_Output;

   procedure Print_Subtitle (Title : String) is
   begin
      Print_Header ("-", Title);
   end Print_Subtitle;

   procedure Print_Title (Title : String) is
   begin
      Print_Header ("#", Title);
   end Print_Title;

   -----------------------------------------
   -- Stampa a schermo un titolo visibile --
   -- nell'output a terminale             --
   -----------------------------------------
   procedure Print_Header (CharIn : String; Title : String) is
      Temp_Title_String : Unbounded_String;
   begin
      for I in 1 .. Title'Length + 8 loop
         Append (Temp_Title_String, CharIn);
      end loop;
      Put_Line (To_String (Temp_Title_String));
      Temp_Title_String := To_Unbounded_String ("");

      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, " ");
      Append (Temp_Title_String, Title);

      Append (Temp_Title_String, " ");
      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, CharIn);
      Append (Temp_Title_String, CharIn);
      Put_Line (To_String (Temp_Title_String));

      Temp_Title_String := To_Unbounded_String ("");

      for I in 1 .. Title'Length + 8 loop
         Append (Temp_Title_String, CharIn);
      end loop;
      Put_Line (To_String (Temp_Title_String));

   end Print_Header;

   -----------
   -- Reset --
   -----------
   procedure Reset is
   begin
      null;
   end Reset;

end Ocarina.Backends.Ever_XML;
