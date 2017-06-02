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


   FD_System         : File_Type;

   Foo_Bar_JSON : JSON_Value := Create_Object;

   --------------------------------------
   -- Procedura che visita i vari nodi --
   --------------------------------------

   procedure Visit (E : Node_Id; Depth : Integer);
   procedure Visit_Component_Instance (E : Node_Id; Depth : Integer);

   procedure Put_Tag (FD : File_Type; Tag, Value : String; Indent : Integer);
   procedure Open_Tag (FD : File_Type; Tag : String; Indent : Integer);
   procedure Close_Tag (FD : File_Type; Tag : String; Indent : Integer);


   function Get_Category_String (Cat : Component_Category) return String;

   procedure Print_Title ( Title : String );
   procedure Print_Subtitle ( Title : String );
   procedure Print_Header (CharIn : String; Title : String);
   procedure Print_Func_Output (Func : String; Output : String);

   procedure GenerateForSystem (AADL_Root : Node_Id; Name_System : String);

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
   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
      Root_Systems  : Node_List;
      List_Node     : Node_Id;

   begin

      if Root_System_Name /= No_Name then
         GenerateForSystem (AADL_Root, Get_Name_String (Root_System_Name));
      else
         -- Genero un file per ogni system
         Root_Systems := Find_All_Root_Systems (AADL_Root);

         List_Node := Root_Systems.First;

         while Present (List_Node) loop

            Put_Line ( ATE.Get_Name_Of_Entity (List_Node, True, True));

            -- Varibale globale che va impostata per far instanziare un sistema
            -- con questo particolare nome
            Root_System_Name := ATE.Get_Name_Of_Entity (List_Node, False, True);

            GenerateForSystem(AADL_Root, ATE.Get_Name_Of_Entity (List_Node, True, True));
            List_Node := ATN.Next_Entity (List_Node);
         end loop;

      end if;
   end Generate;

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

--     procedure Visit_Component_Instance (E : Node_Id; Depth : Integer) is
--        Category    : constant Component_Category := AIE.Get_Category_Of_Component (E);
--        Comp_Name   : constant Name_Id := Display_Name (Identifier (E));
--        -- Variabili di supporto
--        F           : Node_Id;
--        AADL_Property_Value : Node_Id;
--     begin
--        -- Se sono un system apro il tag system, altrimenti apro il tag component
--        -- quando trovo un subcomponent
--        if Category = CC_System then
--           Open_Tag (FD_System, "system", Depth);
--        end if;
--
--        Put_Tag (FD_System, "type", Get_Name_String (Comp_Name), Depth + 1);
--        Put_Tag (FD_System, "category", Get_Category_String (Category), Depth + 1);
--        Put_Tag (FD_System, "namespace", Get_Name_String (Display_Name (Identifier (Namespace (E)))), Depth + 1);
--
--        -- Stessa cosa del tag di apertura. Un System chiude il tag system,
--        -- mentre per tutto il resto il tag component viene chiudo dopo la sua
--        -- visita in subcomponents
--        if Category = CC_System then
--           Close_Tag (FD_System, "system", Depth);
--        end if;
--
--     end Visit_Component_Instance;


   procedure Visit_Component_Instance (E : Node_Id; Depth : Integer) is
      Category    : constant Component_Category := AIE.Get_Category_Of_Component (E);
      Comp_Name   : constant Name_Id := Display_Name (Identifier (E));
      F           : Node_Id;
      Indent      : Integer;

      AADL_Property_Value : Node_Id;
   begin
      Indent := Depth;

      -- Se sono un system apro il tag system, altrimenti apro il tag component
      if Category = CC_System then
         Open_Tag (FD_System, "system", Indent);
      end if;

      Indent := Depth + 1;

      -- Aggiungo il nome NON normalizzato con il tag <name>
      -- Questo vuol dire che il nome è lo stesso del file AADL e non vengono
      -- sostituiti i trattini . con i trattini _, ecc...
      Put_Tag (FD_System, "type", Get_Name_String (Comp_Name), Indent);
      Put_Tag (FD_System, "category", Get_Category_String (Category), Indent);
      Put_Tag (FD_System, "namespace", Get_Name_String (Display_Name (Identifier (Namespace (E)))), Indent);

      -- La Normalize_Name sotituisce i punti . con dei trattini bassi _
      -- Estremamante scomoda
      Print_Func_Output ("Normalize_Name (Display_Name (Identifier (E)))", Get_Name_String (Normalize_Name (Comp_Name)));
      Print_Func_Output ("Display_Name (Identifier (E))", Get_Name_String (Comp_Name));

      Print_Func_Output ("Get_Category_Of_Component (E)", Component_Category'Image (Category));
      Print_Func_Output ("Get_Category_String (E)", Get_Category_String (Category));

      Print_Func_Output ("Namespace", Get_Name_String (Display_Name (Identifier (Namespace (E)))));

      Print_Title ("Features");

      -- Se si usa la 'Image di ADA spesso si ottengono numeri, bisogna quindi usare
      -- la funzione di Ocarina Get_Name_String che restituisce il nome effettivo usato nell'AADL

      Open_Tag (FD_System, "features", Indent);

      -- Features (E) ritorna un List_Id
      -- First_Node converte da List_Id a Node_Id
      if Present (Features (E)) then
         Print_Func_Output ("Present (Features (E))", "True");

         F := First_Node (Features (E));

         -- Faccio un loop e fintanto che possiedo una feature F su cui indagare
         -- vado avanti. Si passa alla Feature successiva con la chiamata:
         -- F := Next_Node (F);

         Indent := Indent + 1;
         while Present (F) loop
            Open_Tag (FD_System, "feature", Indent);
            Print_Func_Output ("Display_Name (Identifier (F))", Get_Name_String (Display_Name (Identifier (F))));

            Put_Tag (FD_System, "name", Get_Name_String (Display_Name (Identifier (F))), Indent + 1);

            -- ###############################
            -- ### Direction: in/out/inout ###
            -- ###############################
            declare
               Direction_Kind : Name_Id;
            begin
               -- Controllo se la feature controllata (che è un nodo dell'albero) è di tipo
               -- K_Port_Spec_Instance. I tipi sono definiti nel file ocarina-me_aadl-aadl_instances-nodes.ads
               -- come una enum chiama NodeKind
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

               Print_Func_Output ("Direction_Kind", Get_Name_String (Direction_Kind));
               Put_Tag (FD_System, "direction", Get_Name_String (Direction_Kind), Indent + 1);

            end;

            -- ###################################
            -- ### Type: event/data/event data ###
            -- ###################################
            declare
               Type_Kind : Name_Id;
               Name_F : Name_Id;
            begin
               -- Controllo la tipologia di porta. Per le porte NON event posso anche chiedere
               -- il nome del tipo associato, mentre Ocarina va in crash se lo si chiede per
               -- quelle di tipo data (ed infatti in AADL non lo si può neanche specificare).
               Name_F := Get_String_Name ("none");
               if Kind (F) = K_Port_Spec_Instance then
                  if Is_Event (F) and then not Is_Data (F) then
                     Type_Kind := Get_String_Name ("event");
                  elsif not Is_Event (F) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("data");
                     Name_F := Display_Name (Identifier (Corresponding_Instance (F)));
                  elsif Is_Event (F) and then Is_Data (F) then
                     Type_Kind := Get_String_Name ("event_data");
                     Name_F := Display_Name (Identifier (Corresponding_Instance (F)));
                  end if;
               elsif Kind (F) = K_Subcomponent_Access_Instance then
                  Type_Kind := Get_String_Name ("access");
                  Name_F := Display_Name (Identifier (Corresponding_Instance (F)));
               else
                  Type_Kind := Get_String_Name ("feature");
                  Name_F := Display_Name (Identifier (Corresponding_Instance (F)));
               end if;

               Print_Func_Output ("Type_Kind", Get_Name_String (Type_Kind));
               Print_Func_Output ("Display_Name",  Get_Name_String (Name_F));

               Put_Tag (FD_System, "type", Get_Name_String (Type_Kind), Indent + 1);
               Put_Tag (FD_System, "name_f", Get_Name_String (Name_F), Indent + 1);
            end;

            Close_Tag (FD_System, "feature", Indent);
            Put_Line("");
            -- Passo alla Feature successiva
            F := Next_Node (F);
         end loop;

      else
         Print_Func_Output ("Present (Feature (E))", "False");
      end if;

      Indent := Depth + 1;
      Close_Tag (FD_System, "features", Indent);

      Print_Title ("Properties");
      Open_Tag (FD_System, "properties", Indent);
      if Present (Properties (E)) then
         Print_Func_Output ("Present (Properties (E))", "True");

         F := First_Node (Properties (E));
         while Present (F) loop
            AADL_Property_Value := Get_Value_Of_Property_Association (E, Name (Identifier (F)));
            Print_Func_Output ("Display_Name (Identifier (F))", Get_Name_String (Display_Name (Identifier (F))));

            Open_Tag (FD_System, "property", Indent);
            Put_Tag (FD_System, "name", Get_Name_String (Display_Name (Identifier (F))), Indent + 1);
            -- Source_Text

            if Present (AADL_Property_Value) and then ATN.Kind (AADL_Property_Value) = ATN.K_Literal then

               Print_Func_Output ("ValueProp", Ocarina.AADL_Values.Image (ATN.Value (AADL_Property_Value), Quoted => False));
               Put_Tag (FD_System, "value", Ocarina.AADL_Values.Image (ATN.Value (AADL_Property_Value), Quoted => False), Indent + 1);
            end if;
            Close_Tag (FD_System, "property", Indent);
            F := Next_Node (F);
         end loop;
      else
         Print_Func_Output ("Present (Properties (E))", "False");
      end if;
      Close_Tag (FD_System, "properties", Indent);
      Print_Title ("Connections");

      if Present (Connections (E)) then
         Print_Func_Output ("Present (Connections (E))", "True");

         F := First_Node (Connections (E));
         while Present (F) loop

            Print_Func_Output ("Display_Name (Identifier (F))", Get_Name_String (Display_Name (Identifier (F))));
            Print_Func_Output ("Kind (F)", Node_Kind'Image (Kind (F)));
            Print_Func_Output ("Get_Category_Of_Connection (F)", Port_Connection_Type'Image ( AIE.Get_Category_Of_Connection (F)) );

            if AIE.Get_Category_Of_Connection (F) = CT_Port_Connection then

               Print_Func_Output ("Source",  Get_Name_String (Display_Name (Identifier (AIE.Get_Referenced_Entity (Source (F))))));
               Print_Func_Output ("Dest",  Get_Name_String (Display_Name (Identifier (AIE.Get_Referenced_Entity (Destination (F))))));

               Print_Func_Output ("Parent Source",  Get_Name_String (Display_Name (Identifier (Parent_Component ((AIE.Get_Referenced_Entity (Source (F))))))));
               Print_Func_Output ("Parent Source Name",  Get_Name_String (Display_Name (Identifier (Item (AIN.First_Node (Path (Source (F))))))));

               Print_Func_Output ("Parent Dest",  Get_Name_String (Display_Name (Identifier (Parent_Component ((AIE.Get_Referenced_Entity (Destination (F))))))));
               Print_Func_Output ("Parent Dest Name",  Get_Name_String (Display_Name (Identifier (Item (AIN.First_Node (Path (Destination (F))))))));



            end if;


            F := Next_Node (F);
         end loop;
      else
         Print_Func_Output ("Present (Connections (E))", "False");
      end if;

      Open_Tag (FD_System, "subcomponents", Indent);

      Print_Title ("Subcomponents");
      if Present (Subcomponents (E)) then
         Print_Func_Output ("Present (Subcomponents (E))", "True");
         F := First_Node (Subcomponents (E));
         while Present (F) loop
            Print_Func_Output ("Display_Name (Identifier (F))", Get_Name_String (Display_Name (Identifier (F))));

            -- Not working Print_Func_Output ("Corresponding_Declaration (E)", Get_Name_String (Display_Name (Corresponding_Declaration (Corresponding_Instance (F)))));

            Open_Tag (FD_System, "component", Depth);
            Put_Tag (FD_System, "name", Get_Name_String (Display_Name (Identifier (F))), Depth);
            Visit (Corresponding_Instance (F), Depth + 1);
            Close_Tag (FD_System, "component", Depth);

            F := Next_Node (F);
         end loop;
      else
         Print_Func_Output ("Present (Subcomponents (E))", "False");
      end if;

      Close_Tag (FD_System, "subcomponents", Indent);

      Put_Line("");
      Put_Line("------------------------");
      Put_Line("");

      -- Stessa cosa del tag di apertura. Un System chiude il tag system,
      -- mentre tutto il resto chiudo il tag component
      if Category = CC_System then
         Close_Tag (FD_System, "system", Depth);
      end if;

   end Visit_Component_Instance;


   procedure Put_Tag (FD : File_Type; Tag, Value : String; Indent : Integer) is
      Temp_Tag : Unbounded_String;
   begin
      for I in 1 .. Indent loop
         Append(Temp_Tag, ASCII.HT);
      end loop;

      Append (Temp_Tag, "<" & Tag & ">");
      Append (Temp_Tag, Value);
      Append (Temp_Tag, "</" & Tag & ">");
      Put_Line (FD, To_String (Temp_Tag));
   end Put_Tag;

   procedure Open_Tag (FD : File_Type; Tag : String; Indent : Integer) is
      Temp_Tag : Unbounded_String;
   begin
      for I in 1 .. Indent loop
         Append(Temp_Tag, ASCII.HT);
      end loop;

      Append (Temp_Tag, "<" & Tag & ">");
      Put_Line (FD, To_String (Temp_Tag));
   end Open_Tag;

   procedure Close_Tag (FD : File_Type; Tag : String; Indent : Integer) is
      Temp_Tag : Unbounded_String;
   begin
      for I in 1 .. Indent loop
         Append(Temp_Tag, ASCII.HT);
      end loop;

      Append (Temp_Tag, "</" & Tag & ">");
      Put_Line (FD, To_String (Temp_Tag));
   end Close_Tag;


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
      Put_Line ( To_String (Temp_Title_String));

   end Print_Header;


   -----------
   -- Reset --
   -----------
   procedure Reset is
   begin
      null;
   end Reset;

end Ocarina.Backends.Ever_XML;
