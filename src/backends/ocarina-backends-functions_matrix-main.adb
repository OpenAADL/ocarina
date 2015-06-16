------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.FUNCTIONS_MATRIX.MAIN                   --
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

with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Backends.Functions_Matrix.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.XML_Tree.Nutils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Component (E : Node_Id; Table : Node_Id);
   function Get_Full_Component_Name
     (E : Node_Id;
      R : Boolean := False) return Name_Id;

   Current_Parent_Node : Node_Id;
   Functional_System   : Node_Id := No_Node;
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
      Add_Str_To_Name_Buffer ("_functions_matrix");
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

      Set_Str_To_Name_Buffer
        ("Traceability of functions " & "implementation for System ");
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
      Set_Str_To_Name_Buffer
        ("Traceability of functions implementation " & "for System ");
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

   -----------------------------
   -- Get_Full_Component_Name --
   -----------------------------

   function Get_Full_Component_Name
     (E : Node_Id;
      R : Boolean := False) return Name_Id
   is
      T : Name_Id;
      pragma Unreferenced (T);
   begin
      if R then
         Set_Str_To_Name_Buffer ("");
      end if;

      if Parent_Subcomponent (E) /= No_Node
        and then Parent_Component (Parent_Subcomponent (E)) /= No_Node
        and then
          Parent_Subcomponent (Parent_Component (Parent_Subcomponent (E))) /=
          No_Node
      then
         T :=
           Get_Full_Component_Name
             (Parent_Component (Parent_Subcomponent (E)));
      end if;

      Get_Name_String_And_Append
        (Display_Name (Identifier (Parent_Subcomponent (E))));

      if not R then
         Add_Str_To_Name_Buffer (".");
      end if;

      if R then
         return Name_Find;
      else
         return No_Name;
      end if;
   end Get_Full_Component_Name;

   ---------------------
   -- Visit_Component --
   ---------------------

   procedure Visit_Component (E : Node_Id; Table : Node_Id) is
      N  : Node_Id;
      T  : Node_Id;
      TR : Node_Id;
      TD : Node_Id;
      P  : Node_Id;
      Q  : Node_Id;
      S  : Node_Id;
   begin
      TR := Make_XML_Node ("tr");

      --  Create a new colon that contain the name of the
      --  sub-component being analyzed.
      TD := Make_XML_Node ("td");

      Set_Str_To_Name_Buffer
        ("font-family: Arial; background-color: #0a97ac;" &
         "text-align: left; font-weight: bold; font-size: 0.8em");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("style");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (TD));

      N := Make_Defining_Identifier (Get_Full_Component_Name (E, True));
      XTN.Set_Node_Value (TD, N);

      Append_Node_To_List (TD, XTN.Subitems (TR));

      T := First_Node (Subcomponents (Functional_System));

      --  Here, we iterate again on all system sub-components
      --  and try to see which one is connected to the component
      --  actually analyzed (S).

      while Present (T) loop
         TD := Make_XML_Node ("td");

         if Get_Bound_Function (E) /= No_Node
           and then Get_Bound_Function (E) = Corresponding_Instance (T)
         then
            Set_Str_To_Name_Buffer
              ("font-family: Arial; font-weight: bold;" &
               "background-color: #91ff94;" &
               "text-align: center; font-size: 0.8em");
            P := Make_Defining_Identifier (Name_Find);
            Set_Str_To_Name_Buffer ("style");
            Q := Make_Defining_Identifier (Name_Find);
            Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (TD));

            Set_Str_To_Name_Buffer ("O");
         else
            Set_Str_To_Name_Buffer
              ("font-family: Arial; font-weight: bold;" &
               "background-color: #b83f3f;" &
               "text-align: center; font-size: 0.8em");
            P := Make_Defining_Identifier (Name_Find);
            Set_Str_To_Name_Buffer ("style");
            Q := Make_Defining_Identifier (Name_Find);
            Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (TD));

            Set_Str_To_Name_Buffer ("X");
         end if;

         N := Make_Defining_Identifier (Name_Find);

         if N /= No_Node then
            XTN.Set_Node_Value (TD, N);
         end if;

         Append_Node_To_List (TD, XTN.Subitems (TR));
         T := Next_Node (T);
      end loop;

      Append_Node_To_List (TR, XTN.Subitems (Table));

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            Visit_Component (Corresponding_Instance (S), Table);
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Component;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S           : Node_Id;
      N           : Node_Id;
      TR          : Node_Id;
      TD          : Node_Id;
      Table       : Node_Id;
      P           : Node_Id;
      Q           : Node_Id;
      Impl_System : Node_Id := No_Node;
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
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            if Get_Category_Of_Component (S) = CC_System
              and then
                Display_Name (Identifier (S)) =
                Get_String_Name ("Functional")
            then
               Functional_System := Corresponding_Instance (S);
            end if;

            if Get_Category_Of_Component (S) = CC_System
              and then Display_Name (Identifier (S)) = Get_String_Name ("Impl")
            then
               Impl_System := Corresponding_Instance (S);
            end if;

            S := Next_Node (S);
         end loop;
      end if;

      if Impl_System = No_Node then
         Display_Error ("Implementation system not found", Fatal => True);
      end if;

      if Functional_System = No_Node then
         Display_Error ("Functional system not found", Fatal => True);
      end if;

      if not AINU.Is_Empty (Subcomponents (Functional_System)) then

         --  Add a <tr> node that represent a line.
         TR := Make_XML_Node ("tr");

         --  Add a <td> node that represent a colon in the line.
         TD := Make_XML_Node ("td");

         Append_Node_To_List (TD, XTN.Subitems (TR));

         S := First_Node (Subcomponents (Functional_System));

         --  In the following loop, we build a complete line
         --  that contains the name of all system subcomponents.
         while Present (S) loop
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
            S := Next_Node (S);
         end loop;

         Append_Node_To_List (TR, XTN.Subitems (Table));
      end if;

      if not AINU.Is_Empty (Subcomponents (Impl_System)) then
         S := First_Node (Subcomponents (Impl_System));
         while Present (S) loop
            Visit_Component (Corresponding_Instance (S), Table);
            S := Next_Node (S);
         end loop;
      end if;

      --  Add the table to the main HTML node (<body/>).
      Append_Node_To_List (Table, XTN.Subitems (Current_Parent_Node));
   end Visit_System_Instance;
end Ocarina.Backends.Functions_Matrix.Main;
