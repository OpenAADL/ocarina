------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . S T A T S . M A P P I N G        --
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

with Ocarina.Namet; use Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Stats.Main;

with Ocarina.Backends.XML_Common.Mapping;
with Ocarina.Backends.XML_Values;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

package body Ocarina.Backends.Stats.Mapping is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Stats.Main;
   use Ocarina.Backends.XML_Common.Mapping;
   use Ocarina.Backends.XML_Tree.Nodes;
   use Ocarina.Backends.XML_Tree.Nutils;

   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XV renames Ocarina.Backends.XML_Values;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   procedure Map_Virtual_Bus_Layers (E : Node_Id; X : Node_Id);
   procedure Check_Mils_Enforcement (E : Node_Id; N : Node_Id);

   procedure Map_Scheduler (E : Node_Id; N : Node_Id) is
      Scheduler : Supported_POK_Scheduler;
      R         : Node_Id;
      Q         : Node_Id;
   begin
      Scheduler := Get_POK_Scheduler (E);

      Set_Str_To_Name_Buffer ("scheduler");
      R := Make_Defining_Identifier (Name_Find);

      if Scheduler = RMS then
         Set_Str_To_Name_Buffer ("rms");
      elsif Scheduler = EDF then
         Set_Str_To_Name_Buffer ("edf");
      elsif Scheduler = Static then
         Set_Str_To_Name_Buffer ("static");
      else
         Set_Str_To_Name_Buffer ("unknown");
      end if;

      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));
   end Map_Scheduler;

   ---------------------------------
   -- Map_Distributed_Application --
   ---------------------------------

   function Map_Distributed_Application (E : Node_Id) return Node_Id is
      D : constant Node_Id := New_Node (XTN.K_HI_Distributed_Application);
   begin
      pragma Assert (AINU.Is_System (E));

      --  Update the global variable to be able to fetch the root of
      --  the distributed application and generate the source files.

      XML_Root := D;

      Get_Name_String (To_XML_Name (AIN.Name (AIN.Identifier (E))));

      XTN.Set_Name (D, Name_Find);
      XTN.Set_Units (D, New_List (XTN.K_List_Id));
      XTN.Set_HI_Nodes (D, New_List (XTN.K_List_Id));

      return D;
   end Map_Distributed_Application;

   -----------------
   -- Map_HI_Node --
   -----------------

   function Map_HI_Node (E : Node_Id) return Node_Id is
      N : constant Node_Id := New_Node (XTN.K_HI_Node);
   begin
      pragma Assert (AINU.Is_Process (E) or else AINU.Is_System (E));

      --  The name of the node is not the name of the process
      --  component instance, but the name of the process subcomponent
      --  corresponding to this instance.

      if AINU.Is_System (E) then
         Set_Str_To_Name_Buffer ("general");
      else
         Get_Name_String
           (To_XML_Name
              (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))));
         Add_Str_To_Name_Buffer ("_stats");
      end if;

      XTN.Set_Name (N, Name_Find);

      Set_Units (N, New_List (K_List_Id));

      --  Append the partition N to the node list

      Append_Node_To_List (N, HI_Nodes (Current_Entity));
      Set_Distributed_Application (N, Current_Entity);

      return N;
   end Map_HI_Node;

   -----------------
   -- Map_HI_Unit --
   -----------------

   function Map_HI_Unit (E : Node_Id) return Node_Id is
      U : Node_Id;
      N : Node_Id;
      P : Node_Id;
   begin
      pragma Assert (AINU.Is_System (E) or else AINU.Is_Process (E));

      U := New_Node (XTN.K_HI_Unit, AIN.Identifier (E));

      --  Packages that are common to all nodes
      Get_Name_String (To_XML_Name (Display_Name (Identifier (E))));
      Add_Str_To_Name_Buffer ("_stats");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_XML_File (N);
      Set_Distributed_Application_Unit (P, U);
      XTN.Set_XML_File (U, P);

      XTN.Set_Root_Node (P, Make_XML_Node ("root"));

      Append_Node_To_List (U, Units (Current_Entity));
      XTN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

   ---------------------------
   -- Map_Subprogram_Access --
   ---------------------------

   function Map_Subprogram_Access (E : Node_Id) return Node_Id is
      N   : Node_Id;
      R   : Node_Id;
      Q   : Node_Id;
      Src : Node_Id;
      Dst : Node_Id;
   begin
      N := Make_XML_Node ("access");

      Set_Str_To_Name_Buffer ("type");
      R := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer ("subprogram");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      Src := Item (AIN.First_Node (Path (Source (E))));
      Dst := Item (AIN.First_Node (Path (Destination (E))));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Src))));
      Set_Str_To_Name_Buffer ("src");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Dst))));
      Set_Str_To_Name_Buffer ("dst");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      return N;
   end Map_Subprogram_Access;

   -------------------------
   -- Map_Port_Connection --
   -------------------------

   function Map_Port_Connection (E : Node_Id) return Node_Id is
      N   : Node_Id;
      R   : Node_Id;
      Q   : Node_Id;
      Src : Node_Id;
      Dst : Node_Id;
   begin
      N := Make_XML_Node ("Connection");

      Set_Str_To_Name_Buffer ("type");
      R := Make_Defining_Identifier (Name_Find);

      if Get_Category_Of_Connection (E) = CT_Data then
         if Get_Category_Of_Connection (E) = CT_Event then
            Set_Str_To_Name_Buffer ("eventdata");
         else
            Set_Str_To_Name_Buffer ("data");
         end if;
      elsif Get_Category_Of_Connection (E) = CT_Event then
         Set_Str_To_Name_Buffer ("event");
      end if;
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      Src := Item (AIN.First_Node (Path (Source (E))));
      Dst := Item (AIN.First_Node (Path (Destination (E))));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Src))));
      Set_Str_To_Name_Buffer ("src");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Dst))));
      Set_Str_To_Name_Buffer ("dst");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));
      return N;
   end Map_Port_Connection;

   --------------------
   -- Map_Bus_Access --
   --------------------

   function Map_Bus_Access (E : Node_Id) return Node_Id is
      N   : Node_Id;
      R   : Node_Id;
      Q   : Node_Id;
      Src : Node_Id;
      Dst : Node_Id;
   begin
      N := Make_XML_Node ("access");

      Set_Str_To_Name_Buffer ("type");
      R := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer ("bus");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      Src := Item (AIN.First_Node (Path (Source (E))));
      Dst := Item (AIN.First_Node (Path (Destination (E))));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Src))));
      Set_Str_To_Name_Buffer ("src");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Dst))));
      Set_Str_To_Name_Buffer ("dst");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      return N;
   end Map_Bus_Access;

   ---------------------
   -- Map_Data_Access --
   ---------------------

   function Map_Data_Access (E : Node_Id) return Node_Id is
      N   : Node_Id;
      R   : Node_Id;
      Q   : Node_Id;
      Src : Node_Id;
      Dst : Node_Id;
   begin
      N := Make_XML_Node ("access");

      Set_Str_To_Name_Buffer ("type");
      R := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer ("data");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      Src := Item (AIN.First_Node (Path (Source (E))));
      Dst := Item (AIN.First_Node (Path (Destination (E))));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Src))));
      Set_Str_To_Name_Buffer ("src");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Dst))));
      Set_Str_To_Name_Buffer ("dst");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      return N;
   end Map_Data_Access;

   ----------------
   -- Map_System --
   ----------------

   function Map_System
     (E       : Node_Id;
      Is_Root : Boolean := False) return Node_Id
   is
      N : Node_Id;
      Q : Node_Id;
      P : Node_Id;
   begin
      N := Make_XML_Node ("system");

      Check_Mils_Enforcement (E, N);

      --  Set the name of the system

      P := Make_Defining_Identifier (To_XML_Name (AIN.Name (Identifier (E))));

      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      Set_Str_To_Name_Buffer ("root");
      Q := Make_Defining_Identifier (Name_Find);

      if Is_Root then
         Set_Str_To_Name_Buffer ("yes");
      else
         Set_Str_To_Name_Buffer ("no");
      end if;

      P := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      return N;
   end Map_System;

   ----------------------------
   -- Check_Mils_Enforcement --
   ----------------------------

   procedure Check_Mils_Enforcement (E : Node_Id; N : Node_Id) is
      Q : Node_Id;
      R : Node_Id;
   begin
      Set_Str_To_Name_Buffer ("mils_checked");
      Q := Make_Defining_Identifier (Name_Find);
      if Get_POK_Mils_Verified (E) then
         Set_Str_To_Name_Buffer ("true");
      else
         Set_Str_To_Name_Buffer ("false");
      end if;
      R := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));
   end Check_Mils_Enforcement;

   -----------------
   -- Map_Process --
   -----------------

   function Map_Process (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("process");

      Check_Mils_Enforcement (E, N);

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      return N;
   end Map_Process;

   ---------------------
   -- Map_Virtual_Bus --
   ---------------------

   function Map_Virtual_Bus (E : Node_Id) return Node_Id is
      N              : Node_Id;
      R              : Node_Id;
      Q              : Node_Id;
      Security_Level : Unsigned_Long_Long := 0;
   begin
      N := Make_XML_Node ("vbus");

      --  Set the name

      Set_Str_To_Name_Buffer ("name");
      R := Make_Defining_Identifier (Name_Find);

      Q :=
        Make_Defining_Identifier (To_XML_Name (Display_Name (Identifier (E))));

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      --  Set the security level

      Security_Level := Get_Security_Level (E);

      R := Make_Literal (XV.New_Numeric_Value (Security_Level, 1, 10));
      Set_Str_To_Name_Buffer ("security_level");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));
      return N;
   end Map_Virtual_Bus;

   ---------------------
   -- Get_Virtual_Bus --
   ---------------------

   function Get_Virtual_Bus (E : Node_Id) return Node_Id is
      Virtual_Bus : Node_Id;
      R           : Node_Id;
   begin
      Virtual_Bus := Get_Provided_Virtual_Bus_Class (E);

      if Present (Virtual_Bus)
        and then No (Get_Handling (Virtual_Bus, By_Name, H_X_Virtual_Bus))
      then
         R := Map_Virtual_Bus (Virtual_Bus);
         Set_Handling (Virtual_Bus, By_Name, H_X_Virtual_Bus, R);
         Append_Node_To_List (R, XTN.Subitems (Stats_Root_Node));
      end if;
      return Virtual_Bus;
   end Get_Virtual_Bus;

   -----------------------
   -- Get_Virtual_Buses --
   -----------------------

   function Get_Virtual_Buses (E : Node_Id) return List_Id is
      Virtual_Bus : Node_Id;
      R           : Node_Id;
      K           : Node_Id;
      L           : List_Id;
   begin
      L := Get_Bounded_Virtual_Bus_Classes (E);

      if L /= No_List then
         K := ATN.First_Node (L);

         while Present (K) loop
            Virtual_Bus := ATE.Get_Referenced_Entity (K);

            if No (Get_Handling (Virtual_Bus, By_Name, H_X_Virtual_Bus)) then
               R := Map_Virtual_Bus (Virtual_Bus);
               Set_Handling (Virtual_Bus, By_Name, H_X_Virtual_Bus, R);
               Append_Node_To_List (R, XTN.Subitems (Stats_Root_Node));
            end if;
            K := ATN.Next_Node (K);
         end loop;

         return L;
      end if;

      return No_List;
   end Get_Virtual_Buses;

   ----------------------------
   -- Map_Virtual_Bus_Layers --
   ----------------------------

   procedure Map_Virtual_Bus_Layers (E : Node_Id; X : Node_Id) is
      K : Node_Id;
      R : Node_Id;
      Q : Node_Id;
      S : Node_Id;
      J : Node_Id;
      L : List_Id;
   begin
      L := Get_Virtual_Buses (E);

      if L /= No_List then

         J := ATN.First_Node (L);

         while Present (J) loop
            S := Make_XML_Node ("layer");
            K := ATE.Get_Referenced_Entity (J);

            Set_Str_To_Name_Buffer ("name");
            Q := Make_Defining_Identifier (Name_Find);
            R :=
              Make_Defining_Identifier
                (To_XML_Name (Display_Name (Identifier (K))));
            Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (S));
            Append_Node_To_List (S, XTN.Subitems (X));
            J := ATN.Next_Node (J);
         end loop;
      end if;
   end Map_Virtual_Bus_Layers;

   --------------------
   -- Map_Subprogram --
   --------------------

   function Map_Subprogram (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("subprogram");

      Map_Virtual_Bus_Layers (E, N);

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));
      return N;
   end Map_Subprogram;

   -------------------
   -- Map_Processor --
   -------------------

   function Map_Processor (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("processor");

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));
      Map_Scheduler (E, N);
      Map_Virtual_Bus_Layers (E, N);
      return N;
   end Map_Processor;

   -------------
   -- Map_Bus --
   -------------

   function Map_Bus (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("bus");

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      Map_Virtual_Bus_Layers (E, N);
      return N;
   end Map_Bus;

   ---------------------------
   -- Map_Virtual_Processor --
   ---------------------------

   function Map_Virtual_Processor (E : Node_Id) return Node_Id is
      N              : Node_Id;
      P              : Node_Id;
      Q              : Node_Id;
      Security_Level : Unsigned_Long_Long;
   begin
      N := Make_XML_Node ("virtual_processor");

      Map_Virtual_Bus_Layers (E, N);

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      --  Now, specify the security level
      Security_Level := Get_Security_Level_Through_Virtual_Bus (E);

      if Security_Level > 0 then
         P := Make_Literal (XV.New_Numeric_Value (Security_Level, 1, 10));
         Set_Str_To_Name_Buffer ("security_level");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));
      end if;

      Map_Scheduler (E, N);

      return N;
   end Map_Virtual_Processor;

   --------------
   -- Map_Data --
   --------------

   function Map_Data (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("data");

      Map_Virtual_Bus_Layers (E, N);

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));
      return N;
   end Map_Data;

   --------------
   -- Map_Port --
   --------------

   function Map_Port (F : Node_Id) return Node_Id is
      N        : Node_Id;
      P        : Node_Id;
      Q        : Node_Id;
      R        : Node_Id;
      D        : Node_Id;
      Nb_Dests : Unsigned_Long_Long := 0;
      Nb_Srcs  : Unsigned_Long_Long := 0;
   begin
      N := Make_XML_Node ("port");

      --  Add the direction of the port as attribute

      Set_Str_To_Name_Buffer ("direction");
      P := Make_Defining_Identifier (Name_Find);
      if Is_In (F) and then not Is_Out (F) then
         Set_Str_To_Name_Buffer ("in");
      elsif Is_Out (F) and then not Is_In (F) then
         Set_Str_To_Name_Buffer ("out");
      else
         Set_Str_To_Name_Buffer ("inout");
      end if;
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      --  Add the name of the port as an attribute

      Set_Str_To_Name_Buffer ("name");
      R := Make_Defining_Identifier (Name_Find);

      Q :=
        Make_Defining_Identifier (To_XML_Name (Display_Name (Identifier (F))));

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      if Get_POK_Refresh_Time (F) /= Null_Time then
         Set_Str_To_Name_Buffer ("refresh_time");
         R := Make_Defining_Identifier (Name_Find);
         Q := Map_Time (Get_POK_Refresh_Time (F));
         Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));
      end if;

      if Get_Compute_Deadline (F) /= Null_Time then
         Set_Str_To_Name_Buffer ("deadline");
         R := Make_Defining_Identifier (Name_Find);
         Q := Map_Time (Get_Compute_Deadline (F));
         Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));
      end if;

      if Is_Out (F) then
         if AINU.Is_Empty (Destinations (F)) then
            Display_Located_Error
              (ATN.Loc (F),
               "This out port should be connected",
               Fatal => True);
         end if;

         D := AIN.First_Node (Destinations (F));

         --  For each out port, we add each destination

         while Present (D) loop
            Nb_Dests := Nb_Dests + 1;

            P := Make_XML_Node ("destination");

            Set_Str_To_Name_Buffer ("name");
            R := Make_Defining_Identifier (Name_Find);

            Q :=
              Make_Defining_Identifier
                (To_XML_Name (Display_Name (Identifier (Item (D)))));

            Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));

            if Get_POK_Refresh_Time (Item (D)) /= Null_Time then
               Set_Str_To_Name_Buffer ("refresh_time");
               R := Make_Defining_Identifier (Name_Find);
               Q := Map_Time (Get_POK_Refresh_Time (Item (D)));
               Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
            end if;

            if Get_Compute_Deadline (Item (D)) /= Null_Time then
               Set_Str_To_Name_Buffer ("deadline");
               R := Make_Defining_Identifier (Name_Find);
               Q := Map_Time (Get_Compute_Deadline (Item (D)));
               Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
            end if;

            if Parent_Component (Item (D)) /= No_Node then
               if AINU.Is_Thread (Parent_Component (Item (D))) then
                  Set_Str_To_Name_Buffer ("containing_component_kind");
                  R := Make_Defining_Identifier (Name_Find);

                  Set_Str_To_Name_Buffer ("thread");
                  Q := Make_Defining_Identifier (Name_Find);

                  Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
               elsif AINU.Is_System (Parent_Component (Item (D))) then
                  Set_Str_To_Name_Buffer ("containing_component_kind");
                  R := Make_Defining_Identifier (Name_Find);

                  Set_Str_To_Name_Buffer ("system");
                  Q := Make_Defining_Identifier (Name_Find);

                  Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
               elsif AINU.Is_Process (Parent_Component (Item (D))) then
                  Set_Str_To_Name_Buffer ("containing_component_kind");
                  R := Make_Defining_Identifier (Name_Find);

                  Set_Str_To_Name_Buffer ("process");
                  Q := Make_Defining_Identifier (Name_Find);

                  Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
               end if;

               Set_Str_To_Name_Buffer ("containing_component");
               R := Make_Defining_Identifier (Name_Find);

               Q :=
                 Make_Defining_Identifier
                   (To_XML_Name
                      (Display_Name
                         (Identifier
                            (Parent_Subcomponent
                               (Parent_Component (Item (D)))))));

               Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
            end if;

            --  Get the security levels of the port
            Map_Virtual_Bus_Layers (Item (D), P);

            --  Add the Node to the subitems
            Append_Node_To_List (P, XTN.Subitems (N));

            D := AIN.Next_Node (D);
         end loop;

         --  Add the amount of destination in the items

         Set_Str_To_Name_Buffer ("dests");
         P := Make_Defining_Identifier (Name_Find);
         Q := Make_Literal (XV.New_Numeric_Value (Nb_Dests, 1, 10));
         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      end if;

      if Is_In (F) then

         if AINU.Is_Empty (Sources (F)) then
            Display_Located_Error
              (ATN.Loc (F),
               "This in port should be connected",
               Fatal => True);
         end if;

         D := AIN.First_Node (Sources (F));

         --  For each out port, we add each destination

         while Present (D) loop
            Nb_Srcs := Nb_Srcs + 1;

            P := Make_XML_Node ("source");

            Set_Str_To_Name_Buffer ("name");
            R := Make_Defining_Identifier (Name_Find);

            Q :=
              Make_Defining_Identifier
                (To_XML_Name (Display_Name (Identifier (Item (D)))));

            Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));

            if Get_POK_Refresh_Time (Item (D)) /= Null_Time then
               Set_Str_To_Name_Buffer ("refresh_time");
               R := Make_Defining_Identifier (Name_Find);
               Q := Map_Time (Get_POK_Refresh_Time (Item (D)));
               Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
            end if;

            if Get_Compute_Deadline (Item (D)) /= Null_Time then
               Set_Str_To_Name_Buffer ("deadline");
               R := Make_Defining_Identifier (Name_Find);
               Q := Map_Time (Get_Compute_Deadline (Item (D)));
               Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
            end if;

            if Parent_Component (Item (D)) /= No_Node then
               if AINU.Is_Thread (Parent_Component (Item (D))) then
                  Set_Str_To_Name_Buffer ("containing_component_kind");
                  R := Make_Defining_Identifier (Name_Find);

                  Set_Str_To_Name_Buffer ("thread");
                  Q := Make_Defining_Identifier (Name_Find);

                  Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
               elsif AINU.Is_System (Parent_Component (Item (D))) then
                  Set_Str_To_Name_Buffer ("containing_component_kind");
                  R := Make_Defining_Identifier (Name_Find);

                  Set_Str_To_Name_Buffer ("system");
                  Q := Make_Defining_Identifier (Name_Find);

                  Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
               elsif AINU.Is_Process (Parent_Component (Item (D))) then
                  Set_Str_To_Name_Buffer ("containing_component_kind");
                  R := Make_Defining_Identifier (Name_Find);

                  Set_Str_To_Name_Buffer ("process");
                  Q := Make_Defining_Identifier (Name_Find);

                  Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
               end if;

               Set_Str_To_Name_Buffer ("containing_component");
               R := Make_Defining_Identifier (Name_Find);

               Q :=
                 Make_Defining_Identifier
                   (To_XML_Name
                      (Display_Name
                         (Identifier
                            (Parent_Subcomponent
                               (Parent_Component (Item (D)))))));

               Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (P));
            end if;

            --  Get the security levels of the port
            Map_Virtual_Bus_Layers (Item (D), P);

            --  Add the Node to the subitems
            Append_Node_To_List (P, XTN.Subitems (N));

            D := AIN.Next_Node (D);
         end loop;
         Set_Str_To_Name_Buffer ("srcs");
         P := Make_Defining_Identifier (Name_Find);
         Q := Make_Literal (XV.New_Numeric_Value (Nb_Srcs, 1, 10));
         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));
      end if;

      --  Now, specify the security level
      Map_Virtual_Bus_Layers (F, N);

      return N;
   end Map_Port;

   ----------------
   -- Map_Thread --
   ----------------

   function Map_Thread (E : Node_Id) return Node_Id is
      P : Node_Id;
      Q : Node_Id;
      N : Node_Id;
   begin
      N := Make_XML_Node ("thread");

      Map_Virtual_Bus_Layers (E, N);

      Check_Mils_Enforcement (E, N);

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));
      return N;
   end Map_Thread;
end Ocarina.Backends.Stats.Mapping;
