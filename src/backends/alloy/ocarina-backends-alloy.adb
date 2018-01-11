------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . A L L O Y                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2014-2018 ESA & ISAE.                    --
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

with Ada.Text_IO;

with Charset;           use Charset;
with Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Instances; use Ocarina.Instances;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Helper;
with Ocarina.Backends.Utils;

package body Ocarina.Backends.Alloy is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   use Ocarina.Namet;
   use Ada.Text_IO;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.Backends.Helper;
   use Ocarina.Backends.Utils;
   use AIN;

   procedure Visit (E : Node_Id);
   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

   FD               : File_Type;
   Root_System_Name : Name_Id;

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
      Category : constant Component_Category := Get_Category_Of_Component (E);

      E_Subcomponents : constant Node_Array := Subcomponents_Of (E);
      E_Properties : constant Node_Array := Properties_Of (E);

   begin
      --  Create Alloy component

      --  Rule #1: the name of this component is deduced from the name
      --  of the corresponding instance name

      if Present (Parent_Subcomponent (E)) then
         Put_Line
           (FD,
            "one sig " &
            To_Lower
              (Get_Name_String
                 (Normalize_Name (Fully_Qualified_Instance_Name (E)))) &
            " extends Component{}{");

      else
         Root_System_Name := Normalize_Name (Display_Name (Identifier (E)));

         Put_Line
           (FD,
            "one sig " &
            To_Lower (Get_Name_String (Root_System_Name)) &
            " extends Component{}{");
      end if;

      Put_Line (FD, ASCII.HT & "type=" & Category_Name (Category));

      --  Rule #2: list subcomponents

      Put (FD, ASCII.HT & "subcomponents=");

      if E_Subcomponents'Length > 0 then
         for J in E_Subcomponents'Range loop
            declare
               Subcomponent_Name : constant String :=
                 To_Lower
                   (Get_Name_String
                      (Normalize_Name
                         (Fully_Qualified_Instance_Name
                            (Corresponding_Instance (E_Subcomponents (J))))));
            begin
               Put (FD, Subcomponent_Name);

               if J < E_Subcomponents'Last then
                  Put (FD, "+");
               end if;
            end;
         end loop;
         New_Line (FD);

      else
         Put_Line (FD, "none");
      end if;

      --  Rule #3: list properties

      Put (FD, ASCII.HT & "properties=");

      if E_Properties'Length > 0 then
         for J in E_Properties'Range loop
            Put
              (FD,
               To_Lower
                 (Get_Name_String
                    (Normalize_Name (Display_Name (Identifier
                                                     (E_Properties (J)))))));

            if J < E_Properties'Last then
               Put (FD, "+");
            end if;
         end loop;
         New_Line (FD);

      else
         Put_Line (FD, "none");
      end if;

      --  Close create of component

      Put_Line (FD, "}");
      New_Line (FD);

      --  Iterate over subcomponents

      Visit_Subcomponents_Of (E);

   end Visit_Component_Instance;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      --  Registration of the generator

      Register_Backend ("alloy", Generate'Access, Alloy_Backend);
   end Init;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      --  Instantiate the AADL tree
      Instance_Root : constant Node_Id := Instantiate_Model (AADL_Root);

      Root_Subcomponents : constant Node_Array := Subcomponents_Of
        (Root_System (Instance_Root));

   begin
      --  Open a new .als file

      Create (File => FD, Name => "con_model.als");

      Put_Line (FD, "// This file has been generated by Ocarina");
      Put_Line (FD, "// DO NOT EDIT IT");
      New_Line (FD);
      Put_Line (FD, "module con_model");
      Put_Line (FD, "open lib/data_structure");
      New_Line (FD);

      --  Visit instance model

      New_Line (FD);
      Put_Line (FD, "// Mapping of the AADL instance tree");
      New_Line (FD);

      Visit (Instance_Root);

      --  Add global contract

      New_Line (FD);
      Put_Line
        (FD,
         "// Declaration of the contract(s) " & "representing the model(s)");
      New_Line (FD);
      Put_Line (FD, "one sig aadl_model extends Contract{}{");
      Put_Line (FD, ASCII.HT & "assumption=none");
      Put_Line (FD, ASCII.HT & "input=none");
      Put_Line (FD, ASCII.HT & "guarantee=none");

      --  Generate output

      declare
         Print_Subcomponents : Boolean          := True;
      begin
         --  We consider two patterns
         --  a) system with subcomponents as system/bus/device only
         --  b) general case

         for Sub of Root_Subcomponents loop
            Print_Subcomponents :=
              Print_Subcomponents
              and then
              (Get_Category_Of_Component (Sub) = CC_System
                 or else Get_Category_Of_Component (Sub) = CC_Device
                 or else Get_Category_Of_Component (Sub) = CC_Bus);
         end loop;

         --  We are in case a), generate all subcomponents of root system

         if Print_Subcomponents then
            Put (FD, ASCII.HT & "output=");
            if Root_Subcomponents'Length > 0 then
               for J in Root_Subcomponents'Range loop
                  declare
                     Subcomponent_Name : constant String :=
                       To_Lower
                       (Get_Name_String
                          (Normalize_Name
                             (Fully_Qualified_Instance_Name
                                (Corresponding_Instance
                                   (Root_Subcomponents (J))))));
                  begin
                     Put (FD, Subcomponent_Name);

                     if J < Root_Subcomponents'Last then
                        Put (FD, "+");
                     end if;
                  end;
               end loop;
               New_Line (FD);

            else
               Put_Line (FD, "none");
            end if;
         else
            --  We are in case b), generate only root system

            Put_Line
              (FD,
               ASCII.HT &
               "output=" &
               To_Lower (Get_Name_String (Root_System_Name)));
         end if;
      end;

      Put_Line (FD, "}");

      --  Close file descriptor

      Close (FD);
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
   end Reset;

end Ocarina.Backends.Alloy;
