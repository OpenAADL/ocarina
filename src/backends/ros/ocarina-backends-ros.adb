------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . R O S                    --
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
use Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Ocarina.Backends.Ros is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   use Ocarina.Namet;
   use Ada.Text_IO;
   use Ada.Strings.Fixed;
   use Ocarina.ME_AADL;
   use Ocarina.Backends.Utils;
   use AIN;

   FD_System         : File_Type;

   FD               : File_Type;
   Root_System_Name : Name_Id;

   --------------------------------------
   -- Procedura che visita i vari nodi --
   --------------------------------------

   procedure Visit (E : Node_Id);

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      --  Registration of the generator

      Register_Backend ("ros", Generate'Access, Ros_Backend);
   end Init;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;

   begin
      --  Instantiate the AADL tree

      Instance_Root := Instantiate_Model (AADL_Root);
      if No (Instance_Root) then
         raise Program_Error;
      end if;

      Visit (Root_System (Instance_Root));
   end Generate;


   -----------
   -- Visit --
   -----------
   procedure Visit (E : Node_Id) is
      Category    : constant Component_Category := Get_Category_Of_Component (E);
      Comp_Name   : constant Name_Id := Normalize_Name (Display_Name (Identifier (E)));
      T           : Node_Id;
   begin

      -- Ogni system ha un suo launch file, quindi non appena trovo un sistema creo un file
      -- che lo gestisca
      if Category = CC_System then
         Create (File => FD_System, Name => To_Lower (Get_Name_String (Comp_Name)) & ".launch");
         Put_Line (FD_System, "<launch>");
         Put_Line ("Creo file launch per system " & To_Lower (Get_Name_String (Comp_Name)));   
      end if;

      Put_Line ("Visito: " & Get_Name_String (Comp_Name) & " - " & Component_Category'Image (Category));
      New_Line;

      -- Controllo se il nodo che sto visitando possiede sottocomponenti
      if Present (Subcomponents (E)) then

         T := First_Node (Subcomponents (E));

         while Present (T) loop
            declare
               Subcomponent_Name : constant String :=
                  To_Lower
                     (Get_Name_String
                        (Normalize_Name
                           (Fully_Qualified_Instance_Name
                              (Corresponding_Instance (T)))));
               Classifier_Name : constant String := 
                  Get_Name_String
                     (Display_Name
                        (Identifier
                           (Corresponding_Instance (T))));
               S         : constant String := Classifier_Name (1..Index (Classifier_Name, ".", 1) - 1);
               File_Name : String(1..200);
            begin
               -- Se fra tutti i sottocomponenti ho un componente di tipo
               -- process allora aggiungo una riga al file launch
               if Get_Category_Of_Subcomponent (T) = CC_Process then
                  File_Name := Classifier_Name;
                  Put_Line (FD_System, ASCII.HT & "<node pkg=""test"" type=""" & S & """ name=""" & Subcomponent_Name & """ />");
                  Put_Line (File_Name);
               end if;

               T := Next_Node (T);
            end;
         end loop;
         
      end if;

      Put_Line (FD_System, "</launch>");
      Close (FD_System);

   end Visit;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
   end Reset;

end Ocarina.Backends.Ros;
