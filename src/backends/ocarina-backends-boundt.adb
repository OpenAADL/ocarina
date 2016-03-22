------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              O C A R I N A . B A C K E N D S . B O U N D T               --
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

with Ocarina.Namet;
with Ocarina.Backends.Messages;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Instances;           use Ocarina.Instances;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.Properties; use Ocarina.Backends.Properties;
with Ocarina.Options;             use Ocarina.Options;

with Utils; use Utils;

with Ada.Text_IO;
with GNAT.IO_Aux;

package body Ocarina.Backends.BoundT is

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   use Ocarina.Namet;
   use Ocarina.Backends.Messages;
   use Ada.Text_IO;
   use Ocarina.ME_AADL;
   use AIN;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   procedure Visit (E : Node_Id);

   procedure Visit_Architecture_Instance (E : Node_Id);

   procedure Visit_Component_Instance (E : Node_Id);

   procedure Visit_System_Instance (E : Node_Id);

   procedure Visit_Process_Instance (E : Node_Id);

   procedure Visit_Thread_Instance (E : Node_Id);

   FD            : File_Type;
   Assertions_FD : File_Type;

   P_Model_Name            : Name_Id := No_Name;
   Current_Thread_Instance : Name_Id := No_Name;

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
      use Ocarina.ME_AADL.AADL_Instances.Entities;

      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Process =>
            if Boundt_Process = No_Name
              or else
                To_Lower
                  (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))) =
                Boundt_Process
            then
               Visit_Process_Instance (E);
            end if;

         when CC_Thread =>
            Visit_Thread_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   ---------------------------
   -- Visit_Thread_Instance --
   ---------------------------

   procedure Visit_Thread_Instance (E : Node_Id) is
      Raw_Name : constant String :=
        String'
          (Get_Name_String (P_Model_Name) &
           "_" &
           Get_Name_String (Current_Thread_Instance));
      Base_Name : constant String :=
        String'
          ("polyorb_hi_generated__activity__" &
           Get_Name_String (Current_Thread_Instance));
      Procedure_Name  : constant String := String'(Base_Name & "_job");
      Assertion_Basis : constant String :=
        String'(Base_Name & "_interrogators");
      Assertion_Send : constant String :=
        String'(Assertion_Basis & "__send_outputXn");
      Assertion_Get : constant String :=
        String'(Assertion_Basis & "__get_valueXn");

      T_Dispatch_Protocol : Supported_Thread_Dispatch_Protocol;
      Modes_Nb            : Int;
   begin
      Put_Line (FD, "");
      Put_Line (FD, "  thread " & Raw_Name);

      T_Dispatch_Protocol := Get_Thread_Dispatch_Protocol (E);
      case T_Dispatch_Protocol is
         when Thread_Periodic =>
            Put_Line (FD, "    type cyclic");
         when Thread_Sporadic =>
            Put_Line (FD, "    type sporadic");
         when Thread_Hybrid =>
            --  In PolyORB-HI, an hybrid thread is really a sporadic
            --  thread that receives a period_event every periods
            Put_Line (FD, "    type sporadic");
         when others =>
            raise Program_Error;
      end case;

      --  We can guess the related top subprogram
      --  name simply by adding "_job" the the thread
      --  raw name

      Put_Line (FD, "    root " & Procedure_Name);
      Put_Line (FD, "  end " & Raw_Name);

      --  PolyORB-HI generates unbounded subprograms

      Put_Line (Assertions_FD, "subprogram """ & Assertion_Send & """");
      Put_Line (Assertions_FD, "    time 0 cycles;");
      Put_Line (Assertions_FD, "end;");
      Put_Line (Assertions_FD, "");

      Put_Line (Assertions_FD, "subprogram """ & Assertion_Get & """");
      Put_Line (Assertions_FD, "    time 0 cycles;");
      Put_Line (Assertions_FD, "end;");
      Put_Line (Assertions_FD, "");

      --  If their is modes in the generated thread, then a while loop
      --  is generated. We can bound this loop by the number of modes
      --  defined in the thread

      if not Is_Empty (Modes (E)) then

         Modes_Nb := Int (Length (Modes (E)));

         Put_Line (Assertions_FD, "subprogram """ & Procedure_Name & """");
         Put_Line
           (Assertions_FD,
            "    all loops repeats " &
            Int'Image (Modes_Nb) &
            " times; end loops;");
         Put_Line (Assertions_FD, "end;");
         Put_Line (Assertions_FD, "");
      end if;

   end Visit_Thread_Instance;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process_Instance (E : Node_Id) is
      T : Node_Id;
   begin
      P_Model_Name :=
        ATN.Name
          (ATN.Identifier
             (Corresponding_Declaration (Parent_Subcomponent (E))));

      Put_Line (FD, "program " & Get_Name_String (P_Model_Name));

      --  Then we parse all threads which are subcomponent
      --  of the process

      T := First_Node (Subcomponents (E));
      while Present (T) loop

         case AADL_Version is
            when AADL_V1 =>
               Current_Thread_Instance :=
                 ATN.Name (ATN.Identifier (Corresponding_Declaration (T)));

            when AADL_V2 =>
               Current_Thread_Instance :=
                 Get_String_Name
                   (Get_Name_String
                      (To_Lower
                         (AIN.Name
                            (AIN.Identifier
                               (AIN.Namespace
                                  (AIN.Corresponding_Instance (T)))))) &
                    "_" &
                    Get_Name_String
                      (ATN.Name
                         (ATN.Identifier (Corresponding_Declaration (T)))));
         end case;
         Visit (Corresponding_Instance (T));

         T := Next_Node (T);
      end loop;

      Put_Line (FD, "end " & Get_Name_String (P_Model_Name));
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      --  Visit all the subcomponents of the system

      if not Ocarina.ME_AADL.AADL_Instances.Nutils.Is_Empty
          (Subcomponents (E))
      then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_System_Instance;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Register_Backend ("boundt", Generate'Access, Bound_T);
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

      --  Open a new TPO file

      if Boundt_Process = No_Name then
         Create (File => FD, Name => "scenario.tpo");
      else
         Create
           (File => FD,
            Name => "scenario_" & Get_Name_String (Boundt_Process) & ".tpo");
      end if;

      --  Open the assertion template file
      if not GNAT.IO_Aux.File_Exists ("assertions.txt") then
         Display_Error
           ("No assertion file found, create a new one",
            Fatal   => False,
            Warning => True);
         Create (File => Assertions_FD, Name => "assertions.txt");
      else
         Open
           (File => Assertions_FD,
            Mode => Append_File,
            Name => "assertions.txt");
      end if;

      Put_Line (Assertions_FD, "");
      Put_Line (Assertions_FD, "--  Automatically generated assertions");
      Put_Line (Assertions_FD, "--  DO NOT EDIT !");
      Put_Line (Assertions_FD, "");

      --  Parse all the processes, each one will be an TPO program

      Visit_Architecture_Instance (Instance_Root);

      --  Close file descriptors

      Close (FD);
      Close (Assertions_FD);
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      P_Model_Name            := No_Name;
      Current_Thread_Instance := No_Name;
   end Reset;

end Ocarina.Backends.BoundT;
