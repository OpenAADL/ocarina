------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . E X E C U T I O N _ T E S T S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

with Ocarina.Output;
with Ocarina.Namet;
with System.Address_To_Access_Conversions;

with Ada.Directories;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Real_Time;

with GNAT.Directory_Operations;
with GNAT.Directory_Operations.Iteration;

with Ocarina.Backends.Execution_Utils;
with Ocarina.Backends.Utils;

package body Ocarina.Backends.Execution_Tests is

   use Ocarina.Output;
   use Ocarina.Namet;
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded.Text_IO;
   use Ada.Strings.Unbounded;
   use GNAT.Directory_Operations;
   use Ocarina.Backends.Execution_Utils;
   use Ocarina.Backends.Utils;
   use String_String_Maps;

   package Addr_To_Acc is new System.Address_To_Access_Conversions
     (Unbounded_String);

   procedure Exit_On_Error (Error : Boolean; Reason : String);

   -------------------
   -- Exit_On_Error --
   -------------------

   procedure Exit_On_Error (Error : Boolean; Reason : String) is
   begin
      if Error then
         Set_Standard_Error;
         Write_Line (Reason);
         OS_Exit (1);
      end if;
   end Exit_On_Error;

   ----------
   -- Init --
   ----------

   procedure Init is
   --  --  Example to find the size of the Pattern_Matcher Parse_Regexp :
   --  Parse_Regexp : constant Pattern_Matcher :=
   --    Compile ("(\[-? *[0-9]+\.[0-9]* *\])", Single_Line);
   begin
      --  Put_Line (Integer'Image (Parse_Regexp'Size));
      --  --  => 1184, the real Pattern_Matcher will be (1184 - 160) / 8 = 128
      --  --  160 is the base size of a Pattern_Matcher
      TSim_ERC32_Path   := GNAT.OS_Lib.Locate_Exec_On_Path ("tsim-erc32");
      TSim_LEON_Path    := GNAT.OS_Lib.Locate_Exec_On_Path ("tsim-leon");
      Qemu_Path         := GNAT.OS_Lib.Locate_Exec_On_Path ("qemu");
      Qemu_Sparc_Path := GNAT.OS_Lib.Locate_Exec_On_Path ("qemu-system-sparc");
      Xcov_Path         := GNAT.OS_Lib.Locate_Exec_On_Path ("xcov");
      Command_Name_Path :=
        new String'
          (Dir_Name
             (Ada.Directories.Full_Name (Ada.Command_Line.Command_Name)));
      Compile (Parse_Regexp, "(\[-? *[0-9]+\.[0-9]* *\])", Single_Line);
      Compile (Strip_CR_Regexp, "(\r)", Single_Line);
      Compile (Header_End_Regexp, "(resuming at.*)", Multiple_Lines);
      Compile
        (Tsim_Traces_Regexp,
         "(tsim>|Program exited normally.)",
         Single_Line);
      Compile (Empty_Line_Regexp, "(^\n)", Multiple_Lines);
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Free (TSim_ERC32_Path);
      Free (TSim_LEON_Path);
      Free (Qemu_Path);
      Free (Xcov_Path);
      Free (All_Traces);
      Free (Scenario_Dir);
      Free (Command_Name_Path);
   end Reset;

   -------------------
   -- No_Regression --
   -------------------

   function No_Regression
     (Trace    : Unbounded_String;
      Ref      : Unbounded_String;
      App      : Name_Id;
      Ref_Path : String) return Boolean
   is
      File           : File_Type;
      Trace_Filtered : String_Ptr;
      Ref_Filtered   : String_Ptr;
      Padding        : Integer;
   begin
      Write_Eol;
      Write_Line
        ("--- Testing regression for : " & Get_Name_String (App) & " ---");
      Write_Line ("Using following referencial : " & Ref_Path);

      Filter_Line (To_String (Trace), Trace_Filtered);
      Filter_Line (To_String (Ref), Ref_Filtered);

      if Write_Log then
         declare
            Referencial_Log_File : constant String :=
              Get_Current_Dir & "log." & Get_Name_String (App) & ".ref.txt";
            Trace_Log_File : constant String :=
              Get_Current_Dir & "log." & Get_Name_String (App) & ".trace.txt";
         begin
            Write_Line
              ("Writing log for referencial in : " & Referencial_Log_File);
            Create (File, Out_File, Referencial_Log_File);
            Put (File, Ref_Filtered.all);
            Close (File);

            Write_Line ("Writing log for trace in : " & Trace_Log_File);
            Create (File, Out_File, Trace_Log_File);
            Put (File, Trace_Filtered.all);
            Close (File);
         end;
      end if;

      if Trace_Filtered.all'Length < Ref_Filtered.all'Length then
         Write_Line
           ("Warning : trace length (" &
            Trace_Filtered.all'Length'Img &
            " ) is lower than referencial length (" &
            Ref_Filtered.all'Length'Img &
            " )");
      end if;

      Padding := Trace_Filtered'First - Ref_Filtered'First;
      for I in Ref_Filtered.all'Range loop
         exit when I + Padding > Trace_Filtered.all'Last;

         if Ref_Filtered.all (I) /= Trace_Filtered.all (I + Padding) then
            Write_Line
              ("Failed at char position = " &
               I'Img &
               " (ref = '" &
               Ref_Filtered.all (I) &
               "' trace= '" &
               Trace_Filtered.all (I + Padding) &
               "')");
            return False;
         end if;
      end loop;
      return True;
   end No_Regression;

   ----------------------
   -- Load_Referencial --
   ----------------------

   function Load_Referencial (File_Path : String) return Unbounded_String is
      File                 : File_Type;
      Complete_File_String : Unbounded_String;
   begin
      Open (File, In_File, File_Path);

      loop
         exit when End_Of_File (File);
         Append (Complete_File_String, To_String (Get_Line (File)) & ASCII.LF);
      end loop;

      Close (File);
      return Complete_File_String;
   end Load_Referencial;

   ------------------------
   --  Write_Referencial --
   ------------------------

   procedure Write_Referencial
     (File_Path : String;
      Ref       : Unbounded_String;
      App       : Name_Id)
   is
      File : File_Type;
   begin
      Write_Line
        ("Writing referencial for " &
         Get_Name_String (App) &
         " to : " &
         File_Path);
      Create (File, Out_File, File_Path);
      Put_Line (File, To_String (Ref));
      Close (File);
   end Write_Referencial;

   -----------------------------
   -- Execute_Regression_Test --
   -----------------------------

   function Execute_Regression_Test
     (Scenario_Dirname : String;
      Ref_Map          : Map;
      Timeout          : Natural) return Boolean
   is
      Result       : Expect_Match;
      Referencial  : Unbounded_String;
      Return_Value : Boolean      := True;
      TimeoutVar   : Integer      := Timeout;
      First        : constant Int := Ref_Name_Tables.First;
      Last         : constant Int := Ref_Name_Tables.Last (Process_List);
      Processes    : Fd_Array (Integer (First) .. Integer (Last));
      M            : Process_Type;
      Position     : Cursor;
      Ref_Path     : String_Ptr;
   begin
      All_Traces := new Trace (Integer (First) .. Integer (Last));

      for J in First .. Last loop
         M        := Process_List.Table (J);
         Position := Find (Ref_Map, Get_Name_String (M.Node_Name));

         if Position = No_Element then
            Exit_On_Error
              (True,
               "Error : no referencial defined for " &
               Get_Name_String (M.Node_Name));
         end if;

         declare
            --  Application path
            Appli_File : constant String :=
              Get_Current_Dir &
              Get_Name_String (M.Appli_Name) &
              Dir_Separator &
              Get_Binary_Location (Get_Current_Backend_Kind, M.Node_Name);

            Ref_File : constant String :=
              Scenario_Dirname & Element (Position);

         begin
            if not Ada.Directories.Exists (Ref_File) then
               Create_Referencial := True;
            end if;

            Exit_On_Error
              (not Ada.Directories.Exists (Appli_File),
               "Error : application " & Appli_File & " does not exist");

            Write_Line ("Launching : " & Appli_File);
            Launch_Test
              (Processes (Integer (J)),
               Appli_File,
               M.Execution_Platform,
               TimeoutVar);
            Add_Filter
              (Processes (Integer (J)),
               Filter_Procedure'Access,
               GNAT.Expect.Output,
               All_Traces (Integer (J))'Address);
         end;
      end loop;

      Write_Eol;

      if M.Execution_Platform = Platform_LEON_ORK
        or else M.Execution_Platform = Platform_ERC32_ORK
      then
         --  Let tsim deal with the timeout
         TimeoutVar := -1;
      end if;

      for J in First .. Last loop
         M        := Process_List.Table (J);
         Ref_Path :=
           new String'
             (Scenario_Dirname &
              Element (Ref_Map, Get_Name_String (M.Node_Name)));

         begin
            Expect (Processes (Integer (J)), Result, Never_Match, TimeoutVar);
         exception
            when GNAT.Expect.Process_Died =>
               Write_Line
                 ("Warning: process " &
                  Get_Name_String (M.Node_Name) &
                  " has died during test phase");
         end;

         Close (Processes (Integer (J)));

         if not Create_Referencial then
            Referencial := Load_Referencial (Ref_Path.all);
            if M.Execution_Platform = Platform_LEON_ORK
              or else M.Execution_Platform = Platform_ERC32_ORK
            then
               Append (All_Traces (Integer (J)), ASCII.LF);
            end if;
            if No_Regression
                (All_Traces (Integer (J)),
                 Referencial,
                 M.Node_Name,
                 Ref_Path.all)
            then
               Write_Line
                 ("--- Regression test result for " &
                  Get_Name_String (M.Node_Name) &
                  " : SUCCESS ---");
            else
               Return_Value := False;
               Write_Line
                 ("--- Regression test result for " &
                  Get_Name_String (M.Node_Name) &
                  " : !!! FAILED !!! ---");
            end if;
         else
            Write_Referencial
              (Ref_Path.all,
               All_Traces (Integer (J)),
               M.Node_Name);
         end if;

         Free (Ref_Path);
         TimeoutVar := 0;
      end loop;

      Free (All_Traces);
      return Return_Value;
   end Execute_Regression_Test;

   -----------------
   -- Launch_Test --
   -----------------

   procedure Launch_Test
     (Fd      : out GNAT.Expect.Process_Descriptor;
      Command :     String;
      Arch    :     Supported_Execution_Platform;
      Timeout :     Natural)
   is
   begin
      case Arch is
         when Platform_LEON_ORK =>
            declare
               TSim_LEON_Args : GNAT.OS_Lib.Argument_List (1 .. 1);
            begin
               Exit_On_Error
                 (TSim_LEON_Path = null,
                  "Error : tsim-leon not found in PATH");
               Header_Has_Ended   := False;
               TSim_LEON_Args (1) := new String'(Command);

               Non_Blocking_Spawn
                 (Descriptor  => Fd,
                  Command     => TSim_LEON_Path.all,
                  Args        => TSim_LEON_Args,
                  Buffer_Size => 128000,
                  Err_To_Out  => True);
            end;
            Send
              (Fd,
               Tsim_Cmd ("go 0x40000000 " & Integer'Image (Timeout) & " ms"));
            GNAT.OS_Lib.Close (Get_Input_Fd (Fd));

         when Platform_LEON_GNAT =>
            declare
               Args : GNAT.OS_Lib.Argument_List (1 .. 5);
            begin
               Exit_On_Error
                 (Qemu_Sparc_Path = null,
                  "Error : qemu_system_sparc not found in PATH");

               Write_Line ("Launching qemu_system_sparc");
               Args (1) := new String'("-nographic");
               Args (2) := new String'("-M");
               Args (3) := new String'("at697");
               Args (4) := new String'("-kernel");
               Args (5) := new String'(Command);

               GNAT.Expect.Non_Blocking_Spawn
                 (Descriptor  => Fd,
                  Command     => Qemu_Sparc_Path.all,
                  Args        => Args,
                  Buffer_Size => 128000,
                  Err_To_Out  => True);

            end;

         when Platform_ERC32_ORK =>
            declare
               TSim_ERC32_Args : GNAT.OS_Lib.Argument_List (1 .. 1);
            begin
               Exit_On_Error
                 (TSim_ERC32_Path = null,
                  "Error : tsim-erc32 not found in PATH");
               Header_Has_Ended    := False;
               TSim_ERC32_Args (1) := new String'(Command);

               Non_Blocking_Spawn
                 (Descriptor  => Fd,
                  Command     => TSim_ERC32_Path.all,
                  Args        => TSim_ERC32_Args,
                  Buffer_Size => 128000,
                  Err_To_Out  => True);
            end;
            Send
              (Fd,
               Tsim_Cmd ("go 0x02000000 " & Integer'Image (Timeout) & " ms"));
            GNAT.OS_Lib.Close (Get_Input_Fd (Fd));

         when Platform_Native =>
            declare
               Args : GNAT.OS_Lib.Argument_List (1 .. 1);
            begin
               Args (1) := new String'("");

               GNAT.Expect.Non_Blocking_Spawn
                 (Descriptor  => Fd,
                  Command     => Command,
                  Args        => Args,
                  Buffer_Size => 128000,
                  Err_To_Out  => True);

            end;

         when Platform_LEON_RTEMS =>
            declare
               Args : GNAT.OS_Lib.Argument_List (1 .. 11);
               Dir  : constant String :=
                 Dir_Name (Normalize_Pathname (Command & Dir_Separator));
               App_Name : constant String :=
                 File_Name (Normalize_Pathname (Command));
               File      : File_Type;
               Boot_File : Unbounded_String :=
                 To_Unbounded_String
                   (Command_Name_Path.all &
                    "resources" &
                    Dir_Separator &
                    "rtems-boot.img");
            begin
               Exit_On_Error
                 (Qemu_Path = null,
                  "Error : QEMU not found in PATH");

               if not Ada.Directories.Exists (To_String (Boot_File)) then
                  Boot_File :=
                    To_Unbounded_String
                      (Command_Name_Path.all & "rtems-boot.img");
               end if;

               Exit_On_Error
                 (not Ada.Directories.Exists (To_String (Boot_File)),
                  "Error : QEMU image boot file 'rtems-boot.img' " &
                  "not found");

               Create (File, Out_File, Dir & "rtems-grub.cfg");
               Put_Line (File, "set default=0");
               Put_Line (File, "set timeout=0");
               Put_Line (File, "menuentry ""Ocarina"" {");
               Put_Line (File, "  set root=(hd0,0)");
               Put_Line (File, "  multiboot (hd0,0)/" & App_Name);
               Put_Line (File, "}");
               Close (File);

               Write_Line
                 ("Launching qemu with following dir as hda : " & Dir);
               Args (1)  := new String'("-boot");
               Args (2)  := new String'("a");
               Args (3)  := new String'("-fda");
               Args (4)  := new String'(To_String (Boot_File));
               Args (5)  := new String'("-hda");
               Args (6)  := new String'("fat:" & Dir);
               Args (7)  := new String'("-nographic");
               Args (8)  := new String'("-no-kqemu");
               Args (9)  := new String'("-serial");
               Args (10) := new String'("stdio");
               Args (11) := new String'("-no-reboot");

               GNAT.Expect.Non_Blocking_Spawn
                 (Descriptor  => Fd,
                  Command     => Qemu_Path.all,
                  Args        => Args,
                  Buffer_Size => 128000,
                  Err_To_Out  => True);

            end;

         when others =>
            Exit_On_Error
              (True,
               "Platform " & Arch'Img & " is not supported yet.");
      end case;
   end Launch_Test;

   -------------------------------
   --  Clean_String_From_Regexp --
   -------------------------------

   procedure Clean_String_From_Regexp
     (Str    : in out String_Ptr;
      Regexp :        Pattern_Matcher)
   is
      Matches : Match_Array (1 .. 1);
      Old_Str : String_Ptr;
   begin
      Match (Regexp, Str.all, Matches);
      while Matches (1) /= No_Match loop
         --  backup old string
         Old_Str := Str;

         --  create new parsed string from Output_Regexp
         Str :=
           new String'
             (Old_Str.all (Old_Str.all'First .. Matches (1).First - 1) &
              Old_Str.all (Matches (1).Last + 1 .. Old_Str.all'Last));

         --  Deallocate old string
         Free (Old_Str);
         Match (Regexp, Str.all, Matches);
      end loop;
   end Clean_String_From_Regexp;

   -----------------------------------
   --  Clean_String_From_All_Regexp --
   -----------------------------------

   procedure Clean_String_From_All_Regexp (Str : in out String_Ptr) is
   begin
      Clean_String_From_Regexp (Str, Parse_Regexp);
      Clean_String_From_Regexp (Str, Strip_CR_Regexp);
      Clean_String_From_Regexp (Str, Header_End_Regexp);
      Clean_String_From_Regexp (Str, Tsim_Traces_Regexp);
      Clean_String_From_Regexp (Str, Empty_Line_Regexp);
   end Clean_String_From_All_Regexp;

   -----------------
   -- Filter_Line --
   -----------------

   procedure Filter_Line (Line : String; Output_Str : in out String_Ptr) is
   begin
      Output_Str := new String'(Line);

      --  remove any unwanted characters from Str
      Clean_String_From_All_Regexp (Output_Str);
   end Filter_Line;

   ----------------------
   -- Filter_Procedure --
   ----------------------

   procedure Filter_Procedure
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address)
   is
      Output_Buffer : constant Unbounded_String_Ptr :=
        Unbounded_String_Ptr (Addr_To_Acc.To_Pointer (User_Data));
   begin
      pragma Unreferenced (Descriptor);

      if not Header_Has_Ended then
         if Match (Header_End_Regexp, Str) then
            Header_Has_Ended := True;
         end if;
      else
         Append (Output_Buffer.all, Str);
      end if;
   end Filter_Procedure;

   -------------------------
   -- No_Filter_Procedure --
   -------------------------

   procedure No_Filter_Procedure
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address)
   is
      Output_Buffer : constant Unbounded_String_Ptr :=
        Unbounded_String_Ptr (Addr_To_Acc.To_Pointer (User_Data));
   begin
      pragma Unreferenced (Descriptor);
      Append (Output_Buffer.all, Str);
   end No_Filter_Procedure;

   --------------
   -- Tsim_Cmd --
   --------------

   function Tsim_Cmd (Cmd : String) return String is
   begin
      return Cmd & ASCII.CR;
   end Tsim_Cmd;

   ---------------------------
   -- Execute_Coverage_Test --
   ---------------------------

   function Execute_Coverage_Test (Timeout : Natural) return Boolean is
      use Ada.Real_Time;
      Result       : Expect_Match;
      Return_Value : Boolean;
      TimeoutVar   : Natural            := Timeout;
      First        : constant Int       := Ref_Name_Tables.First;
      Last         : constant Int       := Ref_Name_Tables.Last (Process_List);
      Processes    : Fd_Array (Integer (First) .. Integer (Last));
      M            : Process_Type;
      Span         : constant Time_Span := Seconds (2);
   begin
      if Xcov_Path = null then
         Exit_On_Error (True, "Error: xcov not found");
      end if;

      All_Traces := new Trace (Integer (First) .. Integer (Last));

      --  Launch all processes with xcov to get execution traces

      for J in First .. Last loop
         M := Process_List.Table (J);

         Enter_Directory (M.Appli_Name);

         declare
            Args : GNAT.OS_Lib.Argument_List (1 .. 3);
         begin
            Args (1) := new String'("run");
            if Get_Current_Backend_Kind = PolyORB_Kernel_C then
               Args (2) := new String'("--target=prepare");
            else
               case M.Execution_Platform is
                  when Platform_Native | Platform_None =>
                     Args (2) := new String'("--target=i386-linux");
                  when Platform_LEON_GNAT =>
                     Args (2) := new String'("--target=leon-elf");
                  when others =>
                     Exit_On_Error
                       (True,
                        "Error : This platform is not yet" &
                        " supported for coverage test");
               end case;
            end if;

            Args (3) :=
              new String'
                (Get_Current_Dir &
                 Get_Name_String (M.Appli_Name) &
                 Dir_Separator &
                 Get_Binary_Location (Get_Current_Backend_Kind, M.Node_Name));

            Exit_On_Error
              (not Ada.Directories.Exists (Args (3).all),
               "Error : application " & Args (3).all & " does not exist");
            Write_Line ("Launching : " & Args (3).all);
            if Get_Current_Backend_Kind = PolyORB_Kernel_C then
               declare
                  Args2   : GNAT.OS_Lib.Argument_List (1 .. 8);
                  Success : Boolean;
               begin
                  --  Prepare trace file with xcov
                  GNAT.OS_Lib.Spawn (Xcov_Path.all, Args, Success);

                  --  Get traces with Qemu
                  Args2 (1) := new String'("-fda");
                  Args2 (2) :=
                    new String'
                      (GNAT.OS_Lib.Getenv ("POK_PATH").all &
                       "/misc/grub-boot-only.img");
                  Args2 (3) := new String'("-hda");
                  Args2 (4) := new String'("fat:.");
                  Args2 (5) := new String'("-boot");
                  Args2 (6) := new String'("a");
                  Args2 (7) := new String'("-nographic");
                  Args2 (8) := new String'(Args (3).all);

                  GNAT.Expect.Non_Blocking_Spawn
                    (Descriptor  => Processes (Integer (J)),
                     Command     => Qemu_Path.all,
                     Args        => Args2,
                     Buffer_Size => 128000,
                     Err_To_Out  => True);
               end;
            else
               GNAT.Expect.Non_Blocking_Spawn
                 (Descriptor  => Processes (Integer (J)),
                  Command     => Xcov_Path.all,
                  Args        => Args,
                  Buffer_Size => 128000,
                  Err_To_Out  => True);
            end if;
            Add_Filter
              (Processes (Integer (J)),
               No_Filter_Procedure'Access,
               GNAT.Expect.Output,
               All_Traces (Integer (J))'Address);
         end;
         Leave_Directory;
      end loop;

      --  Stop all processes at the end of the timeout

      for J in First .. Last loop
         M := Process_List.Table (J);
         begin
            Expect (Processes (Integer (J)), Result, Never_Match, TimeoutVar);
         exception
            when GNAT.Expect.Process_Died =>
               Write_Line
                 ("Warning: process " &
                  Get_Name_String (M.Node_Name) &
                  " has died during coverage test phase");
         end;
         Write_Line (To_String (80 * '#'));
         Write_Line (To_String (All_Traces (Integer (J))));
         Write_Line (To_String (80 * '#'));

         --  Send "Ctrl-a x" command to stop qemu

         if Get_Current_Backend_Kind = PolyORB_Kernel_C
           or else M.Execution_Platform = Platform_LEON_GNAT
         then
            Send (Processes (Integer (J)), ASCII.SOH & 'x');
         end if;
         Close (Processes (Integer (J)));
         TimeoutVar := 0;
      end loop;

      --  Remove outputs from traces

      Free (All_Traces);
      All_Traces := new Trace (Integer (First) .. Integer (Last));

      --  Extract the list of functions to analyze from object files

      for J in First .. Last loop
         M := Process_List.Table (J);
         declare
            File    : File_Type;
            O_Files : Unbounded_String;

            procedure Add_To_O_Files
              (Item  :        String;
               Index :        Positive;
               Quit  : in out Boolean);
            --  Procedure associated with the Find function to add
            --  object files to the string variable.

            procedure Add_To_O_Files
              (Item  :        String;
               Index :        Positive;
               Quit  : in out Boolean)
            is
               --  Unused parameters needed to respect the pattern for Find
               pragma Unreferenced (Index, Quit);
            begin
               Append (O_Files, Item & ASCII.LF);
            end Add_To_O_Files;

            procedure Find_O_Files is new GNAT.Directory_Operations.Iteration
              .Find
              (Action => Add_To_O_Files);
         --  Procedure used to find object files in the application
         --  directory.

         begin
            --  List object files

            Find_O_Files
              (Get_Current_Dir & Get_Name_String (M.Appli_Name),
               ".*\.o");

            Find_O_Files
              (Get_Current_Dir & Get_Name_String (M.Appli_Name),
               ".*\.lo");

            --  Extract functions to analyze with xcov from previously found
            --  object files

            declare
               Nb_O_Files : constant Integer :=
                 Ada.Strings.Unbounded.Count (O_Files, ASCII.LF & "");
               Args2         : GNAT.OS_Lib.Argument_List (1 .. Nb_O_Files + 1);
               Lst, Next_Lst : Integer := 1;
            begin
               Args2 (1) := new String'("disp-routines");

               --  Extract each object file from the String and add it in
               --  the argument list.

               for I in 1 .. Nb_O_Files loop
                  --  Search the end of the next object file (assuming they
                  --  are separated with spaces)

                  Next_Lst :=
                    Lst +
                    Index
                      (Tail (O_Files, To_String (O_Files)'Length - Lst),
                       ASCII.LF & "");

                  --  Add the object file with its full path to the argument
                  --  list (without the space).

                  Args2 (I + 1) :=
                    new String'(Slice (O_Files, Lst, Next_Lst - 1));
                  Lst := Next_Lst + 1;
               end loop;

               --  Remove object files in the traces

               Free (All_Traces);
               All_Traces := new Trace (Integer (First) .. Integer (Last));

               --  Finally, get functions to analyze with xcov

               GNAT.Expect.Non_Blocking_Spawn
                 (Descriptor  => Processes (Integer (J)),
                  Command     => Xcov_Path.all,
                  Args        => Args2,
                  Buffer_Size => 128000,
                  Err_To_Out  => True);
               Add_Filter
                 (Processes (Integer (J)),
                  No_Filter_Procedure'Access,
                  GNAT.Expect.Output,
                  All_Traces (Integer (J))'Address);
               begin
                  Expect (Processes (Integer (J)), Result, Never_Match);
               exception
                  when GNAT.Expect.Process_Died =>
                     null;
               end;
               Close (Processes (Integer (J)));

               --  Write result in <Node_Name>.trace.list file

               Create
                 (File,
                  Out_File,
                  Get_Current_Dir &
                  Get_Name_String (M.Node_Name) &
                  ".trace.list");
               Put (File, To_String (All_Traces (Integer (J))));
               Close (File);
               Write_Line
                 (Get_Name_String (M.Node_Name) & ".trace.list file created");
            end;
         end;
      end loop;

      --  Delay needed to ensure trace files are well written

      delay until Ada.Real_Time.Clock + Span;

      --  Analyze traces with xcov to generate html pages

      for J in First .. Last loop
         M := Process_List.Table (J);
         declare
            Args    : GNAT.OS_Lib.Argument_List (1 .. 5);
            Success : Boolean;
         begin
            Args (1) := new String'("coverage");
            Args (2) := new String'("--level=branch");
            Args (3) := new String'("--annotate=html+asm");
            Args (4) :=
              new String'
                ("--routine-list=" &
                 Get_Current_Dir &
                 Get_Name_String (M.Node_Name) &
                 ".trace.list");
            Args (5) :=
              new String'
                (Get_Current_Dir &
                 Get_Name_String (M.Appli_Name) &
                 Dir_Separator &
                 Get_Name_String (M.Node_Name) &
                 ".trace");
            Write_Line
              ("xcov " &
               Args (1).all &
               " " &
               Args (2).all &
               " " &
               Args (3).all &
               " " &
               Args (4).all &
               " " &
               Args (5).all);
            GNAT.OS_Lib.Spawn (Xcov_Path.all, Args, Success);
            if not Success then
               Return_Value := False;
            end if;
         end;
      end loop;

      Free (All_Traces);
      return Return_Value;
   end Execute_Coverage_Test;
end Ocarina.Backends.Execution_Tests;
