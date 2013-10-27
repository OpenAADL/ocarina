------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . S C R I P T S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013 ESA & ISAE.                       --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Errors;                           use Errors;
with Locations;                        use Locations;
with Namet;                            use Namet;
with Output;                           use Output;
with Types;                            use Types;
with Utils;                            use Utils;

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;                   use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;

with GNAT.OS_Lib;                      use GNAT.OS_Lib;

with Ocarina.Parser;                   use Ocarina.Parser;
with Ocarina.Backends;                 use Ocarina.Backends;

with Ocarina.Files;                    use Ocarina.Files;
with Ocarina.Instances;                use Ocarina.Instances;
with Ocarina.Options;                  use Ocarina.Options;
with Ocarina.Analyzer;                 use Ocarina.Analyzer;
with Ocarina.Transfo.Fusions;          use Ocarina.Transfo.Fusions;
with Ocarina.Transfo.Move;             use Ocarina.Transfo.Move;
with Ocarina.Transfo.Optim;            use Ocarina.Transfo.Optim;
with Ocarina.Utils;                    use Ocarina.Utils;

package body Ocarina.Scripts is

   -------------------
   -- Ocarina_Shell --
   -------------------

   procedure Ocarina_Shell is

      use Ada.Text_IO;

      function "+" (S : String) return String_Access;
      procedure Show_Help;
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
      procedure Print_Status;
      function Next return String;
      function Argument (Index : Natural) return String_Access;
      function Count (Prompt : String := "> ") return Natural;

      Syntax_Error : exception;

      type Command is
        (Help,
         Analyze,
         Instantiate,
         Generate,
         Load,
         Status,
         Fusion,
         Move,
         Optimize,
         Brute_Optimize,
         Version,
         Quit);

      Args    : array (1 .. 16) of String_Access;
      Argc    : Natural;
      Line    : String (1 .. 1024);
      Last    : Natural;
      Scan    : Natural;
      Argv    : String_Access;
      Cmmd    : Command;
      Success : Boolean;

      AADL_Root             : Node_Id := No_Node;
      File_Name             : Name_Id;
      Buffer                : Location;
      Language              : Name_Id;

      ---------
      -- "+" --
      ---------

      function "+" (S : String) return String_Access is
      begin
         return new String'(S);
      end "+";

      ---------------
      -- Show_Help --
      ---------------

      Help_Messages : constant array (Command) of String_Access
        := (Help           => +"print this message",
            Analyze        => +"analyse model",
            Instantiate    => +"instantiate model",
            Generate       => +"generate code",
            Load           => +"load and parse file given as argument",
            Fusion         => +"fusion threads",
            Move           => +"move a thread",
            Optimize       => +"optimize model, using greedy algorithm",
            Brute_Optimize => +"optimize model, using brute force",
            Status         => +"print configuration",
            Version        => +"print Ocarina version information",
            Quit           => +"quit this shell");

      procedure Show_Help is
      begin
         for J in Help_Messages'Range loop
            Write_Line (J'Img & ASCII.HT & Help_Messages (J).all);
         end loop;
      end Show_Help;

      ------------------
      -- Print_Status --
      ------------------

      procedure Print_Status is
      begin
         Write_Line ("AADL version: " & Ocarina.AADL_Version'Img);
         Write_Line ("Library Path: "
                       & Get_Name_String (Default_Library_Path));
      end Print_Status;

      --------------
      -- Argument --
      --------------

      function Argument (Index : Natural) return String_Access is
      begin
         if Index > Argc then
            raise Constraint_Error;
         end if;
         return Args (Index);
      end Argument;

      -----------
      -- Count --
      -----------

      function Count (Prompt : String := "> ") return Natural is
      begin
         if Standard_Input then
            Put (Prompt);
         end if;

         begin
            Get_Line (Current_Input, Line, Last);
         exception
            when Ada.IO_Exceptions.End_Error =>
               --  This means the user hit CTRL-D or the script does
               --  not end with a QUIT command. Not harmful, we just
               --  simulate a QUIT.

               Argc := 1;
               if Args (Argc) /= null then
                  Free (Args (Argc));
               end if;
               Args (Argc) := new String'(Command'Image (Quit));
               return Argc;

            when E : others =>
               Write_Line ("raised "& Exception_Information (E));
               Write_Line (Exception_Message (E));
               raise;
         end;

         Scan := 1;
         Argc := 0;
         loop
            declare
               Arg : constant String := Next;
            begin
               exit when Arg = "";
               Argc := Argc + 1;
               if Args (Argc) /= null then
                  Free (Args (Argc));
               end if;
               Args (Argc) := new String'(Arg);
            end;
         end loop;
         return Argc;
      end Count;

      ----------
      -- Next --
      ----------

      function Next return String is
         use ASCII;

         F, L : Natural;
      begin
         while Scan <= Last
           and then (Line (Scan) = ' ' or else Line (Scan) = HT)
         loop
            Scan := Scan + 1;
         end loop;

         if Scan > Last then
            return "";
         end if;

         if Line (Scan) = '"' then -- "
            Scan := Scan + 1;
            F    := Scan;

            while Scan <= Last loop
               if Line (Scan) = '"' then --  "
                  L    := Scan - 1;
                  Scan := Scan + 1;
                  return Line (F .. L);

               elsif Line (Scan) = NUL then
                  return "";

               end if;

               Scan := Scan + 1;
            end loop;
            return "";

         else
            F := Scan;
            while Scan <= Last
              and then Line (Scan) /= ' '
              and then Line (Scan) /= HT
            loop
               L    := Scan;
               Scan := Scan + 1;
            end loop;
            return Line (F .. L);
         end if;
      end Next;

   begin
      Language := Get_String_Name ("aadl");

      if Standard_Input then
         Write_Line ("Ocarina shell, type help for information");
      end if;

      --  Console main loop: read inputs and process them

      <<Main>>
      loop
         Argc := Count;
         if Argc > 0
           and then Argument (1) /= null
           and then Argument (1).all (Argument (1).all'First) /= '#'
         then
            begin
               Argv := Argument (1);

               begin
                  Cmmd := Command'Value (Argv.all);
               exception when Constraint_Error =>
                  raise Syntax_Error;
               end;

               case Cmmd is
                  when Help =>
                     Show_Help;

                  when Analyze =>
                     Success := Analyze (Language, AADL_Root);
                     if not Success then
                        Write_Line ("Cannot analyze AADL specifications");
                     else
                        Write_Line ("Model analyzed sucessfully");
                     end if;

                  when Instantiate =>
                     if Argc = 2 then
                        Root_System_Name := To_Lower
                          (Get_String_Name (Argument (2).all));
                     end if;
                     AADL_Root := Instantiate_Model (AADL_Root);
                     if Present (AADL_Root) then
                        Write_Line ("Model instantiated sucessfully");
                     end if;

                  when Generate =>
                     if Argc /= 2 then
                        raise Syntax_Error;
                     end if;
                     Set_Current_Backend_Name (Argument (2).all);
                     Write_Line ("Generating code for "
                                 & Argument (2).all);
                     Generate_Code (AADL_Root);

                  when Load =>
                     if Argc /= 2 then
                        raise Syntax_Error;
                     end if;
                     Set_Str_To_Name_Buffer (Argument (2).all);

                     File_Name := Search_File (Name_Find);
                     if File_Name = No_Name then
                        Write_Line ("cannot find file "
                                      & Argument (2).all);
                        goto Main;
                     end if;

                     Buffer := Load_File (File_Name);
                     if File_Name = No_Name then
                        Write_Line ("cannot read file "
                                      & Argument (2).all);
                        goto Main;
                     end if;
                     AADL_Root := Parse (Language, AADL_Root, Buffer);
                     Exit_On_Error
                       (No (AADL_Root),
                        "cannot parse AADL specifications");

                     Write_Line
                       ("File " & Argument (2).all
                        & " loaded and parsed sucessfully");

                  when Brute_Optimize =>
                     declare
                        Instance_Root : Node_Id;
                     begin
                        Instance_Root := Instantiate_Model (AADL_Root);
                        Exit_On_Error (No (Instance_Root),
                                       "Cannot instantiate AADL models");

                        Ocarina.Transfo.Optim.Init (Instance_Root);

                        Exhaustive_Space_Exploration (Instance_Root, Success);
                        Exit_On_Error
                          (not Success,
                           "cannot perform brute optimization on model");

                        Set_Current_Backend_Name ("aadl");
                        Generate_Code (AADL_Root);
                     end;

                  when Optimize =>
                     declare
                        Instance_Root : Node_Id;
                     begin
                        Instance_Root := Instantiate_Model (AADL_Root);
                        Exit_On_Error (No (Instance_Root),
                                       "Cannot instantiate AADL models");

                        Ocarina.Transfo.Optim.Init (Instance_Root);

                        Greedy_Heuristic (Instance_Root, Success);
                        Exit_On_Error
                          (not Success,
                           "cannot perform optimization on model");

                        Set_Current_Backend_Name ("aadl");
                        Generate_Code (AADL_Root);
                     end;

                  when Fusion =>
                     declare
                        Thread_To_Fusion_1 : Name_Id := No_Name;
                        Thread_To_Fusion_2 : Name_Id := No_Name;
                        Owner_Process      : Name_Id := No_Name;
                        --  Transformation-related variables

                        AADL_Instance : Node_Id;
                        New_Thread    : Node_Id;
                        Success       : Boolean;
                     begin
                        AADL_Instance := Instantiate_Model (AADL_Root);
                        Exit_On_Error (No (AADL_Instance),
                                       "Cannot instantiate AADL models");

                        Owner_Process := Get_String_Name (Argument (2).all);
                        Thread_To_Fusion_1 := Get_String_Name
                          (Argument (3).all);
                        Thread_To_Fusion_2 := Get_String_Name
                          (Argument (4).all);

                        Fusion_Threads (AADL_Root,
                                        Owner_Process,
                                        Thread_To_Fusion_1,
                                        Thread_To_Fusion_2,
                                        New_Thread,
                                        Success);

                        Exit_On_Error (not Success,
                                       "Cannot fusion the AADL threads");

                        Set_Current_Backend_Name ("aadl");
                        Generate_Code (AADL_Root);
                     end;

                  when Move =>
                     declare
                        Thread_To_Move : Name_Id := No_Name;
                        Src_Process    : Name_Id := No_Name;
                        Dst_Process    : Name_Id := No_Name;
                        --  Transformation-related variables

                        AADL_Instance : Node_Id;
                     begin
                        AADL_Instance := Instantiate_Model (AADL_Root);
                        Exit_On_Error (No (AADL_Instance),
                                       "Cannot instantiate AADL models");

                        Thread_To_Move := Get_String_Name (Argument (2).all);
                        Src_Process := Get_String_Name (Argument (3).all);
                        Dst_Process := Get_String_Name (Argument (4).all);

                        Move_Thread (Thread_To_Move,
                                     Src_Process,
                                     Dst_Process);

                        Set_Current_Backend_Name ("aadl");
                        Generate_Code (AADL_Root);
                     end;

                  when Status =>
                     Print_Status;

                  when Version =>
                     Version;

                  when Quit =>
                     exit;
               end case;
            exception
               when Syntax_Error =>
                  Write_Line ("syntax error");

               when E : others =>
                  Write_Line ("raised "& Exception_Information (E));
                  Write_Line (Exception_Message (E));
            end;
         end if;
      end loop;
   end Ocarina_Shell;

end Ocarina.Scripts;
