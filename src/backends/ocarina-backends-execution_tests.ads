------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . E X E C U T I O N _ T E S T S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2020 ESA & ISAE.        --
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

with System;

with GNAT.OS_Lib;
with GNAT.Expect;
with GNAT.Regpat;

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Ocarina.Backends.Properties;

package Ocarina.Backends.Execution_Tests is

   use GNAT.OS_Lib;
   use GNAT.Expect;
   use GNAT.Regpat;
   use Ada.Containers;
   use Ocarina.Backends.Properties;

   package String_String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String,
      String,
      Ada.Strings.Hash,
      Equivalent_Keys => "=");
   --  Define a package to create hashed maps between string and string.
   --  this one is needed to associate referencial file names to applications

   procedure Init;
   --  Initialize this package, looking for simulator path
   --  This function should always be called before any operations

   procedure Reset;
   --  Free all ressources allocated by this package

   procedure Launch_Test
     (Fd      : out GNAT.Expect.Process_Descriptor;
      Command :     String;
      Arch    :     Supported_Execution_Platform;
      Timeout :     Natural);
   --  Used to launch the test-program on a specific envoronnement
   --  with specific arguments (invokes Non_Blocking_Spawn)

   function No_Regression
     (Trace    : Ada.Strings.Unbounded.Unbounded_String;
      Ref      : Ada.Strings.Unbounded.Unbounded_String;
      App      : Name_Id;
      Ref_Path : String) return Boolean;
   --  The actual oracle function: it compares the trace with the
   --  given referencial

   function Load_Referencial
     (File_Path : String) return Ada.Strings.Unbounded.Unbounded_String;
   --  Loads the referencial from a file and cleans it from unwanted
   --  characters.

   procedure Write_Referencial
     (File_Path : String;
      Ref       : Ada.Strings.Unbounded.Unbounded_String;
      App       : Name_Id);
   --  Write the referencial Ref to a file

   function Execute_Regression_Test
     (Scenario_Dirname : String;
      Ref_Map          : String_String_Maps.Map;
      Timeout          : Natural) return Boolean;
   --  Executes the entire test

   procedure Clean_String_From_Regexp
     (Str    : in out String_Ptr;
      Regexp :        Pattern_Matcher);
   --  Clean a string from Regexp

   procedure Clean_String_From_All_Regexp (Str : in out String_Ptr);
   --  Clean a string from all Regexps

   procedure Filter_Line (Line : String; Output_Str : in out String_Ptr);
   --  Clean a line from non conventional characters (\r) and from Clean_Regexp
   --  Line         : non filtered raw Line
   --  Clean_Regexp : Regexp used to stip Line from unwanted informations
   --                 such as time measures between brackets
   --  Output_Str   : output result string stripped from regexp

   procedure Filter_Procedure
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address);
   --  Called every time a line is read from the standard output of the tested
   --  program.
   --  Descriptor : the process descriptor of the running tested program
   --  Str        : the line read from the standard output
   --  User_Data  : see g-expect.ads for details

   procedure No_Filter_Procedure
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address);
   --  Filter function used as a bypass to store the output in User_Data
   --  Descriptor : the process descriptor of the running tested program
   --  Str        : the String read from the standard output
   --  User_Data  : see g-expect.ads for details

   function Execute_Coverage_Test (Timeout : Natural) return Boolean;
   --  Execute a coverage test on the generated application
   --  Scenario_Dirname : the name of the scenario directory
   --  Timeout : the duration (in ms) of the test

   function Tsim_Cmd (Cmd : String) return String;
   --  Formats the command string Cmd into a format understood by TSim.
   --  It just adds ASCII.CR to the end of the string.

   type Unbounded_String_Ptr is
     access all Ada.Strings.Unbounded.Unbounded_String;
   --  Define an access type for Unbounded_String

   type Trace is
     array
       (Positive range <>) of aliased Ada.Strings.Unbounded.Unbounded_String;
   --  Type for all traces

   type Trace_Ptr is access all Trace;
   --  Access to previous type

   procedure Free is new Ada.Unchecked_Deallocation (Trace, Trace_Ptr);
   --  Deallocation procedure for Trace_Ptr

   All_Traces : Trace_Ptr;
   --  Points to all traces generated from apps executions

   Header_Has_Ended : Boolean := True;
   --  Tells whether header has ended or not this is
   --  explicitly set to False when simulator is Tsim
   --  when Header has ended, then every lines read from
   --  application stdout are added to its trace

   Command_Name_Path : String_Ptr;
   --  The command name path

   Create_Referencial : Boolean := False;
   --  Decides if we make a regression test or if we write a referencial
   --  file

   Ref_Map : String_String_Maps.Map;
   --  Map table for referencial files

   Timeout : Unsigned_Long_Long;
   --  Timeout used to stop the testing process

   Write_Log : Boolean := True;
   --  Decides if we should write log files for referencials and traces

   Parse_Regexp : Pattern_Matcher (4 * 1024);

   Strip_CR_Regexp : Pattern_Matcher (4 * 1024);

   Header_End_Regexp : Pattern_Matcher (4 * 1024);

   Tsim_Traces_Regexp : Pattern_Matcher (56);

   Empty_Line_Regexp : Pattern_Matcher (20);
   --  Set of Regexp to compare outputs. An additional regexp must be
   --  initialized in Init and called in Clean_String_From_All_Regexp
   --  to be applied.

private
   type Fd_Array is array (Positive range <>) of Process_Descriptor;

   TSim_ERC32_Path : String_Access;
   TSim_LEON_Path  : String_Access;
   Qemu_Path       : String_Access;
   Qemu_Sparc_Path : String_Access;
   Xcov_Path       : String_Access;

end Ocarina.Backends.Execution_Tests;
