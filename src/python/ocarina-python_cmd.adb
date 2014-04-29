------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                   O C A R I N A . P Y T H O N _ C M D                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2014 ESA & ISAE.                       --
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

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;

with Ocarina.Configuration;            use Ocarina.Configuration;
with Ocarina.Utils;

package body Ocarina.Python_Cmd is

   ----------------
   -- On_Version --
   ----------------

   procedure On_Version (Data : in out Callback_Data'Class; Command : String);

   procedure On_Version
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Data, Command);
   begin
      Ocarina.Utils.Version;
   end On_Version;

   ---------------
   -- On_Status --
   ---------------

   procedure On_Status (Data : in out Callback_Data'Class; Command : String);

   procedure On_Status
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Data, Command);
   begin
      Ocarina.Utils.Print_Status;
   end On_Status;

   -----------------------
   -- On_Load_AADL_File --
   -----------------------

   procedure On_Load_AADL_File
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Load_AADL_File
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Ocarina.Utils.Load_AADL_File (Nth_Arg (Data, 1, ""));
   end On_Load_AADL_File;

   --------------------
   -- On_Instantiate --
   ---------------------

   procedure On_Instantiate
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Instantiate
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Ocarina.Utils.Instantiate (Nth_Arg (Data, 1, ""));
   end On_Instantiate;

   ----------------
   -- On_Analyze --
   ----------------

   procedure On_Analyze
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Analyze
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command, Data);
   begin
      Ocarina.Utils.Analyze;
   end On_Analyze;

   -----------------
   -- On_Generate --
   -----------------

   procedure On_Generate
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Generate
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Ocarina.Utils.Generate (Nth_Arg (Data, 1, ""));
   end On_Generate;

   ------------------------------------
   -- Register_Scripts_And_Functions --
   ------------------------------------

   function Register_Scripts_And_Functions return Scripts_Repository is
      Repo : Scripts_Repository;
   begin
      --  Register all scripting languages. In practice, you only need to
      --  register those you intend to support

      Repo := new Scripts_Repository_Record;
      Register_Python_Scripting (Repo, "libocarina_python");
      --  Note: it must match the name of the library generated

      Register_Standard_Classes (Repo, "Console");

      --  Register our custom functions

      --  version() function
      Register_Command
        (Repo, "version", 0, 0, Handler => On_Version'Unrestricted_Access);

      --  status() function
      Register_Command
        (Repo, "status", 0, 0, Handler => On_Status'Unrestricted_Access);

      --  load() function
      Register_Command
        (Repo, "load", 1, 1, Handler => On_Load_AADL_File'Unrestricted_Access);

      --  analyze() function
      Register_Command
        (Repo, "analyze", 0, 0,
         Handler => On_Analyze'Unrestricted_Access);

      --  instantiate() function
      Register_Command
        (Repo, "instantiate", 1, 1,
         Handler => On_Instantiate'Unrestricted_Access);

      --  generate() function
      Register_Command
        (Repo, "generate", 1, 1,
         Handler => On_Generate'Unrestricted_Access);

      return Repo;
   end Register_Scripts_And_Functions;

   --------------------
   -- Initialize_Lib --
   --------------------

   procedure Initialize_Lib is
      procedure Adainit;
      pragma Import (C, Adainit, "ocarina_pythoninit");

   begin
      --  Initialize Ada runtime
      Adainit;

      --  Initialize Ocarina runtime
      Ocarina.Initialize;
      Default_AADL_Version := Get_Default_AADL_Version;
      AADL_Version         := Ocarina.AADL_V2;
      Ocarina.Configuration.Init_Modules;

      --  Initialize Python bindings
      Repo := Register_Scripts_And_Functions;

   end Initialize_Lib;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Repo := Register_Scripts_And_Functions;
   end Initialize;

end Ocarina.Python_Cmd;
