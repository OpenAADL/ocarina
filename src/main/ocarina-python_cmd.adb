with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;

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
      Register_Python_Scripting (Repo, "ocarina");
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

end Ocarina.Python_Cmd;
