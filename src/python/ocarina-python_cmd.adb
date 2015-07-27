------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                   O C A R I N A . P Y T H O N _ C M D                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2014-2015 ESA & ISAE.                    --
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

pragma Warnings (Off);
--  Silence all warnings

with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;    use GNATCOLL.Scripts.Python;

with Ocarina.Configuration;      use Ocarina.Configuration;

with GNATCOLL.VFS;               use GNATCOLL.VFS;
with Ocarina.Output;             use Ocarina.Output;

with Errors;
with Ocarina.Types;              use Ocarina.Types;
with Ocarina.Utils;              use Ocarina.Utils;
with Ocarina.Lmp;                use Ocarina.Lmp;
with Ocarina.ME_AADL.AADL_Tree.Nodes.Python;
with Ocarina.ME_AADL.AADL_Instances.Nodes.Python;
with Ocarina.ME_AADL.AADL_Tree.Entities;

with Ocarina.Namet;

package body Ocarina.Python_Cmd is

   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package ATNP renames Ocarina.ME_AADL.AADL_Tree.Nodes.Python;
   package AINP renames Ocarina.ME_AADL.AADL_Instances.Nodes.Python;

   --------------
   -- On_Reset --
   --------------

   procedure On_Reset (Data : in out Callback_Data'Class; Command : String);

   procedure On_Reset
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Data, Command);
   begin
      Ocarina.Utils.Reset;
   end On_Reset;

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
      Ocarina.Configuration.Version;
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
      Result : constant Boolean :=
        Ocarina.Utils.Instantiate (Nth_Arg (Data, 1, ""));

   begin
      Set_Return_Value (Data, Result);
   end On_Instantiate;

   ----------------------
   -- On_Get_AADL_Root --
   ----------------------

   procedure On_Get_AADL_Root
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_AADL_Root
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, Integer'Image (Integer (
         Ocarina.ME_AADL.AADL_Instances.Nodes.Root_System (
         Ocarina.Utils.Get_AADL_Root))));
   end On_Get_AADL_Root;

   ----------------
   -- On_Analyze --
   ----------------

   procedure On_Analyze
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Analyze
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Result : constant Boolean := Ocarina.Utils.Analyze;
   begin
      Set_Return_Value (Data, Result);
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

   ---------------------
   -- On_Get_Packages --
   ---------------------

   procedure On_Get_Packages
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Packages
     (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Packages);
   end On_Get_Packages;

   --------------------------------
   -- On_Get_Import_Declarations --
   --------------------------------

   procedure On_Get_Import_Declarations
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Import_Declarations
     (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Import_Declarations);
   end On_Get_Import_Declarations;

   -------------------------------
   -- On_Get_Alias_Declarations --
   -------------------------------

   procedure On_Get_Alias_Declarations
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Alias_Declarations
     (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Alias_Declarations);
   end On_Get_Alias_Declarations;

   ----------------------------
   -- On_Get_Component_Types --
   ----------------------------

   procedure On_Get_Component_Types
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Component_Types
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Component_Types (Nth_Arg (Data, 1, "")));
   end On_Get_Component_Types;

   --------------------------------------
   -- On_Get_Component_Implementations --
   --------------------------------------

   procedure On_Get_Component_Implementations
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Component_Implementations
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Component_Implementations
         (Nth_Arg (Data, 1, "")));
   end On_Get_Component_Implementations;

   --------------------
   -- On_Get_Annexes --
   --------------------

   procedure On_Get_Annexes
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Annexes
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Annexes);
   end On_Get_Annexes;

   ----------------------
   -- On_Get_Prototype --
   ----------------------

   procedure On_Get_Prototype
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Prototype
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Prototype);
   end On_Get_Prototype;

   ------------------------------
   -- On_Get_Prototype_Binding --
   ------------------------------

   procedure On_Get_Prototype_Binding
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Prototype_Binding
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Prototype_Binding);
   end On_Get_Prototype_Binding;

   -----------------------
   -- On_Get_Flow_Specs --
   -----------------------

   procedure On_Get_Flow_Specs
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Flow_Specs
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Flow_Specs);
   end On_Get_Flow_Specs;

   ---------------------------------
   -- On_Get_Flow_Implementations --
   ---------------------------------

   procedure On_Get_Flow_Implementations
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Flow_Implementations
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Flow_Implementations);
   end On_Get_Flow_Implementations;

   ------------------
   -- On_Get_Modes --
   ------------------

   procedure On_Get_Modes
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Modes
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Modes);
   end On_Get_Modes;

   -----------------------------
   -- On_Get_Mode_Transitions --
   -----------------------------

   procedure On_Get_Mode_Transitions
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Mode_Transitions
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Mode_Transitions);
   end On_Get_Mode_Transitions;

   ---------------------
   -- On_Get_In_Modes --
   ---------------------

   procedure On_Get_In_Modes
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_In_Modes
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_In_Modes);
   end On_Get_In_Modes;

   --------------------------
   -- On_Get_Property_Sets --
   --------------------------

   --  procedure Get_PropertyBinding return Node_List;

   procedure On_Get_Property_Sets
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Property_Sets
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Property_Sets);
   end On_Get_Property_Sets;

   ---------------------------
   -- On_Get_Property_Types --
   ---------------------------

   procedure On_Get_Property_Types
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Property_Types
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Property_Types (
         Get_Node_Id_From_String (Nth_Arg (Data, 1, ""))));
   end On_Get_Property_Types;

   ---------------------------------
   -- On_Get_Property_Definitions --
   ---------------------------------

   procedure On_Get_Property_Definitions
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Property_Definitions
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List (Data, Get_Property_Definitions (
         Get_Node_Id_From_String (Nth_Arg (Data, 1, ""))));
   end On_Get_Property_Definitions;

   ------------------------------
   -- On_Get_PropertyConstants --
   ------------------------------

   procedure On_Get_PropertyConstants
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_PropertyConstants
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      ATNP.return_List
        (Data, Get_Property_Constants
           (Get_Node_Id_From_String (Nth_Arg (Data, 1, ""))));
   end On_Get_PropertyConstants;

   ---------------------------
   -- On_Get_Property_Value --
   ---------------------------

   procedure On_Get_Property_Value
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Property_Value
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
   begin
      Get_Property_Value
        (Data, Nth_Arg (Data, 1, ""),
         Nth_Arg (Data, 2, ""));
   end On_Get_Property_Value;

   procedure On_Get_Property_Value_By_Name
      (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Property_Value_By_Name
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
   begin
      Get_Property_Value_By_Name
        (Data, Nth_Arg (Data, 1, ""),
         Nth_Arg (Data, 2, ""));
   end On_Get_Property_Value_By_Name;

   ----------------------
   -- On_Get_Instances --
   ----------------------

   procedure On_Get_Instances
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Instances
      (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
      List_Node : Node_Id;
   begin
      AINP.return_List (Data, Get_Instances (Nth_Arg (Data, 1, "")));
   end On_Get_Instances;

   --------------------
   -- On_Get_Node_Id --
   --------------------

   procedure On_Get_Node_Id
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Node_Id
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Ocarina.Utils.Get_Node_Id (Data, Nth_Arg (Data, 1, ""));
   end On_Get_Node_Id;

   ---------------------------
   -- On_Get_Component_Name --
   ---------------------------

   procedure On_Get_Component_Name
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Component_Name
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Ocarina.Lmp.Get_Component_Name (Data,
         Node_Id (Integer'Value (Nth_Arg (Data, 1, ""))));
   end On_Get_Component_Name;

   -------------------------------
   -- On_Get_Component_Fullname --
   -------------------------------

   procedure On_Get_Component_Fullname
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Component_Fullname
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Ocarina.Lmp.Get_Component_Fullname (Data,
         Node_Id (Integer'Value (Nth_Arg (Data, 1, ""))));
   end On_Get_Component_Fullname;

   --------------------------
   -- On_Get_Instance_Name --
   --------------------------

   procedure On_Get_Instance_Name
     (Data : in out Callback_Data'Class; Command : String);

   procedure On_Get_Instance_Name
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Ocarina.Lmp.Get_Instance_Name (Data,
         Node_Id (Integer'Value (Nth_Arg (Data, 1, ""))));
   end On_Get_Instance_Name;

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

      --  reset() function
      Register_Command
        (Repo, "reset", 0, 0, Handler => On_Reset'Unrestricted_Access);

      --  version() function
      Register_Command
        (Repo, "version", 0, 0, Handler => On_Version'Unrestricted_Access);

      --  status() function
      Register_Command
        (Repo, "status", 0, 0, Handler => On_Status'Unrestricted_Access);

      --  load() function
      Register_Command
        (Repo, "load", 1, 1,
          Handler => On_Load_AADL_File'Unrestricted_Access);

      --  analyze() function
      Register_Command
        (Repo, "analyze", 0, 0, Handler => On_Analyze'Unrestricted_Access);

      --  LMP accessor

      --  getPackages() function
      Register_Command
        (Repo, "getPackages", 0, 0,
        Handler => On_Get_Packages'Unrestricted_Access);

      --  getImportDeclarations() function
      Register_Command
        (Repo, "getImportDeclarations", 0, 0,
        Handler => On_Get_Import_Declarations'Unrestricted_Access);

      --  getAliasDeclarations() function
      Register_Command
        (Repo, "getAliasDeclarations", 0, 0,
        Handler => On_Get_Alias_Declarations'Unrestricted_Access);

      --  getComponentTypes(kind) function
      Register_Command
        (Repo, "getComponentTypes", 1, 1,
         Handler => On_Get_Component_Types'Unrestricted_Access);

      --  getComponentImplementations(kind) function
      Register_Command
        (Repo, "getComponentImplementations", 1, 1,
         Handler => On_Get_Component_Implementations'Unrestricted_Access);

      --  getAnnexes() function
      Register_Command
        (Repo, "getAnnexes", 0, 0,
        Handler => On_Get_Annexes'Unrestricted_Access);

      --  getPrototypes() function
      Register_Command
        (Repo, "getPrototypes", 0, 0,
         Handler => On_Get_Prototype'Unrestricted_Access);

      --  getPrototypeBindings() function
      Register_Command
        (Repo, "getPrototypeBindings", 0, 0,
         Handler => On_Get_Prototype_Binding'Unrestricted_Access);

      --  getFlowSpecifications() function
      Register_Command
        (Repo, "getFlowSpecifications", 0, 0,
         Handler => On_Get_Flow_Specs'Unrestricted_Access);

      --  getFlowImplementations() function
      Register_Command
        (Repo, "getFlowImplementations", 0, 0,
         Handler => On_Get_Flow_Implementations'Unrestricted_Access);

      --  getModes() function
      Register_Command
        (Repo, "getModes", 0, 0,
         Handler => On_Get_Modes'Unrestricted_Access);

      --  getModeTransitions() function
      Register_Command
        (Repo, "getModeTransitions", 0, 0,
         Handler => On_Get_Mode_Transitions'Unrestricted_Access);

      --  getInModes() function
      Register_Command
        (Repo, "getInModes", 0, 0,
         Handler => On_Get_In_Modes'Unrestricted_Access);

      --  getPropertySets() function
      Register_Command
        (Repo, "getPropertySets", 0, 0,
         Handler => On_Get_Property_Sets'Unrestricted_Access);

      --  getPropertyTypes() function
      Register_Command
        (Repo, "getPropertyTypes", 1, 1,
         Handler => On_Get_Property_Types'Unrestricted_Access);

      --  getPropertyDefinitions() function
      Register_Command
        (Repo, "getPropertyDefinitions", 1, 1,
         Handler => On_Get_Property_Definitions'Unrestricted_Access);

      --  getPropertyValue() function
      Register_Command
        (Repo, "getPropertyValue", 2, 2,
         Handler => On_Get_Property_Value'Unrestricted_Access);

      --  getPropertyValueByName() function
      Register_Command
        (Repo, "getPropertyValueByName", 2, 2,
         Handler => On_Get_Property_Value_By_Name'Unrestricted_Access);

      --  getPropertyConstants() function
      Register_Command
        (Repo, "getPropertyConstants", 1, 1,
         Handler => On_Get_PropertyConstants'Unrestricted_Access);

      --  getInstances() function
      Register_Command
        (Repo, "getInstances", 1, 1,
         Handler => On_Get_Instances'Unrestricted_Access);

      --  instantiate() function
      Register_Command
        (Repo, "instantiate", 1, 1,
         Handler => On_Instantiate'Unrestricted_Access);

      --  getRoot() function
      Register_Command
        (Repo, "getRoot", 0, 0,
         Handler => On_Get_AADL_Root'Unrestricted_Access);

      --  generate() function
      Register_Command
        (Repo, "generate", 1, 1,
         Handler => On_Generate'Unrestricted_Access);

      --  getComponentName() function
      Register_Command
        (Repo, "getComponentName", 1, 1,
         Handler => On_Get_Component_Name'Unrestricted_Access);

      --  getComponentFullname() function
      Register_Command
        (Repo, "getComponentFullname", 1, 1,
         Handler => On_Get_Component_Fullname'Unrestricted_Access);

      --  getInstanceName() function
      Register_Command
        (Repo, "getInstanceName", 1, 1,
         Handler => On_Get_Instance_Name'Unrestricted_Access);

      --  getNodeId() function
      Register_Command
        (Repo, "getNodeId", 1, 1,
         Handler => On_Get_Node_Id'Unrestricted_Access);

      Repo := Ocarina.ME_AADL.AADL_Instances.Nodes.Python.
         Register_Generated_Functions (Repo);

      Repo := Ocarina.ME_AADL.AADL_Tree.Nodes.Python.
         Register_Generated_Functions (Repo);

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
      Errors.Use_Exception_To_Exit;

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
