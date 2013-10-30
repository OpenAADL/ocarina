------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                       O C A R I N A . P Y T H O N                        --
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

with Ada.Text_IO;             use Ada.Text_IO;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;

with Ocarina.Utils;

package body Ocarina.Python is

   function Register_Scripts_And_Functions
     return GNATCOLL.Scripts.Scripts_Repository;
   --  Register the various scripting languages and the functions we
   --  export to them

   -------------
   -- Console --
   -------------

   type Text_Console is new GNATCOLL.Scripts.Virtual_Console_Record with record
      Instances : GNATCOLL.Scripts.Instance_List;
   end record;

   overriding procedure Insert_Text
     (Console : access Text_Console; Txt : String);
   overriding procedure Insert_Prompt
     (Console : access Text_Console; Txt : String);
   overriding procedure Insert_Error
     (Console : access Text_Console; Txt : String);
   overriding procedure Set_Data_Primitive
     (Instance : GNATCOLL.Scripts.Class_Instance;
      Console : access Text_Console);
   overriding function Get_Instance
     (Script  : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Console : access Text_Console) return GNATCOLL.Scripts.Class_Instance;

   ------------------------
   -- Set_Data_Primitive --
   ------------------------

   procedure Set_Data_Primitive
     (Instance : Class_Instance; Console  : access Text_Console) is
   begin
      Set (Console.Instances, Get_Script (Instance), Instance);
   end Set_Data_Primitive;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Text_Console) return Class_Instance is
   begin
      return Get (Console.Instances, Script);
   end Get_Instance;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Txt);
   end Insert_Text;

   -------------------
   -- Insert_Prompt --
   -------------------

   procedure Insert_Prompt (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Txt);
   end Insert_Prompt;

   ------------------
   -- Insert_Error --
   ------------------

   procedure Insert_Error (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Standard_Error, Txt);
   end Insert_Error;

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

   ------------------------------------
   -- Register_Scripts_And_Functions --
   ------------------------------------

   function Register_Scripts_And_Functions return Scripts_Repository is
      Repo : Scripts_Repository;
   begin
      --  Register all scripting languages. In practice, you only need to
      --  register those you intend to support

      Repo := new Scripts_Repository_Record;
      Register_Python_Scripting (Repo, "Ocarina");
      Register_Standard_Classes (Repo, "Console");

      --  Register our custom functions

      --  version() function
      Register_Command
        (Repo, "version", 0, 0, Handler => On_Version'Unrestricted_Access);

      --  status() function
      Register_Command
        (Repo, "status", 0, 0, Handler => On_Status'Unrestricted_Access);

      return Repo;
   end Register_Scripts_And_Functions;

   ----------------
   -- Run_Python --
   ----------------

   procedure Run_Python is
      Repo   : Scripts_Repository := Register_Scripts_And_Functions;
      Buffer : String (1 .. 1000);
      Last   : Integer;
      Errors : Boolean;
      Console : aliased Text_Console;
   begin
      Put_Line ("Ocarina interactive Python shell");
      Put_Line ("Please type python commands:");

      Set_Default_Console
        (Lookup_Scripting_Language (Repo, "python"), Console'Unchecked_Access);

      loop
         Get_Line (Buffer, Last);
         Execute_Command
           (Script       => Lookup_Scripting_Language (Repo, "python"),
            Command      => Buffer (1 .. Last),
            Show_Command => False,
            Hide_Output  => False,
            Errors       => Errors);
      end loop;
   exception
      when End_Error =>
         Destroy (Repo);
   end Run_Python;

end Ocarina.Python;
