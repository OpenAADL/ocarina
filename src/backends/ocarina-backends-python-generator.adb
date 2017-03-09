------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P Y T H O N . G E N E R A T O R     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

pragma Style_Checks (Off);
pragma Warnings (Off);

with Ocarina.Namet;  use Ocarina.Namet;
with Ocarina.Output; use Ocarina.Output;
with Ada.Text_IO;    use Ada.Text_IO;
with Utils;          use Utils;

with Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ocarina.Backends.Utils;
with Ocarina.Backends.python.Nodes;
with Ocarina.Backends.python.Nutils;
with Ocarina.Backends.SpaceStudio_properties;
with Ocarina.Backends.python_Values;
with Ocarina.Backends.Messages;

package body Ocarina.Backends.python.Generator is

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.python.Nodes;
   use Ocarina.Backends.python.Nutils;
   use Ocarina.Backends.python_Values;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.SpaceStudio_properties;

   procedure Generate_HI_Distributed_Application (N : Node_Id);
   procedure Generate_HI_Node (N : Node_Id);
   procedure Generate_HI_Unit (N : Node_Id);
   procedure Generate_python_File (N : Node_Id);
   procedure Generate_Map_function;
   procedure Generate_deployement_File;
   function Generate_deployement_type_enum return Int;
   procedure Generate_deployement_port_list;
   procedure Generate_deployement_table (Nb : Int);
   --     procedure Generate_Communication_File;
   procedure Generate_systemC_Files;
   procedure Write_Include (name : Name_Id);
   procedure Write_constructor (name : Name_Id);
   procedure Write_thread (Component : Binding);
   procedure Write_Variable (component : Binding);
   procedure Write_Thread_Call
     (input_list  : Port_List.Fifo_Type;
      output_list : Port_List.Fifo_Type;
      subprogram  : Funct);
   procedure Write_header (component : Binding);
   procedure write_import;

   function is_present
     (List_port : Port_List.Fifo_Type;
      Name_Port : Name_Id) return Boolean;
   procedure Write (T : Token_Type);
   procedure Write_Line (T : Token_Type);

   function Get_File_Name (N : Node_Id) return Name_Id;
   --  Generate a file name from the package node given as parameter

   procedure Release_Output (Fd : File_Descriptor);
   --  Releases the output by closing the opened files

   function Set_Output (N : Node_Id) return File_Descriptor;
   --  Adjust the output depending on the command line options and
   --  return a file descriptor in order to be able to close it.

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (N : Node_Id) return Name_Id is
      Suffix_python : constant String := ".py";
      Suffix_HTML   : constant String := ".html";
   begin
      --  The File name corresponding is the lowerd name of N

      Get_Name_String
        (Conventional_Base_Name (Name (Defining_Identifier (N))));

      --  Adding file suffix
      if Is_HTML (N) then
         Add_Str_To_Name_Buffer (Suffix_HTML);
      else
         Add_Str_To_Name_Buffer (Suffix_python);
      end if;

      return Name_Find;
   end Get_File_Name;

   ----------------
   -- Set_Output --
   ----------------

   function Set_Output (N : Node_Id) return File_Descriptor is
   begin
      if not Print_On_Stdout then
         declare
            File_Name        : constant Name_Id := Get_File_Name (N);
            File_Name_String : constant String  := Get_Name_String (File_Name);
            Fd               : File_Descriptor;

         begin
            if Present (python_DTD (N)) then
               --  If a DTD has been specified, copy it as target
               --  file, then move at the end of the file to add
               --  output.

               Ada.Directories.Copy_File
                 (Source_Name => Get_Name_String (Name (python_DTD (N))),
                  Target_Name => File_Name_String);
               Fd := Open_Read_Write (File_Name_String, Text);
               Lseek (Fd, 0, Seek_End);
            else
               --  Else, create a new file, overwrite existing file

               Fd := Create_File (File_Name_String, Text);
            end if;

            if Fd = Invalid_FD then
               raise Program_Error;
            end if;

            --  Setting the output

            Set_Output (Fd);
            return Fd;
         end;
      end if;

      return Invalid_FD;
   end Set_Output;

   --------------------
   -- Release_Output --
   --------------------

   procedure Release_Output (Fd : File_Descriptor) is
   begin
      if not Print_On_Stdout and then Fd /= Invalid_FD then
         Set_Standard_Output;
         Close (Fd);
      end if;
   end Release_Output;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin

      case Kind (N) is

         when K_HI_Distributed_Application =>
            Generate_HI_Distributed_Application (N);

         when K_HI_Unit =>
            Generate_HI_Unit (N);

         when K_python_File =>
            if not Is_Directory ("SpaceStudioProject") then
               Ada.Directories.Create_Directory ("SpaceStudioProject");
            end if;
            Ada.Directories.Set_Directory ("SpaceStudioProject");
            Generate_Map_function;
            Generate_deployement_File;
            --Generate_Communication_File;
            Generate_systemC_Files;
            Generate_python_File (N);

         when K_HI_Node =>
            Generate_HI_Node (N);

         when others =>
            Display_Error ("other element in generator", Fatal => False);
            null;
      end case;
   end Generate;

   -----------------------------------------
   -- Generate_HI_Distributed_Application --
   -----------------------------------------

   procedure Generate_HI_Distributed_Application (N : Node_Id) is
      P                     : Node_Id := First_Node (HI_Nodes (N));
      Application_Directory : Name_Id;
   begin
      --  Create the application directory (a lower case string)

      if Name (N) /= No_Name then
         Get_Name_String (Name (N));
         Application_Directory := To_Lower (Name_Find);

         Create_Directory (Application_Directory);

         --  Process the application nodes

         Enter_Directory (Application_Directory);
      end if;

      while Present (P) loop
         Generate (P);
         P := Next_Node (P);
      end loop;

      if Name (N) /= No_Name then
         Leave_Directory;
      end if;
   end Generate_HI_Distributed_Application;

   ----------------------
   -- Generate_HI_Node --
   ----------------------

   procedure Generate_HI_Node (N : Node_Id) is
      U : Node_Id := First_Node (Units (N));
   begin
      while Present (U) loop
         Generate (U);
         U := Next_Node (U);
      end loop;
   end Generate_HI_Node;

   -----------
   -- Write --
   -----------

   procedure Write (T : Token_Type) is
   begin
      Write_Name (Token_Image (T));
   end Write;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (T : Token_Type) is
   begin
      Write (T);
      Write_Eol;
   end Write_Line;

   ----------------------
   -- Generate_HI_Unit --
   ----------------------

   procedure Generate_HI_Unit (N : Node_Id) is
   begin
      Generate (python_File (N));
   end Generate_HI_Unit;

   -----------------------
   -- write_import --
   -----------------------

   procedure write_import is
   begin
      Write_Line ("import sys");
      Write_Line ("import os");
      Write_Line ("jython_lib=" & '"' & "%s/jython.jar/Lib" & '"' & " % (os.environ['SPACE_CODESIGN_ENV'])");
      Write_Line ("sys.path.insert(1,jython_lib)");
      Write_Line ("import shutil");
      Write_Line ("import tempfile");
   end write_import;

   -----------------------
   -- write_main --
   -----------------------

   procedure write_main is
      Component : Binding;
   begin
      Write_Str ("PROJECT_NAME = '");
      Write_Str (Get_Name_String (project.project_name));
      Write_Str ("'");
      Write_Eol;
      Write_Str ("BUS_NAMES = [");
      if not Binding_List.Is_Empty (project.Bus_List) then
         Binding_List.Pop (project.Bus_List, Component);
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.first_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.component_type));
         Write_Str ("'");
         Write_Str (")");
      end if;
      while not Binding_List.Is_Empty (project.Bus_List) loop
         Binding_List.Pop (project.Bus_List, Component);
         Write_Str (", ");
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.first_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.component_type));
         Write_Str ("'");
         Write_Str (")");
      end loop;
      Write_Str ("]");
      Write_Eol;
      Write_Str ("MEMORY_NAMES = [");
      if not Binding_List.Is_Empty (project.Memory_List) then
         Binding_List.Pop (project.Memory_List, Component);
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.first_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.component_type));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.second_name));
         Write_Str ("'");
         Write_Str (")");
      end if;
      while not Binding_List.Is_Empty (project.Memory_List) loop
         Binding_List.Pop (project.Memory_List, Component);
         Write_Str (", ");
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.first_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.component_type));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.second_name));
         Write_Str ("'");
         Write_Str (")");
      end loop;
      Write_Str ("]");
      Write_Eol;
      Write_Line
        ("# processor (processor_name, processor_type, simulation_model, binding_bus, scheduling_protocol)");
      Write_Str ("PROCESSOR_NAMES = [");
      if not Binding_List.Is_Empty (project.Processor_List) then
         Binding_List.Pop (project.Processor_List, Component);
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.instance_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.component_type));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.Simulation_model));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.second_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.scheduling_policy));
         Write_Str ("'");
         Write_Str (")");
      end if;
      while not Binding_List.Is_Empty (project.Processor_List) loop
         Binding_List.Pop (project.Processor_List, Component);
         Write_Str (", ");
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.instance_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.component_type));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.Simulation_model));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.second_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.scheduling_policy));
         Write_Str ("'");
         Write_Str (")");
      end loop;
      Write_Str ("]");
      Write_Eol;
      Write_Line
        ("# module (module_name, binding_processor, priority, dispatch_protocol, period)");
      Write_Str ("MODULE_NAMES = [");
      if not Binding_List.Is_Empty (project.Module_List) then
         Binding_List.Pop (project.Module_List, Component);
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.first_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.second_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str (Get_Name_String (Component.priority));
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.dispatch_protocol));
         Write_Str ("'");
         Write_Str (",");
         Write_Str (Get_Name_String (Component.period));
         Write_Str (")");
      end if;
      while not Binding_List.Is_Empty (project.Module_List) loop
         Binding_List.Pop (project.Module_List, Component);
         Write_Str (", ");
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.first_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.second_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str (Get_Name_String (Component.priority));
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.dispatch_protocol));
         Write_Str ("'");
         Write_Str (",");
         Write_Str (Get_Name_String (Component.period));
         Write_Str (")");
      end loop;
      Write_Str ("]");
      Write_Eol;
      Write_Str ("DEVICE_NAMES = [");
      if not Binding_List.Is_Empty (project.Device_List) then
         Binding_List.Pop (project.Device_List, Component);
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.first_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.second_name));
         Write_Str ("'");
         Write_Str (")");
      end if;
      while not Binding_List.Is_Empty (project.Device_List) loop
         Binding_List.Pop (project.Device_List, Component);
         Write_Str (", ");
         Write_Str ("(");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.first_name));
         Write_Str ("'");
         Write_Str (",");
         Write_Str ("'");
         Write_Str (Get_Name_String (Component.second_name));
         Write_Str ("'");
         Write_Str (")");
      end loop;
      Write_Str ("]");
      Write_Eol;
      Write_Line ("temp_dir = tempfile.mkdtemp(dir='c:/temp')");
      Write_Line ("try:");
      Write_Indentation (4);
      Write_Line ("project = create_project(PROJECT_NAME, temp_dir)");
      Write_Indentation (4);
      Write_Line
        ("populate_project(project, MODULE_NAMES, DEVICE_NAMES, temp_dir + '\\' + PROJECT_NAME)");
      Write_Indentation (4);
      Write_Line
        ("create_designs(project, PROJECT_NAME, MODULE_NAMES, DEVICE_NAMES, PROCESSOR_NAMES, BUS_NAMES, MEMORY_NAMES)");
      Write_Line ("finally:");
      Write_Indentation (4);
      Write_Line ("print 'fin'");
   end write_main;

   -----------------------
   -- write_create --
   -----------------------

   procedure write_create is
   begin
      Write_Line ("def create_project(project_name, base_dir):");
      Write_Indentation (4);
      Write_Line
        ("return projectEngine.createProject(project_name, base_dir)");
   end write_create;

   -----------------------
   -- write_populate --
   -----------------------

   procedure write_populate is
   begin
      Write_Line
        ("def populate_project(project, module_names, device_names, base_dir):");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("path = base_dir + '\import\src'");
      Write_Indentation;
      Write_Line ("os.mkdir(path)");
      Write_Indentation;
      Write_Line
        ("shutil.copyfile('C:\workspace\SpaceStudioProject\deployement.h', path + '\deployement.h')");
      Write_Indentation;
      Write_Line
        ("shutil.copyfile('C:\workspace\SpaceStudioProject\deadline.h', path + '\deadline.h')");
      Write_Indentation;
      Write_Line
        ("shutil.copyfile('C:\workspace\SpaceStudioProject\deadline.cpp', path + '\deadline.cpp')");
      Write_Indentation;
      Write_Line ("for module_name in module_names:");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("name = module_name[0]");
      Write_Indentation;
      Write_Line ("module = project.createModule(name)");
      Write_Indentation;
      --Write_Line("name = re.sub('[.].*','',module_name[0])");
      --Write_Indentation;
      Write_Line
        ("resource_dir = os.path.join(os.environ['SPACE_CODESIGN_ENV'],  'C:\workspace\SpaceStudioProject')"); --TODO verifier genericite
      Write_Indentation;
      Write_Line
        ("shutil.copyfile(os.path.join(resource_dir, name + '.h'), module.getHeaderPath())");
      Write_Indentation;
      Write_Line
        ("shutil.copyfile(os.path.join(resource_dir, name + '.cpp'), module.getSourcePath())");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("for device_name in device_names:");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("name = device_name[0]");
      Write_Indentation;
      Write_Line ("device = project.createDevice(name, False)");
      Write_Indentation;
      --Write_Line("name = re.sub('[.].*','',device_name[0])");
      --Write_Indentation;
      Write_Line
        ("#resource_dir = os.path.join(os.environ['SPACE_CODESIGN_DEV'], 'workspace/PrototypeExploration/src/main/resources/com/spacecodesign/prototype/exploration')");
      Write_Indentation;
      Write_Line
        ("#shutil.copyfile(os.path.join(resource_dir, name + '.h'), device.getHeaderPath())");
      Write_Indentation;
      Write_Line
        ("#shutil.copyfile(os.path.join(resource_dir, name + '.cpp'), device.getSourcePath())");
      Decrement_Indentation;
      Decrement_Indentation;
   end write_populate;

   -----------------------
   -- write_design --
   -----------------------

   procedure write_design is
   begin
      Write_Line
        ("def create_designs(project, name, module_names, device_names, processor_names, bus_names, memory_names):");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("Design_name = name + '.design'");
      Write_Indentation;
      Write_Line ("bus = {}");
      Write_Indentation;
      Write_Line ("processor = {}");
      Write_Indentation;
      Write_Line ("design = project.createArchitecturalDesign(Design_name)");
      Write_Indentation;
      Write_Line ("for bus_name in bus_names :");
      Increment_Indentation;
      Write_Indentation;
      Write_Line
        ("bus[bus_name[0]] = design.createComponentInstance('Bus',bus_name[1])");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line
        ("Timer = design.createComponentInstance('Timer', 'XilinxTimer')");
      Write_Indentation;
      Write_Line ("Timer.connectTo(bus[bus_names[0][0]])");
      Write_Indentation;
      Write_Line ("for memory_name in memory_names :");
      Increment_Indentation;
      Write_Indentation;
      Write_Line
        ("memory = design.createComponentInstance('Memory', memory_name[1])");
      Write_Indentation;
      Write_Line ("memory.connectTo(bus[memory_name[2]])");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("for processor_name in processor_names :");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("if processor_name[2] is None:");
      Increment_Indentation;
      Write_Indentation;
      Write_Line
      ("processor[processor_name[0]] = design.createProcessorInstance(bus[processor_name[3]], processor_name[1])");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("else :");
      Increment_Indentation;
      Write_Indentation;
      Write_Line
        ("processor[processor_name[0]] = design.createProcessorInstance(bus[processor_name[3]], processor_name[1], processor_name[2])");
      Decrement_Indentation;
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("for module_name in module_names :");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("module = design.createModuleInstance(module_name[0])");
      Write_Indentation;
      Write_Line ("if not module_name[1] == 'Hardware' :");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("module.mapTo(processor[module_name[1]])");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("else :");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("module.mapTo(bus[bus_names[0][0]])");
      Decrement_Indentation;
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("for device_name in device_names:");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("device = design.createDeviceInstance(device_name[0])");
      Write_Indentation;
      Write_Line ("device.connectTo(bus[device_name[1]])");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("design.save()");
      Decrement_Indentation;
   end write_design;

   -----------------------
   -- Generate_python_File --
   -----------------------

   procedure Generate_python_File (N : Node_Id) is
      pragma Assert (Present (N));
      Fd : File_Descriptor;
   begin
      Set_Space_Increment (4);
      Fd := Set_Output (N);
      write_import;
      Write_Eol;
      write_create;
      Write_Eol;
      write_populate;
      Write_Eol;
      write_design;
      Write_Eol;
      --Generate (Root_Node (N));
      write_main;

      Release_Output (Fd);
   end Generate_python_File;

   ----------------------------
   -- Generate_Map_function --
   ----------------------------
   procedure Generate_Map_function is
      List_M    : Binding_List.Fifo_Type;
      Module    : Binding;
      File_Name : Name_Id;
      Fd        : File_Descriptor;
   begin
      File_Name := Get_String_Name ("map_function.txt");
      Fd        := Create_File (Get_Name_String (File_Name), Text);
      Set_Output (Fd);
      while not Binding_List.Is_Empty (project.Module_List) loop
         Binding_List.Pop (project.Module_List, Module);
         Binding_List.Push (List_M, Module);
         Write_Line (Get_Name_String (Module.first_name));

         if Module.function_calls.has_before then
            Write_Indentation (2);
            Write_Str ("before ");
            Write_Str
              (Get_Name_String (Module.function_calls.Before_Function.name));
            Write_Str (" ");
            Write_Line
              (Get_Name_String (Module.function_calls.Before_Function.source));
         end if;

         if Module.function_calls.has_loop then
            Write_Indentation (2);
            Write_Str ("loop ");
            Write_Str
              (Get_Name_String (Module.function_calls.Loop_Function.name));
            Write_Str (" ");
            Write_Line
              (Get_Name_String (Module.function_calls.Loop_Function.source));
         end if;

         if Module.function_calls.has_after then
            Write_Indentation (2);
            Write_Str ("after ");
            Write_Str
              (Get_Name_String (Module.function_calls.After_Function.name));
            Write_Str (" ");
            Write_Line
              (Get_Name_String (Module.function_calls.After_Function.source));
         end if;
      end loop;
      project.Module_List := List_M;
      Release_Output (Fd);
   end Generate_Map_function;

   ----------------------------
   -- Generate_deployement_File --
   ----------------------------
   procedure Generate_deployement_File is
      File_Name  : Name_Id;
      Fd         : File_Descriptor;
      Nb_Mod_Dev : Int;
   begin
      File_Name := Get_String_Name ("deployement.h");
      Fd        := Create_File (Get_Name_String (File_Name), Text);
      Set_Output (Fd);

      Write_Line ("#ifndef DEPLOYEMENT_H");
      Write_Line ("#define DEPLOYEMENT_H");
      Write_Eol;
      Write_Line ("#include """ & "PlatformDefinitions.h""");
      Write_Eol;
      Write_Eol;

      Nb_Mod_Dev := Generate_deployement_type_enum;
      Generate_deployement_port_list;
      Generate_deployement_table (Nb_Mod_Dev);

      Write_Eol;
      Write_Eol;
      Write_Line ("#endif");
      Release_Output (Fd);
   end Generate_deployement_File;

   function Generate_deployement_type_enum return Int is

      Nb_Mod_Dev       : Int := 0;
      Component        : Binding;
      List_Module_temp : Binding_List.Fifo_Type;
      List_Device_temp : Binding_List.Fifo_Type;

   begin
      Write_Line ("enum MODULES_DEVICES {");
      while not Binding_List.Is_Empty (project.Module_List) loop
         Nb_Mod_Dev := Nb_Mod_Dev + 1;
         Binding_List.Pop (project.Module_List, Component);
         Binding_List.Push (List_Module_temp, Component);
         Write_Str (Get_Name_String (Component.first_name));
         if not
           (Binding_List.Is_Empty (project.Module_List)
            and then Binding_List.Is_Empty (project.Device_List))
         then
            Write_Line (",");
         end if;
      end loop;
      while not Binding_List.Is_Empty (project.Device_List) loop
         Nb_Mod_Dev := Nb_Mod_Dev + 1;
         Binding_List.Pop (project.Device_List, Component);
         Binding_List.Push (List_Device_temp, Component);
         Write_Str (Get_Name_String (Component.first_name));
         if not Binding_List.Is_Empty (project.Device_List) then
            Write_Line (",");
         end if;
      end loop;
      Write_Eol;
      project.Module_List := List_Module_temp;
      project.Device_List := List_Device_temp;
      Write_Line ("};");
      Write_Eol;
      return Nb_Mod_Dev;
   end Generate_deployement_type_enum;

   procedure Generate_deployement_port_list is

      Component        : Binding;
      List_Module_temp : Binding_List.Fifo_Type;
      List_Device_temp : Binding_List.Fifo_Type;
      Port_element     : Port;
      element          : Connection_Element;
      Destination_List : Name_List.Fifo_Type;
      tempName         : Name_Id;

   begin
      while not Binding_List.Is_Empty (project.Module_List) loop
         Binding_List.Pop (project.Module_List, Component);
         declare
            List_port_temp : Port_List.Fifo_Type;
         begin
            while not Port_List.Is_Empty (Component.ports) loop
               Port_List.Pop (Component.ports, Port_element);
               Port_List.Push (List_port_temp, Port_element);
               Write_Str ("#define ");
               Write_Str (Get_Name_String (Port_element.name));
               Write_Str (" ");
               if Hash_connection.Contains
                   (Beginning_List,
                    Port_element.name)
               then
                  Destination_List := Visit_children (Port_element.name);
                  Name_List.Pop (Destination_List, tempName);
                  element :=
                    Hash_connection.Element (Beginning_List, tempName);
                  Write_Str
                    (Get_Name_String (To_Upper (element.Destination_Name)));
                  Write_Line ("_ID");
               else
                  tempName := Hash_Name.Element (End_List, Port_element.name);
                  tempName := Visit_parent (tempName);
                  element  :=
                    Hash_connection.Element (Beginning_List, tempName);
                  Write_Str (Get_Name_String (To_Upper (element.Source_Name)));
                  Write_Line ("_ID");
               end if;
         --              Write_Line(Get_Name_String(Port_element.direction));
            end loop;
            Component.ports := List_port_temp;
            Binding_List.Push (List_Module_temp, Component);
         end;
      end loop;
      project.Module_List := List_Module_temp;
      while not Binding_List.Is_Empty (project.Device_List) loop
         Binding_List.Pop (project.Device_List, Component);
         declare
            List_port_temp : Port_List.Fifo_Type;
         begin
            while not Port_List.Is_Empty (Component.ports) loop
               Port_List.Pop (Component.ports, Port_element);
               Port_List.Push (List_port_temp, Port_element);
               Write_Str ("#define ");
               Write_Str (Get_Name_String (Port_element.name));
               Write_Str (" ");
               if Hash_connection.Contains
                   (Beginning_List,
                    Port_element.name)
               then
                  Destination_List := Visit_children (Port_element.name);
                  Name_List.Pop (Destination_List, tempName);
                  element :=
                    Hash_connection.Element (Beginning_List, tempName);
                  Write_Str
                    (Get_Name_String (To_Upper (element.Destination_Name)));
                  Write_Line ("_ID");
               else
                  tempName := Hash_Name.Element (End_List, Port_element.name);
                  tempName := Visit_parent (tempName);
                  element  :=
                    Hash_connection.Element (Beginning_List, tempName);
                  Write_Str (Get_Name_String (To_Upper (element.Source_Name)));
                  Write_Line ("_ID");
               end if;
         --              Write_Line(Get_Name_String(Port_element.direction));
            end loop;
            Component.ports := List_port_temp;
            Binding_List.Push (List_Device_temp, Component);
         end;
      end loop;
      project.Device_List := List_Device_temp;
      Write_Eol;
   end Generate_deployement_port_list;

   procedure Generate_deployement_table (Nb : Int) is

      Component        : Binding;
      List_Module_temp : Binding_List.Fifo_Type;
      List_Device_temp : Binding_List.Fifo_Type;

   begin
      Write_Str ("//bool isModule[");
      Write_Int (Nb);
      Write_Str ("] = {");
      while not Binding_List.Is_Empty (project.Module_List) loop
         Binding_List.Pop (project.Module_List, Component);
         Binding_List.Push (List_Module_temp, Component);
         if Binding_List.Is_Empty (project.Module_List)
           and then Binding_List.Is_Empty (project.Device_List)
         then
            Write_Str ("1");
         else
            Write_Str ("1, ");
         end if;
      end loop;
      while not Binding_List.Is_Empty (project.Device_List) loop
         Binding_List.Pop (project.Device_List, Component);
         Binding_List.Push (List_Device_temp, Component);
         if Binding_List.Is_Empty (project.Device_List) then
            Write_Str ("0");
         else
            Write_Str ("0, ");
         end if;
      end loop;
      project.Module_List := List_Module_temp;
      project.Device_List := List_Device_temp;
      Write_Line ("};");
   end Generate_deployement_table;

   ---------------------------------
   -- Generate_communication_File --
   ---------------------------------
--     procedure Generate_communication_File is
--        File_Name        : Name_Id;
--        Fd               : File_Descriptor;
--        Nb_Mod_Dev       : Int;
--     begin
--        File_Name := Get_String_Name("communication.h");
--        Fd := Create_File (Get_Name_String(File_Name), Text);
--        Set_Output (Fd);
--
--        Write_Line("#ifndef COMMUNICATION_H");
--        Write_Line("#define COMMUNICATION_H");
--        Write_Eol;
--        Write_Line("#include """ & "deployement.h""");
--        Write_Eol;
--        Write_Eol;
--        Write_Line("template<typename Type> void Read(MODULES_DEVICES source, Type *data, int size=1, int offset=0)");
--        Write_Line("{");
--        Increment_Indentation;
--        Increment_Indentation;
--  --        Write_Indentation;
--  --        Write_Line("Type data;");
--        Write_Indentation;
--        Write_Line("if (isModule[source])");
--        Write_Indentation;
--        Write_Line("{");
--        Write_Indentation(4);
--        Write_Line("ModuleRead(source, SPACE_BLOCKING, data, size);");
--        Write_Indentation;
--        Write_Line("}");
--        Write_Indentation;
--        Write_Line("else");
--        Write_Indentation;
--        Write_Line("{");
--        Write_Indentation(4);
--        Write_Line("DeviceRead(source, offset, SPACE_BLOCKING, data, size);");
--        Write_Indentation;
--        Write_Line("}");
--        Write_Line("}");
--        Write_Eol;
--        Write_Line("template<typename Type> void Write(MODULES_DEVICES dest, int offset=0, Type *data, int size=1, int offset=0)");
--        Write_Line("{");
--        Write_Indentation;
--        Write_Line("if (isModule[dest])");
--        Write_Indentation;
--        Write_Line("{");
--        Write_Indentation(4);
--        Write_Line("ModuleWrite(dest, SPACE_BLOCKING, data, size);");
--        Write_Indentation;
--        Write_Line("}");
--        Write_Indentation;
--        Write_Line("else");
--        Write_Indentation;
--        Write_Line("{");
--        Write_Indentation(4);
--        Write_Line("DeviceWrite(dest, offset, SPACE_BLOCKING, data, size);");
--        Write_Indentation;
--        Write_Line("}");
--        Write_Line("}");
--        Decrement_Indentation;
--        Decrement_Indentation;
--        Write_Eol;
--        Write_Eol;
--        Write_Line("#endif");
--
--        Release_Output (Fd);
--     end Generate_communication_File;

   ----------------------------
   -- Generate_systemC_Files --
   ----------------------------
   procedure Generate_systemC_Files is

      File_Name        : Name_Id;
      Fd               : File_Descriptor;
      Component        : Binding;
      Port_element     : Port;
      element          : Connection_Element;
      List             : Binding_List.Fifo_Type;
      Destination_List : Name_List.Fifo_Type;
      tempName         : Name_Id;
      Source           : Name_Id;

   begin
      while not Binding_List.Is_Empty (project.Module_List) loop
         Binding_List.Pop (project.Module_List, Component);
         Binding_List.Push (List, Component);
         File_Name :=
           Ocarina.Backends.python.Nutils.Add_Suffix_To_Name
             (".cpp",
              Component.first_name);
         Fd := Create_File (Get_Name_String (File_Name), Text);

         if Fd = Invalid_FD then
            raise Program_Error;
         end if;
         Set_Output (Fd);

         Write_Line ("/////////////////////////////////////////////////");
         Write_Line ("/////////////////   Include   ///////////////////");
         Write_Line ("/////////////////////////////////////////////////");
         Write_Include (Component.first_name);

         Write_Line ("/////////////////////////////////////////////////");
         Write_Line ("///////////////   Constructor   /////////////////");
         Write_Line ("/////////////////////////////////////////////////");
         Write_constructor (Component.first_name);
         Write_Eol;

         Write_Line ("/////////////////////////////////////////////////");
         Write_Line ("//////////////////   Thread   ///////////////////");
         Write_Line ("/////////////////////////////////////////////////");
         Write_thread (Component);
         Write_Eol;
--
--           Write_Line("/////////////////////////////////////////////////");
--           Write_Line("/////////   Send and Receive methods   //////////");
--           Write_Line("/////////////////////////////////////////////////");
--
--           Write_Eol;
--           Write_Read_Write(Component.first_name);

         Release_Output (Fd);

         File_Name :=
           Ocarina.Backends.python.Nutils.Add_Suffix_To_Name
             (".h",
              Component.first_name);
         Fd := Create_File (Get_Name_String (File_Name), Text);

         if Fd = Invalid_FD then
            raise Program_Error;
         end if;
         Set_Output (Fd);
         Write_header (Component);
         Release_Output (Fd);

      end loop;
      project.Module_List := List;
   end Generate_systemC_Files;

   procedure Write_Include (name : Name_Id) is
   begin
      Write_Line ("#include """ & "PlatformDefinitions.h""");
      Write_Line ("#include """ & "ApplicationDefinitions.h""");
      Write_Line ("#include """ & "SpaceDisplay.h""");
      Write_Line ("#include """ & "SpaceTypes.h""");
      Write_Line ("#include """ & "deployement.h""");
      Write_Line ("#include """ & "deadline.h""");
      Write_Line ("#include <fstream>");
      Write_Str ("#include """);
      Write_Line
        (Get_Name_String
           (Ocarina.Backends.python.Nutils.Add_Suffix_To_Name (".h""", name)));
   end Write_Include;

   procedure Write_constructor (name : Name_Id) is
   begin
      Write_Str (Get_Name_String (name));
      Write_Str ("::");
      Write_Str (Get_Name_String (name));
      Write_Line
        ("(sc_module_name zName, double dClockPeriod, sc_time_unit ClockPeriodUnit, unsigned char ucID, unsigned char ucPriority, bool bVerbose)");
      Write_Line
        (": SpaceBaseModule(zName, dClockPeriod, ClockPeriodUnit, ucID, ucPriority, bVerbose)");
      Write_Line ("{");
      Write_Indentation (4);
      Write_Line ("SC_THREAD(thread);");
      Write_Line ("}");
   end Write_constructor;

   procedure Write_thread (Component : Binding) is
      name         : Name_Id             := Component.first_name;
      period       : Name_Id             := Component.period;
      List_port    : Port_List.Fifo_Type := Component.ports;
      List_temp    : Port_List.Fifo_Type;
      List_in      : Port_List.Fifo_Type;
      List_out     : Port_List.Fifo_Type;
      Port_element : Port;
   begin

      while not Port_List.Is_Empty (List_port) loop
         Port_List.Pop (List_port, Port_element);
         Port_List.Push (List_temp, Port_element);
         if Hash_Subprogram.Contains (Subprogram_Ports, Port_element.name) then
            if Hash_connection.Contains
                (Beginning_List,
                 Port_element.name)
            then
               Port_List.Push (List_out, Port_element);
            else
               Port_List.Push (List_in, Port_element);
            end if;
         end if;
      end loop;
      List_port := List_temp;

      Write_Str ("void ");
      Write_Str (Get_Name_String (name));
      Write_Line ("::thread(void) // Can be either SW or HW");
      Write_Line ("{");
      Increment_Indentation;
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("unsigned int initValue = 0;");
      Write_Indentation;
      Write_Line ("unsigned int timerValue;");
      Increment_Indentation;
      Increment_Indentation;
      Write_Variable (Component);
      Write_Eol;
      if Component.function_calls.has_before then
         Write_Thread_Call
           (List_in,
            List_out,
            Component.function_calls.Before_Function);
      end if;
      Write_Indentation;
      Write_Line ("while(1)");
      Write_Indentation;
      Write_Line ("{");
      --        if not(Get_Name_String(period) = "0") then
      Increment_Indentation;
      Increment_Indentation;
--        Write_Indentation;
--        Write_Line("#if defined(SPACE_SIMULATION_RELEASE) || defined(SPACE_SIMULATION_MONITORING) || defined(SPACE_SIMULATION_DEBUG)");
--        Write_Indentation(4);
--        Write_Line("DeviceWrite(XILINXTIMER1_ID, SPACE_WAIT_FOREVER, &initValue);");
--        Write_Indentation;
--        Write_Line("#endif");
--        Write_Eol;

      Write_Indentation;
      Write_Line ("//Execution between 10 and 40 ms");
      if Component.function_calls.has_loop then
         Write_Thread_Call
           (List_in,
            List_out,
            Component.function_calls.Loop_Function);
      else
         Write_Line ("operation();");
      end if;

      Write_Eol;
      Write_Indentation;
      Write_Line
        ("#if defined(SPACE_SIMULATION_RELEASE) || defined(SPACE_SIMULATION_MONITORING) || defined(SPACE_SIMULATION_DEBUG)");
      Increment_Indentation;
      Increment_Indentation;
      Write_Indentation;
      Write_Line
        ("DeviceRead(XILINXTIMER1_ID, XILINX_TIMER_OFFSET_TCR0, &timerValue);");
      Write_Indentation;
      Write_Str ("if (timerValue - initValue < ");
      Write_Str (Get_Name_String (period));
      Write_Line (" * 1000)");
      Write_Indentation (4);
      Write_Str ("deadline::getInstance().addOc(");
      Write_Str (Get_Name_String (name));
      Write_Line (");");
      Write_Indentation;
      Write_Line ("else");
      Write_Indentation (4);
      Write_Str ("deadline::getInstance().addDl(");
      Write_Str (Get_Name_String (name));
      Write_Line (");");
      Write_Indentation;
      Write_Line
        ("DeviceRead(XILINXTIMER1_ID, XILINX_TIMER_OFFSET_TCR0, &initValue);");
      Decrement_Indentation;
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("#endif");

--        Write_Line("DeviceRead(XILINXTIMER1_ID, offset, &timerValue);");
--        Write_Indentation;
--        Write_Str("if (timerValue < ");
--        Write_Str(Get_Name_String(period));
--        Write_Line(")");
--        Write_Indentation(4);
--        Write_Str("wait(");
--        Write_Str(Get_Name_String(period));
--        Write_Line(" - timerValue); // Wait minimum latency");
--        Write_Indentation;
--        Write_Line("else");
--        Write_Indentation(4);
--        Write_Line("sc_stop( ); //Miss Deadline");
      Decrement_Indentation;
      Decrement_Indentation;
      --        end if;
      Write_Indentation;
      Write_Line ("}");
      Decrement_Indentation;
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("}");
   end Write_thread;

   procedure Write_Variable (component : Binding) is
      Port_element : Port;
      Name_out     : Name_Id;
   begin
      if component.function_calls.has_before then
         declare
            List_temp1 : Port_List.Fifo_Type :=
              component.function_calls.Before_Function.ports;
            List_temp2 : Port_List.Fifo_Type;
         begin
            while not Port_List.Is_Empty (List_temp1) loop
               Port_List.Pop (List_temp1, Port_element);
               Port_List.Push (List_temp2, Port_element);
               Write_Indentation;
               Write_Str (Get_Name_String (Port_element.c_type));
               Write_Str (" ");
               Write_Str (Get_Name_String (Port_element.name));
               Write_Line (";");
               if Port_element.direction = Get_String_Name ("out") then
                  Name_out := Port_element.name;
               end if;
            end loop;
            List_temp1 := List_temp2;
         end;
      end if;

      if component.function_calls.has_loop then
         declare
            List_temp1 : Port_List.Fifo_Type :=
              component.function_calls.Loop_Function.ports;
            List_temp2 : Port_List.Fifo_Type;
         begin
            while not Port_List.Is_Empty (List_temp1) loop
               Port_List.Pop (List_temp1, Port_element);
               Port_List.Push (List_temp2, Port_element);
               if not (Port_element.name = Name_out) then
                  Write_Indentation;
                  Write_Str (Get_Name_String (Port_element.c_type));
                  Write_Str (" ");
                  Write_Str (Get_Name_String (Port_element.name));
                  Write_Line (";");
               end if;
            end loop;
            List_temp1 := List_temp2;
         end;
      end if;

--        while not Port_List.Is_Empty(List_in) loop
--           Port_list.pop(List_in, Port_element);
--           Port_list.push(List_temp, Port_element);
--           List_Port_Sub := Hash_Subprogram.Element(Subprogram_Ports, Port_element.name);
--           declare
--              List_temp_port : Sub_ports;
--           begin
--              while not Name_List.Is_Empty(List_Port_Sub.List) loop
--                 Name_List.pop(List_Port_Sub.List, Name_Port_Sub);
--                 Name_List.push(List_temp_port.List, Name_Port_Sub);
--                 Write_Indentation;
--                 Write_Str(Get_Name_String(Port_element.c_type));
--                 Write_Str(" ");
--                 Write_Str(Get_Name_String(Name_Port_Sub));
--                 Write_Line(";");
--              end loop;
--              List_Port_Sub.List := List_temp_port.List;
--           end;
--        end loop;
--        List_in := List_temp;
--        Write_Eol;
--        if not Port_List.Is_Empty(List_out) then
--           Port_list.pop(List_out, Port_element);
--           pragma assert(Port_List.Is_Empty(List_out), "Just one return by subprogram.");
--           Port_list.push(List_out, Port_element);
--           List_Port_Sub := Hash_Subprogram.Element(Subprogram_Ports, Port_element.name);
--           Name_List.pop(List_Port_Sub.List, Name_Port_Sub);
--           pragma assert(Name_List.Is_Empty(List_Port_Sub.List), "Just one return by subprogram.");
--           Name_List.push(List_Port_Sub.List, Name_Port_Sub);
--           Write_Indentation;
--           Write_Str(Get_Name_String(Port_element.c_type));
--           Write_Str(" ");
--           Write_Str(Get_Name_String(Name_Port_Sub));
--           Write_Line(";");
--        end if;
   end Write_Variable;

   procedure Write_Thread_Call
     (input_list  : Port_List.Fifo_Type;
      output_list : Port_List.Fifo_Type;
      subprogram  : Funct)
   is
      List_in       : Port_List.Fifo_Type := input_list;
      List_out      : Port_List.Fifo_Type := output_list;
      Port_element  : Port;
      Name_port_out : Name_Id             := Get_String_Name (" ");

   begin
      declare
         List_temp     : Port_List.Fifo_Type;
         List_Port_Sub : Sub_ports;
         Name_Port_Sub : Name_Id;
      begin
         while not Port_List.Is_Empty (List_in) loop
            Port_List.Pop (List_in, Port_element);
            Port_List.Push (List_temp, Port_element);
            List_Port_Sub :=
              Hash_Subprogram.Element (Subprogram_Ports, Port_element.name);
            declare
               List_temp_port : Sub_ports;
            begin
               while not Name_List.Is_Empty (List_Port_Sub.List) loop
                  Name_List.Pop (List_Port_Sub.List, Name_Port_Sub);
                  Name_List.Push (List_temp_port.List, Name_Port_Sub);
                  if is_present (subprogram.ports, Name_Port_Sub) then
                     Write_Indentation;
                     --Write_Str("Read<");
                     Write_Str ("ModuleRead(");
                     --Write_Str(Get_Name_String(Port_element.c_type));
                     --Write_Str(">(");
                     Write_Str (Get_Name_String (Port_element.name));
                     Write_Str (", SPACE_WAIT_FOREVER, &");
                     Write_Str (Get_Name_String (Name_Port_Sub));
                     Write_Str (")");
                     Write_Line (";");
                  end if;
               end loop;
               List_Port_Sub.List := List_temp_port.List;
               Hash_Subprogram.Replace
                 (Subprogram_Ports,
                  Port_element.name,
                  List_Port_Sub); -- Peut causer un bug a verifier
            end;
         end loop;
         List_in := List_temp;
      end;

      Write_Eol;
      declare
         portList_temp : Port_List.Fifo_Type := subprogram.ports;
         portList2     : Port_List.Fifo_Type;
      begin
         while not Port_List.Is_Empty (portList_temp) loop
            Port_List.Pop (portList_temp, Port_element);
            Port_List.Push (portList2, Port_element);
            if Port_element.direction = Get_String_Name ("out") then
               Name_port_out := Port_element.name;
            end if;
         end loop;
         portList_temp := portList2;
      end;

      if not (Name_port_out = Get_String_Name (" ")) then
         Write_Indentation;
         Write_Str (Get_Name_String (Name_port_out));
         Write_Str (" = ");
         Write_Str (Get_Name_String (subprogram.name));
         Write_Str ("(");
      else
         Write_Indentation;
         Write_Str (Get_Name_String (subprogram.name));
         Write_Str ("(");
      end if;

      declare
         portList_temp : Port_List.Fifo_Type := subprogram.ports;
         portList2     : Port_List.Fifo_Type;
         First         : Boolean             := True;
      begin
         while not Port_List.Is_Empty (portList_temp) loop
            Port_List.Pop (portList_temp, Port_element);
            Port_List.Push (portList2, Port_element);
            if Port_element.direction =
              Get_String_Name ("in")
            then -- or Port_element.direction = Get_String_Name("inout") then
               if First then
                  First := False;
               else
                  Write_Str (", ");
               end if;
               Write_Str (Get_Name_String (Port_element.name));
            end if;
         end loop;
         portList_temp := portList2;
      end;

      Write_Line (");");
      Write_Eol;

      declare
         List_temp     : Port_List.Fifo_Type;
         List_Port_Sub : Sub_ports;
         Name_Port_Sub : Name_Id;
      begin
         while not Port_List.Is_Empty (List_out) loop
            Port_List.Pop (List_out, Port_element);
            Port_List.Push (List_temp, Port_element);
            List_Port_Sub :=
              Hash_Subprogram.Element (Subprogram_Ports, Port_element.name);
            declare
               List_temp_port : Sub_ports;
            begin
               while not Name_List.Is_Empty (List_Port_Sub.List) loop
                  Name_List.Pop (List_Port_Sub.List, Name_Port_Sub);
                  Name_List.Push (List_temp_port.List, Name_Port_Sub);
                  if Name_Port_Sub = Name_port_out then
                     Write_Indentation;
                     Write_Str ("ModuleWrite(");
                     --Write_Str("Write<");
                     --Write_Str(Get_Name_String(Port_element.c_type));
                     --Write_Str(">(");
                     Write_Str (Get_Name_String (Port_element.name));
                     Write_Str (", SPACE_WAIT_FOREVER, &");
                     Write_Str (Get_Name_String (Name_port_out));
                     Write_Str (")");
                     Write_Line (";");
                  end if;
               end loop;
               List_Port_Sub.List := List_temp_port.List;
               Hash_Subprogram.Replace
                 (Subprogram_Ports,
                  Port_element.name,
                  List_Port_Sub);
            end;
         end loop;
         List_out := List_temp;

      end;
   end Write_Thread_Call;

   procedure Write_header (component : Binding) is
   begin
      Write_Str ("#ifndef ");
      Write_Str (Get_Name_String (To_Upper (component.first_name)));
      Write_Line ("_H");
      Write_Str ("#define ");
      Write_Str (Get_Name_String (To_Upper (component.first_name)));
      Write_Line ("_H");
      Write_Eol;
      Write_Line ("#include """ & "systemc.h""");
      Write_Line ("#include """ & "SpaceBaseModule.h""");
      Write_Eol;
      Write_Str ("class ");
      Write_Str (Get_Name_String (component.first_name));
      Write_Line (" : public SpaceBaseModule");
      Write_Line ("{");
      Increment_Indentation;
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("public:");
      Write_Eol;
      Increment_Indentation;
      Increment_Indentation;
      Write_Indentation;
      Write_Str ("SC_HAS_PROCESS(");
      Write_Str (Get_Name_String (component.first_name));
      Write_Line (");");

      Write_Eol;
      Write_Indentation;
      Write_Str (Get_Name_String (component.first_name));
      Write_Line
        ("(sc_module_name zName, double dClockPeriod, sc_time_unit Unit, unsigned char ucID, unsigned char ucPriority, bool bVerbose);");
      Write_Eol;
      Write_Indentation;
      Write_Line ("void thread(void);");
      Write_Eol;
      Decrement_Indentation;
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("private:");
      Write_Eol;
      Write_Indentation (4);
      Write_Line
        ("//<type de retour> operation(<type_operand1> operand1, ...);");
      Write_Eol;
      Decrement_Indentation;
      Decrement_Indentation;
      Write_Line ("};");
      Write_Eol;
      Write_Line ("#endif");
   end Write_header;

   function is_present
     (List_port : Port_List.Fifo_Type;
      Name_Port : Name_Id) return Boolean
   is
      List         : Port_List.Fifo_Type := List_port;
      List_temp    : Port_List.Fifo_Type;
      Port_element : Port;
      Found        : Boolean             := False;
   begin
      while not Port_List.Is_Empty (List) loop
         Port_List.Pop (List, Port_element);
         Port_List.Push (List_temp, Port_element);
         if Port_element.name = Name_Port then
            Found := True;
         end if;
      end loop;
      List := List_temp;
      return Found;
   end is_present;

end Ocarina.Backends.python.Generator;
