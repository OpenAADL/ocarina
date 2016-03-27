pragma Style_Checks (Off);
pragma Warnings (Off);

with Fifo;

package Ocarina.Backends.python.Generator is

   type Port is record
      name      : Name_Id;
      port_type : Name_Id;
      c_type    : Name_Id;
      direction : Name_Id;
   end record;

   package Port_List is new Fifo (Element_Type => Port);

   type Funct is record
      source : Name_Id;
      name   : Name_Id;
      ports  : Port_List.Fifo_Type;
   end record;

   type Call_Funct is record
      has_before      : Boolean := False;
      has_loop        : Boolean := False;
      has_after       : Boolean := False;
      Before_Function : Funct;
      Loop_Function   : Funct;
      After_Function  : Funct;
   end record;

   type Binding is record
      first_name        : Name_Id;
      instance_name     : Name_Id;
      second_name       : Name_Id;
      component_type    : Name_Id;
      priority          : Name_Id;
      period            : Name_Id;
      scheduling_policy : Name_Id;
      dispatch_protocol : Name_Id;
      function_calls    : Call_Funct;
      ports             : Port_List.Fifo_Type;
   end record;

   package Binding_List is new Fifo (Element_Type => Binding);

   type ss_project is record
      project_name   : Name_Id;
      Module_List    : Binding_List.Fifo_Type;
      Device_List    : Binding_List.Fifo_Type;
      Processor_List : Binding_List.Fifo_Type;
      Bus_List       : Binding_List.Fifo_Type;
      Memory_List    : Binding_List.Fifo_Type;
      --Sources_List : Binding_List.Fifo_Type;
   end record;

   Var_Name_Len    : Natural := 0;
   project         : ss_project;
   Print_On_Stdout : Boolean := False;

   procedure Generate (N : Node_Id);
   --  All code generation is performed in the current directory. It
   --  is up to the caller to change the working directory before
   --  calling Generate.

end Ocarina.Backends.python.Generator;
