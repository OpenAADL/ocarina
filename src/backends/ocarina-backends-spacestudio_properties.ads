pragma Style_Checks (Off);
pragma Warnings (Off);

with Utils;                             use Utils;
with Ocarina.Types;                     use Ocarina.Types;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps;        use Ada.Containers;
with Ocarina.Backends.python.Generator; use Ocarina.Backends.python.Generator;
with Fifo;

package Ocarina.Backends.SpaceStudio_properties is

   procedure Prop_Component
     (E            : Node_Id;
      Map          : Hash_Property.Map;
      Concat_Name  : Name_Id;
      Proc_Binding : Name_Id;
      Ports        : Port_List.Fifo_Type);
   procedure Binding;

   type Module_Element is record
      Name     : Name_Id;
      Priority : Name_Id;
   end record;

   package Module_List is new Fifo (Element_Type => Module_Element);

   type Processor_Binding is record
      Name           : Name_Id;
      InstanceName   : Name_Id;
      Component_type : Name_Id;
      List           : Module_List.Fifo_Type;
   end record;

   type td_connection is record
      beginning_port : Name_Id;
      ending_port    : Name_Id;
   end record;

   package Name_List is new Fifo (Element_Type => Name_Id);

   type Sub_ports is record
      List : Name_List.Fifo_Type;
   end record;

   type Connection_Element is record
      Source_Name      : Name_Id;
      Source_Type      : Name_Id;
      Destination_Name : Name_Id;
      Destination_Type : Name_Id;
      ending_port      : Name_Id;
      Parent           : Name_Id;
      Children         : Name_List.Fifo_Type;
   end record;

   procedure Connect_port
     (connection       : td_connection;
      Source_Name      : Name_Id;
      Source_Type      : Name_Id;
      Destination_Name : Name_Id;
      Destination_Type : Name_Id);

   function Visit_Children (Name : Name_Id) return Name_List.Fifo_Type;

   function Visit_Parent (Name : Name_Id) return Name_Id;

   package Hash_Connection is new Ada.Containers.Hashed_Maps
     (Key_Type     => Name_Id,
      Element_Type => Connection_Element,
      Hash            => Hash_String,
      Equivalent_Keys => Equals);

   package Hash_Name is new Ada.Containers.Hashed_Maps
     (Key_Type     => Name_Id,
      Element_Type => Name_Id,
      Hash            => Hash_String,
      Equivalent_Keys => Equals);

   package Hash_Binding is new Ada.Containers.Hashed_Maps
     (Key_Type     => Name_Id,
      Element_Type => Processor_Binding,
      Hash            => Hash_String,
      Equivalent_Keys => Equals);

   package Hash_Instance is new Ada.Containers.Hashed_Maps
     (Key_Type     => Name_Id,
      Element_Type => Integer,
      Hash            => Hash_String,
      Equivalent_Keys => Equals);

   package Hash_Subprogram is new Ada.Containers.Hashed_Maps
     (Key_Type     => Name_Id,
      Element_Type => Sub_ports,
      Hash            => Hash_String,
      Equivalent_Keys => Equals);

   package Hash_Calls is new Ada.Containers.Hashed_Maps
     (Key_Type     => Name_Id,
      Element_Type => Call_Funct,
      Hash            => Hash_String,
      Equivalent_Keys => Equals);

   Map_BP           : Hash_Name.Map;
   Map_BM           : Hash_Name.Map;
   Map_BD           : Hash_Name.Map;
   Beginning_List   : Hash_connection.Map;
   End_List         : Hash_Name.Map;
   Subprogram_Ports : Hash_Subprogram.Map;
   Subprog_Calls    : Hash_Calls.Map;

end Ocarina.Backends.SpaceStudio_properties;
