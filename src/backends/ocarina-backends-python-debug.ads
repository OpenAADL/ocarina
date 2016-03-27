pragma Style_Checks (Off);
pragma Warnings (Off);

with Ocarina.Output; use Ocarina.Output;

with Ocarina.Backends.python.Nodes; use Ocarina.Backends.python.Nodes;

package Ocarina.Backends.python.Debug is

   N_Indents : Natural := 0;

   procedure W_Eol (N : Natural := 1) renames Ocarina.Output.Write_Eol;
   procedure W_Int (N : Int) renames Ocarina.Output.Write_Int;
   procedure W_Line (N : String) renames Ocarina.Output.Write_Line;
   procedure W_Str (N : String) renames Ocarina.Output.Write_Str;
   procedure W_Indents;

   procedure W_Byte (N : Byte);
   procedure W_List_Id (L : List_Id);
   procedure W_Node_Id (N : Node_Id);
   procedure W_Node_Header (N : Node_Id);

   procedure W_Node_Attribute
     (A : String;
      K : String;
      V : String;
      N : Int := 0);

   function Image (N : Node_Kind) return String;
   function Image (N : Name_Id) return String;
   function Image (N : Node_Id) return String;
   function Image (N : List_Id) return String;
   function Image (N : Mode_Id) return String;
   function Image (N : Value_Id) return String;
   function Image (N : Boolean) return String;
   function Image (N : Byte) return String;
   function Image (N : Int) return String;

end Ocarina.Backends.python.Debug;
