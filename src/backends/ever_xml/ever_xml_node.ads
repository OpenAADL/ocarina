with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package Ever_XML_Node is

   use Ada.Text_IO;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   type XMLNode is tagged
      record
         Tag   : Unbounded_String;
         Text  : Unbounded_String;
      end record;

   procedure PrintNode(Node : XMLNode);

end Ever_XML_Node;
