with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Ever_XML_Node is

   use Ada.Text_IO;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   procedure PrintNode(Node : XMLNode) is
   begin
      Put_Line ("<" & To_String (Node.Tag) & ">" & To_String (Node.Text) & "</" & To_String (Node.Tag) & ">");

   end PrintNode;

end Ever_XML_Node;
