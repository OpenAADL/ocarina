pragma Style_Checks (Off);
pragma Warnings (Off);

with Ocarina.Backends.python.Nodes; use Ocarina.Backends.python.Nodes;

with Ada.Text_IO;

package Ocarina.Backends.python_Values is

   package FLT_IO is new Ada.Text_IO.Float_IO (Float);

   type Value_Type (K : Node_Kind := K_Numeric) is record
      case K is
         when K_Numeric =>
            IVal : Unsigned_Long_Long;
            Sign : Short_Short;
            Base : Unsigned_Short_Short;

         when K_String =>
            PCVal : Name_Id;

         when K_Float =>
            FVal : Long_Double;

         when others =>
            null;
      end case;
   end record;

   Bad_Value : constant Value_Type;
   No_Value : constant Value_Id;

   function New_Numeric_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Base  : Unsigned_Short_Short) return Value_Id;

   function New_String_Value (Value : Name_Id) return Value_Id;

   function New_Value (Value : Value_Type) return Value_Id;

   function Value (V : Value_Id) return Value_Type;
   procedure Set_Value (V : Value_Id; X : Value_Type);

   function Image (Value : Value_Id) return String;

   function To_python_Value (V : Value_Id) return Value_Id;
   --  Converts an AADL value to an python one

   function "not" (R : Value_Type) return Value_Type;
   function "-" (R : Value_Type) return Value_Type;
   function "-" (L, R : Value_Type) return Value_Type;
   function "+" (L, R : Value_Type) return Value_Type;
   function "mod" (L, R : Value_Type) return Value_Type;
   function "/" (L, R : Value_Type) return Value_Type;
   function "*" (L, R : Value_Type) return Value_Type;
   function "and" (L, R : Value_Type) return Value_Type;
   function "or" (L, R : Value_Type) return Value_Type;
   function "xor" (L, R : Value_Type) return Value_Type;
   function Shift_Left (L, R : Value_Type) return Value_Type;
   function Shift_Right (L, R : Value_Type) return Value_Type;

   function "<" (L, R : Value_Type) return Boolean;
   --  Assume L and R have the same type.

   procedure Reset;
   --  Reset the value table

   function New_Floating_Point_Value (Value : Long_Double) return Value_Id;

private

   Bad_Value : constant Value_Type := Value_Type'((K => K_Node_Id));
   No_Value  : constant Value_Id   := 0;

end Ocarina.Backends.python_Values;
