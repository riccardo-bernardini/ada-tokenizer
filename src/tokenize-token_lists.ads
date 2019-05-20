private
package Tokenize.Token_Lists with SPARK_Mode => On  is
   type Token_List (<>) is tagged private;

   subtype List_Length is Integer range 1 .. Integer'Last - 1;


   function Create (N : List_Length) return Token_List
         with
               Pre'Class => N < Positive'Last,
               Post => Create'Result.Length = 0 and Create'Result.Capacity = N;

   function Capacity (Item : Token_List) return Positive;

   function Length (Item : Token_List) return Natural
         with Post => Length'Result <= Item.Capacity;

   procedure Append (List : in out Token_List;
                     What : String)
         with Pre'Class => List.Length  < List.Capacity,
         Post => List.Length = List.Length'Old + 1
         and List.Capacity = List.Capacity'Old;

   function Element (List : Token_List;
                     N    : Positive)
                     return String
         with Pre'Class => List.Length >= N;

private
   type Token_List (Length : List_Length) is tagged
      record
         Tokens     : Token_Array (1 .. Length) := (others => Null_Unbounded_String);
         First_Free : Positive := 1;
      end record
         with Predicate => First_Free <= Integer (Length) + 1
         and Tokens'Length = Length;


   function Create (N : List_Length) return Token_List
   is (Token_List'(Length     => N,
                   Tokens     => (others => Null_Unbounded_String),
                   First_Free => 1));

   function Capacity (Item : Token_List) return Positive
   is (Item.Tokens'Last);

   function Length (Item : Token_List) return Natural
   is (Item.First_Free - 1);

   function Element (List : Token_List;
                     N    : Positive)
                     return String
   is (To_String (List.Tokens (N)));
end Tokenize.Token_Lists;
