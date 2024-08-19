private
package Tokenize.Token_Lists with SPARK_Mode => On  is
   type Token_List (<>) is tagged private;

   subtype List_Length is Integer range 1 .. Integer'Last - 1;


   function Create (N : List_Length) return Token_List
     with
       Post => Create'Result.Length = 0 and Create'Result.Capacity = N;

   function Capacity (Item : Token_List) return Positive;

   function Length (Item : Token_List) return Natural
     with Post => Length'Result <= Item.Capacity;

   procedure Append (List : in out Token_List;
                     What : String)
     with Pre'Class => Length (List)  < List.Capacity,
     Post => List.Length = List.Length'Old + 1
     and List.Capacity = List.Capacity'Old;

   function Element (List : Token_List;
                     N    : Positive)
                     return String
     with Pre'Class => Length (List) >= N;

private
   type Token_List (Length : List_Length) is tagged
      record
         Tokens     : Token_Array (1 .. Length) := (others => Null_Unbounded_String);
         First_Free : Positive := 1;
      end record
     with Predicate => Token_List.First_Free <= Integer (Token_List.Length) + 1
     and Token_List.Tokens'Length = Token_List.Length;


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
