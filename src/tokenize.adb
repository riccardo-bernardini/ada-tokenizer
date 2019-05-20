with Ada.Containers.Vectors;
with Tokenize.Token_Lists;
package body Tokenize with SPARK_Mode => On is
   use Token_Lists;

   function To_Array (List : Token_List) return Token_Array;

   function Uncollated_Split (To_Be_Splitted : String;
                              Separator      : Ada.Strings.Maps.Character_Set)
                              return Token_List
         with
               Pre => To_Be_Splitted'Length > 0
               and To_Be_Splitted'Last < Integer'Last - 1,
               Annotate => (Gnatprove, Terminating);
   ----------------------
   -- Uncollated_Split --
   ----------------------

   function Uncollated_Split (To_Be_Splitted : String;
                              Separator      : Ada.Strings.Maps.Character_Set)
                              return Token_List is
      use Ada.Strings.Maps;

      pragma Assert (To_Be_Splitted'Length > 0);


      Result : Token_List := Create (To_Be_Splitted'Length);

      First : Integer;
   begin
      pragma Assert (Result.Capacity = To_Be_Splitted'Length);
      pragma Assert (Result.Length = 0);

      First := To_Be_Splitted'First;
      for Pos in To_Be_Splitted'Range loop
         pragma Assert (Pos >= First);
         pragma Assert (Result.Length <= Pos - To_Be_Splitted'First);

         if Is_In (To_Be_Splitted (Pos), Separator) then
            if First = Pos then
               Result.Append ("");
            else
               pragma Assert (First < Pos);

               Result.Append (To_Be_Splitted (First .. Pos - 1));
            end if;
            pragma Assert (Result.Length <= Pos - To_Be_Splitted'First + 1);

            First := Pos + 1;
         end if;
      end loop;

      if First = To_Be_Splitted'Last + 1 then
         -- If I am here, to_be_splitted(to_be_splitted'last) is a separator
         -- That is, the string ends with a terminator.

         Result.Append ("");
      else
         Result.Append (To_Be_Splitted (First .. To_Be_Splitted'Last));
      end if;


      return Result;
   end Uncollated_Split;

   --------------------
   -- Collated_Split --
   --------------------

   function Collated_Split (To_Be_Splitted : String;
                            Separator      : Ada.Strings.Maps.Character_Set)
                            return Token_List is
      Tokens : constant Token_List := Uncollated_Split (To_Be_Splitted, Separator);
      Result : Token_List := Create (Tokens.Length);
   begin
      for K in 1 .. Tokens.Length loop
         if Tokens.Element (K) /= "" then
            Result.Append (Tokens.Element (K));
         end if;
      end loop;

      return Result;
   end Collated_Split;



   function Split (To_Be_Splitted    : String;
                   Separator         : Ada.Strings.Maps.Character_Set;
                   Collate_Separator : Boolean)
                   return Token_Array is
   begin
      if (Collate_Separator) then
         return To_Array (Collated_Split (To_Be_Splitted, Separator));
      else
         return To_Array (Uncollated_Split (To_Be_Splitted, Separator));
      end if;
   end Split;

   --     -----------
   --     -- Split --
   --     -----------
   --
   --     function Split (To_Be_Splitted    : String;
   --                     Separator         : Character;
   --                     Collate_Separator : Boolean)
   --                     return Token_Array is
   --     begin
   --        return
   --     end Split;


   --------------
   -- To_Array --
   --------------

   function To_Array (List : Token_List)
                      return Token_Array is
      Result : Token_Array (1 .. List.Length);
   begin
      for I in Result'Range loop
         Result (I) := To_Unbounded_String (List.Element (I));
      end loop;

      return Result;
   end To_Array;
end Tokenize;
--        Current := To_Be_Splitted'First;
--
--    Main_Loop:
--        while Current <= To_Be_Splitted'Last loop
--
--       Search_For_Begin:
--           -- Since we are doing a Collated split, we need to skip
--           -- all the separators
--           while Current <= To_Be_Splitted'Last and then
--             To_Be_Splitted(Current) = Separator loop
--              Current := Current+1;
--           end loop Search_For_Begin;
--
--           -- If I am here or Current points after the end of
--           -- the string of To_Be_Splitted(Current) is a non-sep
--           -- character
--
--           exit when (Current > To_Be_Splitted'Last);
--
--           -- If I am here, To_Be_Splitted(Current) is a
--           -- non-separator character
--
--           First := Current;
--
--       Search_For_End:
--           while Current <= To_Be_Splitted'Last and then
--             To_Be_Splitted(Current) /= Separator loop
--              Current := Current+1;
--           end loop Search_For_End;
--
--           String_Vectors.Append (Result,
--                                  To_Be_Splitted(First..Current-1));
--
--           Current := Current+1;
--        end loop Main_Loop;

