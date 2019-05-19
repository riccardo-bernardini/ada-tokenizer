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
      pragma Assert (To_Be_Splitted'Length > 0);

      Result : Token_List := Create (To_Be_Splitted'Length);

      Current, First : Integer;
   begin
      pragma Assert (Result.Capacity = To_Be_Splitted'Length);
      pragma Assert (Result.Length=0);
      Current := To_Be_Splitted'First;

      Main_Loop :
      while Current <= To_Be_Splitted'Last loop
         pragma Assert (Current >= To_Be_Splitted'First);
         pragma Loop_Variant (Increases => Current);
         First := Current;

         Search_For_End :
         while Current <= To_Be_Splitted'Last and then
               not Ada.Strings.Maps.Is_In (To_Be_Splitted (Current), Separator) loop
            Current := Current + 1;

            pragma Loop_Variant (Increases => Current);
         end loop Search_For_End;

         pragma Assert (Current > To_Be_Splitted'First);
         pragma Assert (Current - 1 <= To_Be_Splitted'Last);
         pragma Assert (not Ada.Strings.Maps.Is_In (To_Be_Splitted (Current - 1), Separator));

         Result.Append (To_Unbounded_String (To_Be_Splitted (First .. Current - 1)));

         if (Current = To_Be_Splitted'Last) then
            -- If I am here, to_be_splitted(current) is a separator
            -- (since the condition in the while needs to be false)
            -- This means that the string ends with a terminator.  Since
            -- in the following I do Current := current +1,  the external
            -- loop will exit and I would miss the last empty string
            Result.Append (Null_Unbounded_String);
         end if;

         Current := Current + 1;
      end loop Main_Loop;

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
         if Tokens.Element (K) /= Null_Unbounded_String then
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
         Result (I) := List.Element (I);
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

