with Tokenize.Token_Lists;
package body Tokenize with SPARK_Mode => On is
   use Token_Lists;

   pragma Warnings (Gnatprove, Off, To_String);
   pragma Warnings (Gnatprove, Off, To_Unbounded_String);

   function To_Array (List : Token_List) return Token_Array
     with
       Pre => List.Length >= 0,
       Post => List.Length = To_Array'Result'Length;

   function Uncollated_Split (To_Be_Splitted : String;
                              Separator      : Ada.Strings.Maps.Character_Set)
                              return Token_List
     with
       Pre =>
         To_Be_Splitted'Length > 0
         and To_Be_Splitted'Last <= Token_Lists.List_Length'Last - 2,
         Post =>
           Uncollated_Split'Result.Length <= To_Be_Splitted'Length + 1
           and Uncollated_Split'Result.Length > 0,
         Annotate =>
           (Gnatprove, Terminating);


   function Collated_Split (To_Be_Splitted : String;
                            Separator      : Ada.Strings.Maps.Character_Set;
                            Collation      : Collation_Option)
                            return Token_List
     with
       Pre =>
         To_Be_Splitted'Length > 0 and To_Be_Splitted'Last <= Token_Lists.List_Length'Last - 2,
       Post =>
         Collated_Split'Result.Length <= To_Be_Splitted'Length + 1,
         Annotate =>
           (Gnatprove, Terminating);
   ----------------------
   -- Uncollated_Split --
   ----------------------

   function Uncollated_Split (To_Be_Splitted : String;
                              Separator      : Ada.Strings.Maps.Character_Set)
                              return Token_List is
      use Ada.Strings.Maps;

      pragma Assert (To_Be_Splitted'Length > 0);


      Result : Token_List := Create (To_Be_Splitted'Length + 1);

      First : Integer;
   begin
      pragma Assert (Result.Capacity = To_Be_Splitted'Length + 1);
      pragma Assert (Result.Length = 0);

      First := To_Be_Splitted'First;
      for Pos in To_Be_Splitted'Range loop
         pragma Loop_Invariant (Result.Length >= 0);
         pragma Loop_Invariant (Pos >= First);
         pragma Loop_Invariant (Result.Length <= Pos - To_Be_Splitted'First);
         pragma Loop_Invariant (Result.Capacity = Result.Capacity'Loop_Entry);
         pragma Loop_Invariant (First <= Pos);
         pragma Loop_Invariant (First >= To_Be_Splitted'First);

         if Is_In (To_Be_Splitted (Pos), Separator) then
            if First = Pos then
               Result.Append ("");
            else
               pragma Assert (First < Pos and First >= To_Be_Splitted'First);

               Result.Append (To_Be_Splitted (First .. Pos - 1));
            end if;
            pragma Assert (Result.Length <= Pos - To_Be_Splitted'First + 1);

            First := Pos + 1;
         end if;
      end loop;

      pragma Assert (Result.Length <= To_Be_Splitted'Length);
      pragma Assert (Result.Length >= 0);

      if First = To_Be_Splitted'Last + 1 then
         -- If I am here, to_be_splitted(to_be_splitted'last) is a separator
         -- That is, the string ends with a terminator.

         Result.Append ("");
      else
         Result.Append (To_Be_Splitted (First .. To_Be_Splitted'Last));
      end if;

      pragma Assert (Result.Length >= 1);


      return Result;
   end Uncollated_Split;

   --------------------
   -- Collated_Split --
   --------------------

   function Collated_Split (To_Be_Splitted : String;
                            Separator      : Ada.Strings.Maps.Character_Set;
                            Collation      : Collation_Option)
                            return Token_List
   is
      Tokens : constant Token_List := Uncollated_Split (To_Be_Splitted, Separator);
      pragma Assert (Tokens.Length <= Token_Lists.List_Length'Last - 1);
      Result : Token_List := Create (Tokens.Length);
   begin
      pragma Assert (Result.Capacity = Tokens.Length);
      pragma Assert (Result.Length = 0);

      for K in 1 .. Tokens.Length loop
         pragma Loop_Invariant (Result.Length <= K - 1);
         pragma Loop_Invariant (Result.Capacity = Result.Capacity'Loop_Entry);

         if Tokens.Element (K) /= ""
           or else (if K = 1 then
                      not Collation (Ignore_Head)

                    elsif K = Tokens.Length then
                      not Collation (Ignore_Tail)

                    else
                      not Collation (Collate_Middle))
             --
             -- Ugh!  Quite involuted, isn't it?  This is the best
             -- I have been able to do.  Note that you cannot use
             --
             --    (K = 1 and not Collation (Ignore_Head))
             --    or else (K = tokens.length and not Collation (Ignore_Head))
             --    or else (Collation (Collate_Middle))
             --
             -- since the last alternative would be selected even when
             -- (for example) K=1 and Collation(Ignore_Head) is True. I cannot
             -- even use a "case" since the second condition is not static
             --
         then
            Result.Append (Tokens.Element (K));
         end if;
      end loop;

      return Result;
   end Collated_Split;

   -----------
   -- Split --
   -----------

   function Split (To_Be_Splitted : String;
                   Separator      : Ada.Strings.Maps.Character_Set;
                   Collation      : Collation_Option) return Token_Array
   is
   begin
      if To_Be_Splitted'Length = 0 then
         return Empty_Array;
      end if;

      pragma Assert (To_Be_Splitted'Length > 0);

      return To_Array (Collated_Split (To_Be_Splitted, Separator, Collation));
   end Split;



   -----------
   -- Split --
   -----------

   function Split (To_Be_Splitted    : String;
                   Separator         : Ada.Strings.Maps.Character_Set;
                   Collate_Separator : Boolean)
                   return Token_Array is
   begin
      return Split
        (To_Be_Splitted => To_Be_Splitted,
         Separator      => Separator,
         Collation      => (if Collate_Separator then Full else None));
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

   begin
      if List.Length = 0 then
         return Empty_Array;
      end if;

      pragma Assert (List.Length > 0);

      return Result : Token_Array (1 .. List.Length) := (others => Null_Unbounded_String) do
         pragma Assert (Result'Last >= Result'First);

         for I in Result'Range loop
            Result (I) := To_Unbounded_String (List.Element (I));
         end loop;

      end return;
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

