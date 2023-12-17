with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Advent_IO; use Advent_IO;

package body Day04 is

package Num_Set is new Ada.Containers.Ordered_Sets(Element_Type => Natural);
type Card is record
    Winning: Num_Set.Set;
    Have: Num_Set.Set;
end record;

package Num_Map is new Ada.Containers.Indefinite_Ordered_Maps
    (Key_Type        => Natural,
     Element_Type    => Natural);

function Parse_Card(Card_Str: Unbounded_String) return Card is
    I: Natural := 1;
    Result: Card;
begin
    -- skip card number
    while Element(Card_Str, I) /= ':' loop
        I := I + 1;
    end loop;
    I := I + 2; -- consume ": "
    -- Parse winning
    while Element(Card_Str, I) /= '|' loop
        -- get number
        if Element(Card_Str, I) = ' ' then
            I := I + 1; -- skip space 
            Result.Winning.Insert(Character'Pos(Element(Card_Str, I)) - Character'Pos('0'));
        else
            Result.Winning.Insert(Natural'Value(Slice(Card_Str, I, I+1)));
            I := I + 1; -- consume first digit
        end if;
        I := I + 2; -- consume last digit and space
    end loop;
    I := I + 2; -- Consume "| "
    -- Parse have
    while I < Length(Card_Str) loop
        -- get number
        if Element(Card_Str, I) = ' ' then
            I := I + 1; -- skip space 
            Result.Have.Insert(Character'Pos(Element(Card_Str, I)) - Character'Pos('0'));
        else
            Result.Have.Insert(Natural'Value(Slice(Card_Str, I, I+1)));
            I := I + 1; -- consume first digit
        end if;
        I := I + 2; -- consume last digit and space
    end loop;
    return Result;
end;

function Part1(Input: Puzzle_Input) return String is
    Current: Card;
    Value: Natural;
    Total: Natural := 0;
begin
    for Line of Input loop
        Current := Parse_Card(Line);
        Value := 2 ** (Natural(Current.Winning.Intersection(Current.Have).Length) - 1);
        Total := Total + Value;
    end loop;
    return Total'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
    Win_Map: Num_Map.Map;
    Count_Map: Num_Map.Map;
    Current: Card;
    Winning: Num_Set.Set;
    Current_Win: Natural;
    Current_Count: Natural;
    Total: Natural := 0;
begin
    -- Create win mapping and count mapping
    for I in Input.First_Index..Input.Last_Index loop
        Current := Parse_Card(Input.Element(I));
        Winning := Current.Winning.Intersection(Current.Have);
        Count_Map.Include(I+1, 1);
        Win_Map.Include(I+1, Natural(Winning.Length));
    end loop;
    -- Calculate winnings
    for I in 1..Input.Last_Index+1 loop
        Current_Win := Win_Map(I);
        Current_Count := Count_Map(I);
        -- create Current_Count more of the following Current_Win cards
        for J in 1..Current_Win loop
            Count_Map(I+J) := Count_Map(I+J) + Current_Count;
        end loop;
    end loop;
    -- Sum scratchcard counts
    for I in Count_Map.Iterate loop
        Total := Total + Count_Map(I);
    end loop;
    return Total'Image;
end Part2;

end Day04;
