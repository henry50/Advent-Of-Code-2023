with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Advent_IO; use Advent_IO;

package body Day07 is

type Hand_Vals is array(1..5) of Natural;
type Part_Enum is (Part_1, Part_2);
type Hand_Type is (High, One, Two, Three, Full, Four, Five);

type Input_Line is record
    Hand: Hand_Vals;
    Bet: Natural;
end record;

package Natural_Vec is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Natural);

package Input_Vec is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Input_Line);

function Value_Cards(H: String; Part: Part_Enum) return Hand_Vals is
    Values: Hand_Vals;
begin
    for I in 1..5 loop
        Values(I) := (case H(I) is
            when 'A' => 14,
            when 'K' => 13,
            when 'Q' => 12,
            when 'J' => (case Part is
                when Part_1 => 11,
                when Part_2 => 1),
            when 'T' => 10,
            when others => Character'Pos(H(I)) - Character'Pos('0')
        );
    end loop;
    return Values;
end Value_Cards;

function Parse_Input(Input: Puzzle_Input; Part: Part_Enum) return Input_Vec.Vector is
    Result: Input_Vec.Vector;
    Current_Line: Input_Line;
begin
    for Line of Input loop
        Current_Line.Hand := Value_Cards(Slice(Line, 1, 5), Part);
        Current_Line.Bet := Natural'Value(Slice(Line, 7, Length(Line)));
        Result.Append(Current_Line);
    end loop;
    return Result;
end Parse_Input;

-- Identify a hand type
function Identify(Hand: Hand_Vals; Part: Part_Enum) return Hand_Type is
    Card_To_Count: array(1..14) of Natural := (others => 0);
    Count_To_Card: array(0..5) of Natural_Vec.Vector;
    Jokers: Natural;
begin
    -- count cards in hand
    for I in Hand'Range loop
        Card_To_Count(Hand(I)) := Card_To_Count(Hand(I)) + 1;
    end loop;
    -- consider jokers for part 2
    if Part = Part_2 and Card_To_Count(1) > 0 then
        Jokers := Card_To_Count(1);
        for Card in 2..14 loop
            Card_To_Count(Card) := Card_To_Count(Card) + Jokers;
        end loop;
    end if;
    -- map count to cards with that count
    for I in 2..14 loop
        Count_To_Card(Card_To_Count(I)).Append(I);
    end loop;
    if not Count_To_Card(5).Is_Empty then
        return Five;
    elsif not Count_To_Card(4).Is_Empty then
        return Four;
    elsif not Count_To_Card(3).Is_Empty then
        if Natural(Count_To_Card(2).Length) = 1
        -- for part 2, 1 joker, and two pairs will
        -- create two triples, meaning a full house
        or Natural(Count_To_Card(3).Length) = 2 then
            -- full house
            return Full;
        else 
            return Three;
        end if;
    elsif not Count_To_Card(2).Is_Empty then
        case Natural(Count_To_Card(2).Length) is
            when 1 => return One;
            when 2 => return Two;
            -- for part 2, 1 joker will create
            -- 4 cards with count 2, which corresponds
            -- to one pair
            when others => return One;
        end case;
    else
        return High;
    end if;        
end Identify;

-- wrapper around Hand_Sort to add part number
procedure Sort_Input(Parsed: in out Input_Vec.Vector; Part: Part_Enum) is
    -- implements < for Input_Line
    function Hand_Sort(L, R: Input_Line) return Boolean is
        L_Type: Hand_Type := Identify(L.Hand, Part);
        R_Type: Hand_Type := Identify(R.Hand, Part);
    begin
        if L_Type = R_Type then
            -- find high card
            for I in 1..5 loop
                if L.Hand(I) < R.Hand(I) then
                    return True;
                elsif L.Hand(I) > R.Hand(I) then
                    return False;
                end if;
            end loop;
            -- default to false, shouldn't happen
            return False;
        end if;
        return L_Type < R_Type;
    end Hand_Sort;
    package Input_Sort is new Input_Vec.Generic_Sorting ("<" => Hand_Sort);
    use Input_Sort;
begin
    Sort(Parsed);
end Sort_Input;

function Solve(Input: Puzzle_Input; Part: Part_Enum) return Integer is 
    Parsed: Input_Vec.Vector := Parse_Input(Input, Part);
    Total: Natural := 0;
begin
    -- rank hands
    Sort_Input(Parsed, Part);
    -- find total winnings
    for I in Parsed.First_Index..Parsed.Last_Index loop
        Total := Total + ((I + 1) * Parsed(I).Bet);
    end loop;
    return Integer(Total);
end Solve;

function Part1(Input: Puzzle_Input) return String is
begin
    return Solve(Input, Part_1)'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
begin
    return Solve(Input, Part_2)'Image;
end Part2;

end Day07;
