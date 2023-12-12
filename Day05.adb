with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Advent_IO; use Advent_IO;

with Ada.Text_IO; use Ada.Text_IO;

package body Day05 is

package LLI_Vec is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Long_Long_Integer);

type Almanac_Line is record
    Source_Start: Long_Long_Integer;
    Dest_Start: Long_Long_Integer;
    Length: Long_Long_Integer;
end record;

-- Vector of ranges which form a map
package Almanac_Map is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Almanac_Line);

-- Vector of vectors of ranges (the whole almanac)
package Almanac_Full is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Almanac_Map.Vector,
   "="          => Almanac_Map."=");

type Parsed_Input is record
    Seeds: LLI_Vec.Vector;
    Almanac: Almanac_Full.Vector;
end record;

function Is_Digit(C: Character) return Boolean is
begin
    return Character'Pos(C) - Character'Pos('0') in 0..9;
end Is_Digit;

function Read_Num(S: Unbounded_String; I: in out Natural) return Long_Long_Integer is
    Result: Unbounded_String;
begin
    loop
        Append(Result, Element(S, I));
        I := I + 1;
        exit when I > Length(S);
        exit when not Is_Digit(Element(S, I));
    end loop;
    return Long_Long_Integer'Value(To_String(Result));
end Read_Num;

function Parse_Input(Input: Puzzle_Input) return Parsed_Input is
    Seed_Str: Unbounded_String := Input.Element(0);
    Parsed: Parsed_Input;
    Current_Map: Almanac_Map.Vector;
    Current_Line: Almanac_Line;
    Line: Unbounded_String;
    First_Char: Character;
    J: Natural;
begin
    -- parse seeds
    J := 8; -- consume "seeds: "
    while J < Length(Seed_Str) loop
        Parsed.Seeds.Append(Read_Num(Seed_Str, J));
        J := J + 1; -- consume space
    end loop;
    for I in 3..Input.Last_Index loop
        Line := Input.Element(I);
        -- ignore empty lines
        if Length(Line) /= 0 then
            if Is_Digit(Element(Line, 1)) then
                -- parse line in current map
                J := 1;
                Current_Line.Dest_Start := Read_Num(Line, J);
                J := J + 1; -- consume space
                Current_Line.Source_Start := Read_Num(Line, J);
                J := J + 1; -- consume space
                Current_Line.Length := Read_Num(Line, J);
                Current_Map.Append(Current_Line);
            else
               -- parse new map
               Parsed.Almanac.Append(Current_Map);
               Current_Map.Clear;
            end if;
        end if;
    end loop;
    Parsed.Almanac.Append(Current_Map);
    return Parsed;
end Parse_Input;

function Seed_Location(Seed: Long_Long_Integer; Almanac: Almanac_Full.Vector) return Long_Long_Integer is
    Current_Val: Long_Long_Integer := Seed;
begin
    for Map of Almanac loop
        for Line of Map loop
            if  Current_Val >= Line.Source_Start
            and Current_Val < Line.Source_Start + Line.Length then
                Current_Val := Line.Dest_Start + (Current_Val - Line.Source_Start);
                exit;
            end if;
        end loop;
    end loop;
    return Current_Val;
end;

function Part1(Input: Puzzle_Input) return Integer is
    Parsed: Parsed_Input := Parse_Input(Input);
    Min_Location: Long_Long_Integer := -1;
    Current_Loc: Long_Long_Integer;
begin
    for Seed of Parsed.Seeds loop
        Current_Loc := Seed_Location(Seed, Parsed.Almanac);
        if Current_Loc < Min_Location or Min_Location = -1 then
            Min_Location := Current_Loc;
        end if;
    end loop;
    return Integer(Min_Location);
end Part1;

function Part2(Input: Puzzle_Input) return Integer is
    Parsed: Parsed_Input := Parse_Input(Input);
    Min_Location: Long_Long_Integer := -1;
    Current_Loc: Long_Long_Integer;
    Start: Long_Long_Integer;
    Len: Long_Long_Integer;
    I: Natural := 0;
    function Binary_Min(A, B: Long_Long_Integer) return Long_Long_Integer is
        P, Q, R, S, Y, Z: Long_Long_Integer;
    begin
        P := Seed_Location(A, Parsed.Almanac);
        Q := Seed_Location(B, Parsed.Almanac);
        --  Put("A: "); Put(A'Image); Put(" B: "); Put_Line(B'Image);
        --  Put("P: "); Put(P'Image); Put(" Q: "); Put_Line(Q'Image);
        --  New_Line;
        if P /= Q then
            Y := A+(B-A)/2;
            Z := A+(B-A)/2+1;
            --  Put_Line("-- R(" & A'Image & ", " & Y'Image & ") --");
            R := Binary_Min(A, Y);
            --  Put_Line("-- S(" & Z'Image & ", " & B'Image & ") --");
            S := Binary_Min(Z, B);
            return Long_Long_Integer'Min(R, S);
        else
            return P;
        end if;
    end Binary_Min;
begin
    while I < Parsed.Seeds.Last_Index loop
        Start := Parsed.Seeds.Element(I);
        Len := Parsed.Seeds.Element(I+1);
        for J in Start..Start+Len-1 loop
            Current_Loc := Seed_Location(J, Parsed.Almanac);
            if Current_Loc < Min_Location or Min_Location = -1 then
                Min_Location := Current_Loc;
            end if;
        end loop;
        I := I + 2;
    end loop;
    return Integer(Min_Location);
end Part2;

end Day05;
