with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Advent_IO; use Advent_IO;

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

function Part1(Input: Puzzle_Input) return String is
    Parsed: Parsed_Input := Parse_Input(Input);
    Min_Location: Long_Long_Integer := -1;
    Current_Loc: Long_Long_Integer;
    function Seed_Location(Seed: Long_Long_Integer) return Long_Long_Integer is
        Current_Val: Long_Long_Integer := Seed;
    begin
        for Map of Parsed.Almanac loop
            for Line of Map loop
                if  Current_Val >= Line.Source_Start
                and Current_Val < Line.Source_Start + Line.Length then
                    Current_Val := Line.Dest_Start + (Current_Val - Line.Source_Start);
                    exit;
                end if;
            end loop;
        end loop;
        return Current_Val;
    end Seed_Location;
begin
    for Seed of Parsed.Seeds loop
        Current_Loc := Seed_Location(Seed);
        if Current_Loc < Min_Location or Min_Location = -1 then
            Min_Location := Current_Loc;
        end if;
    end loop;
    return Min_Location'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
    Parsed: Parsed_Input := Parse_Input(Input);
    Min_Location: Long_Long_Integer := -1;
    Start: Long_Long_Integer;
    Len: Long_Long_Integer;
    type Range_Rec is record
        Start:  Long_Long_Integer;
        Stop:   Long_Long_Integer;
        Length: Long_Long_Integer;
    end record;
    -- vector of ranges
    package Range_Vec is new Ada.Containers.Vectors
        (Index_Type   => Natural,
        Element_Type => Range_Rec);
    Unmodified_Ranges: Range_Vec.Vector;
    Modified_Ranges: Range_Vec.Vector;
    Temp_Unmodified_Ranges: Range_Vec.Vector;
    I: Natural := 0;
    type Intersect_Result is record
        Intersect: Range_Vec.Vector;
        Outersect: Range_Vec.Vector;
    end record;
    Result: Intersect_Result;
    -- intersect the seed range with a source/dest mapping, returning the
    -- intersection and the "outersection" (not the intersection)
    function Intersect(Seed, Source, Dest: Range_Rec) return Intersect_Result is
        Result: Intersect_Result;
        Result_Start: Long_Long_Integer := Long_Long_Integer'Max(Seed.Start, Source.Start);
        Result_End: Long_Long_Integer := Long_Long_Integer'Min(Seed.Stop, Source.Stop);
        Result_Len: Long_Long_Integer := Result_End - Result_Start + 1;
        Pre_Len: Long_Long_Integer := Result_Start - Seed.Start;
        Post_Len: Long_Long_Integer := Seed.Stop - Result_End;
        Dest_Diff: Long_Long_Integer := Dest.Start - Source.Start;
        Rng: Range_Rec;
    begin
        if Result_Len <= 0 then
            -- no intersection, return self
            Result.Outersect.Append(Seed);
            return Result;
        end if;
        if Pre_Len > 0 then
            Rng := (Start  => Seed.Start,
                    Stop   => Result_Start-1,
                    Length => Pre_Len);
            Result.Outersect.Append(Rng);
        end if;
        Rng := (Start  => Dest_Diff + Result_Start,
                Stop   => Dest_Diff + Result_End, 
                Length => Result_Len);
        Result.Intersect.Append(Rng);
        if Post_Len > 0 then
            Rng := (Start  => Result_End+1,
                    Stop   => Seed.Stop,
                    Length => Post_Len);
            Result.Outersect.Append(Rng);
        end if;
        return Result;
    end Intersect;
    -- convert start and length to range record
    function Rangeify(Start, Len: Long_Long_Integer) return Range_Rec is
        Result: Range_Rec;
    begin
        Result.Start := Start;
        Result.Stop := Start + Len - 1;
        Result.Length := Len;
        return Result;
    end Rangeify;
begin
    -- generate initial ranges
    while I < Parsed.Seeds.Last_Index loop
        Start := Parsed.Seeds.Element(I);
        Len := Parsed.Seeds.Element(I+1);
        Unmodified_Ranges.Append(Rangeify(Start, Len));
        I := I + 2;
    end loop;
    -- for each map
    for Map of Parsed.Almanac loop
        -- for each line
        for Line of Map loop
            -- for each un-transformed range
            for Rng of Unmodified_Ranges loop
                -- find the intersection of the range with the current mapping
                Result := Intersect(
                    Rng,
                    Rangeify(Line.Source_Start, Line.Length),
                    Rangeify(Line.Dest_Start, Line.Length)
                );
                -- these are the values that could still be transformed
                -- by later lines in this map
                Temp_Unmodified_Ranges.Append_Vector(Result.Outersect);
                -- these are the values that have been modified and cannot
                -- change until the next map
                Modified_Ranges.Append_Vector(Result.Intersect);
            end loop;
            -- copy and clear unmodified ranges
            Unmodified_Ranges := Temp_Unmodified_Ranges;
            Temp_Unmodified_Ranges.Clear;
        end loop;
        -- combine modified and unmodified for next map
        Unmodified_Ranges.Append_Vector(Modified_Ranges);
        Modified_Ranges.Clear;
    end loop;
    -- find the lowest lower bound in the location ranges
    for Rng of Unmodified_Ranges loop
        if Rng.Start < Min_Location or Min_Location = -1 then
            Min_Location := Rng.Start;
        end if;
    end loop;
    return Min_Location'Image;
end Part2;

end Day05;
